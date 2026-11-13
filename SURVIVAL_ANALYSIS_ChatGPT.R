# ============================================================
# DROUGHT SURVIVAL ANALYSIS
# Journal of Ecology - statistical workflow
# ============================================================

# Loading data and packages ----
# Install packages if necessary
packages <- c(
  "tidyverse",
  "survival",
  "survminer",
  "broom",
  "broom.helpers",
  "ggplot2",
  "patchwork",
  "scales",
  "gt",
  "readr",
  "here"
)

new_packages <- packages[
  !packages %in% installed.packages()[, "Package"]
]

if(length(new_packages) > 0) {
  install.packages(new_packages)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

# Create output directory
dir.create("Drought_survival_results_ChatGPT",
           showWarnings = FALSE)

dir.create("Drought_survival_results_ChatGPT/Figures",
           showWarnings = FALSE)

dir.create("Drought_survival_results_ChatGPT/Tables",
           showWarnings = FALSE)

# Import data
data <- read.table(here("trait_data.csv"), header =T, sep = ",")



# DATA PREPARATION ----

data <- data %>%
  
  mutate(
    
    # Experimental factors
    Species = factor(sp,
                     levels = c("AV", "HO", "PL", "CA", "ME")),
    
    Parental = factor(trat,
                      levels = c("ex", "gra"),
                      labels = c("Ungrazed", "Grazed")),
    
    Clipping = factor(clip,
                      levels = c("no", "yes"),
                      labels = c("Unclipped", "Clipped")),
    
    # Flowering and fruiting duration
    periodo_flor = dias_fin_flor - dias_flor,
    
    periodo_fruto = dias_fin_fruto - dias_fruto
    
  )

# DROUGHT SURVIVAL TIME ----

# Irrigation was stopped 135 days after transplantation.
# Survival time is therefore calculated from day 135.
#
# Plants that died before irrigation cessation are excluded
# because they were not exposed to the drought treatment.
#
# The experiment ended on day 192, when the last plant died.
# Therefore, plants dying during the drought period are events
# (Status = 1), whereas plants still alive on day 192 would be
# right-censored (Status = 0).
#
# We assume that dias_muerte records the actual day of death.
# If a plant was alive at the end of the experiment, its
# dias_muerte should be treated as 192 and Status as 0.
#
# IMPORTANT: Check that the dataset distinguishes plants
# that died on day 192 from plants that were still alive.
# If all plants died by day 192, Status = 1 for all individuals
# included in the drought survival analysis.


data <- data %>%
  
  mutate(
    
    # Survival time since irrigation cessation
    Time = dias_muerte - 135,
    
    # Exclude plants that died before drought started
    Time = ifelse(Time <= 0, NA, Time),
    
    # Mortality status
    # All plants died during the experiment, according to the
    # current information. If some plants survived until day 192,
    # these must instead be coded as Status = 0.
    Status = 1
    
  )


# Dataset for survival analyses

surv_data <- data %>%
  
  filter(
    
    !is.na(Time),
    
    !is.na(Species),
    
    !is.na(Parental),
    
    !is.na(Clipping)
    
  )


# Check survival dataset

summary(surv_data$Time)

table(surv_data$Status)

table(
  surv_data$Species,
  surv_data$Parental,
  surv_data$Clipping
)


# Number of plants excluded because they died before drought

sum(data$dias_muerte < 135, na.rm = TRUE)


# Number of plants included in survival analysis

nrow(surv_data)


# Number of mortality events

sum(surv_data$Status == 1)


# ============================================================
# DESCRIPTIVE SURVIVAL STATISTICS
# ============================================================

# Descriptive statistics for each experimental combination.
# Note that mean and median survival times calculated here
# are observed times, not censoring-adjusted estimates.

survival_summary <- surv_data %>%
  
  group_by(Species, Parental, Clipping) %>%
  
  summarise(
    
    N = n(),
    
    Deaths = sum(Status == 1),
    
    Censored = sum(Status == 0),
    
    Mortality_percent = 100 * mean(Status == 1),
    
    Mean_survival = mean(Time),
    
    SD_survival = sd(Time),
    
    Median_survival = median(Time),
    
    .groups = "drop"
    
  )


write.csv(
  
  survival_summary,
  
  "Drought_survival_results_ChatGPT/Tables/Table1_Survival_summary.csv",
  
  row.names = FALSE
  
)



# KAPLAN-MEIER SURVIVAL ANALYSIS ----

# Create a factor identifying the 20 experimental combinations

surv_data <- surv_data %>%
  
  mutate(
    
    Treatment_group = interaction(
      
      Species,
      Parental,
      Clipping,
      sep = " × "
      
    )
    
  )


# Kaplan-Meier model

km_fit <- survfit(
  
  Surv(Time, Status) ~ Treatment_group,
  
  data = surv_data
  
)


# Global log-rank test comparing the 20 groups

logrank_test <- survdiff(
  
  Surv(Time, Status) ~ Treatment_group,
  
  data = surv_data
  
)


# Extract log-rank statistics

logrank_chisq <- logrank_test$chisq

logrank_df <- length(logrank_test$n) - 1

logrank_p <- pchisq(
  
  logrank_chisq,
  
  df = logrank_df,
  
  lower.tail = FALSE
  
)


logrank_results <- tibble(
  
  Chi_square = logrank_chisq,
  
  df = logrank_df,
  
  P_value = logrank_p
  
)


write.csv(
  
  logrank_results,
  
  "Drought_survival_results_ChatGPT/Tables/Table2_Logrank.csv",
  
  row.names = FALSE
  
)


# View results

logrank_results


# ============================================================
# KAPLAN-MEIER FIGURE
# ============================================================

# A single plot with 20 curves may be difficult to interpret.
# We therefore also generate a figure with five species panels.

km_species_list <- list()

for(s in levels(surv_data$Species)) {
  
  subset_data <- surv_data %>%
    
    filter(Species == s) %>%
    
    mutate(
      
      Treatment = interaction(
        
        Parental,
        
        Clipping,
        
        sep = " × "
        
      )
      
    )
  
  fit <- survfit(
    
    Surv(Time, Status) ~ Treatment,
    
    data = subset_data
    
  )
  
  plot_data <- survminer::surv_summary(
    
    fit,
    
    data = subset_data
    
  )
  
  plot_data$Species <- s
  
  km_species_list[[s]] <- plot_data
  
}


km_plot_data <- bind_rows(km_species_list)


km_facet_plot <- ggplot(
  
  km_plot_data,
  
  aes(
    
    x = time,
    
    y = surv,
    
    colour = strata
    
  )
  
) +
  
  geom_step(
    
    linewidth = 0.9
    
  ) +
  
  facet_wrap(
    
    ~ Species,
    
    ncol = 2
    
  ) +
  
  scale_y_continuous(
    
    limits = c(0, 1),
    
    breaks = seq(0, 1, 0.2)
    
  ) +
  
  scale_x_continuous(
    
    breaks = seq(0, 60, 10)
    
  ) +
  
  labs(
    
    x = "Days since irrigation cessation",
    
    y = "Probability of survival",
    
    colour = "Treatment"
    
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    
    strip.background = element_blank(),
    
    strip.text = element_text(
      
      face = "italic",
      
      colour = "black"
      
    ),
    
    legend.position = "bottom",
    
    panel.spacing = unit(1.2, "lines")
    
  )


ggsave(
  
  "Drought_survival_results_ChatGPT/Figures/Figure1_KM_species.pdf",
  
  km_facet_plot,
  
  width = 9,
  
  height = 8
  
)


ggsave(
  
  "Drought_survival_results_ChatGPT/Figures/Figure1_KM_species.tiff",
  
  km_facet_plot,
  
  width = 9,
  
  height = 8,
  
  dpi = 600,
  
  compression = "lzw"
  
)


# ============================================================
# COX PROPORTIONAL HAZARDS MODEL
# ============================================================

# Full factorial model:
# Species * Parental * Clipping
#
# This model tests the effects of species, parental grazing
# history, within-generation clipping and their interactions.
#
# IMPORTANT:
# The full factorial model may be overparameterised because
# some experimental combinations have very small sample sizes.
# Check coefficient estimates, confidence intervals, convergence
# warnings and the number of mortality events before interpreting.
#
# If all plants died, the Cox model can still be fitted, but
# survival curves will eventually reach zero and there will be
# no right-censoring information.

cox_full <- coxph(
  
  Surv(Time, Status) ~
    
    Species * Parental * Clipping,
  
  data = surv_data,
  
  ties = "efron"
  
)


summary(cox_full)


# Analysis of deviance

cox_anova <- drop1(
  
  cox_full,
  
  test = "Chisq"
  
)


write.csv(
  
  as.data.frame(cox_anova),
  
  "Drought_survival_results_ChatGPT/Tables/Table3_Cox_ANOVA.csv"
  
)


# Proportional hazards assumption

ph_test <- cox.zph(cox_full)

print(ph_test)


pdf(
  
  "Drought_survival_results_ChatGPT/Figures/Figure2_Cox_diagnostics.pdf",
  
  width = 8,
  
  height = 7
  
)

plot(ph_test)

dev.off()


# Extract hazard ratios and confidence intervals

cox_results <- broom::tidy(
  
  cox_full,
  
  exponentiate = TRUE,
  
  conf.int = TRUE
  
) %>%
  
  mutate(
    
    HR = estimate,
    
    CI_low = conf.low,
    
    CI_high = conf.high,
    
    P_value = p.value
    
  ) %>%
  
  select(
    
    term,
    
    HR,
    
    CI_low,
    
    CI_high,
    
    P_value
    
  )


write.csv(
  
  cox_results,
  
  "Drought_survival_results_ChatGPT/Tables/Table4_Cox_model.csv",
  
  row.names = FALSE
  
)



# FUNCTIONAL TRAITS ----

# All 22 functional traits are initially included.
#
# IMPORTANT:
# AvgDiam.mm., RTD, SRL, height and biomass were measured
# after plants had died.
#
# These variables require particular caution because their
# measured values may have been influenced by drought exposure,
# plant deterioration or the process leading to mortality.
#
# Although pre-existing differences in these traits could
# theoretically contribute to drought resistance, measurements
# taken after death cannot establish that they were antecedent
# predictors of survival.
#
# Therefore, associations involving these variables should be
# interpreted as exploratory associations, not evidence that
# these traits caused greater or lower drought resistance.
#
# The same issue applies to any other trait measured after
# drought exposure or only in plants that survived.
#
# No trait is removed at this exploratory stage.

traits <- c(
  
  "flor",
  "fruto",
  "seedtotal",
  "seedfruto",
  "massseed",
  "hojas",
  "altura",
  "biomass",
  "dias_flor",
  "dias_fruto",
  "periodo_flor",
  "periodo_fruto",
  "SLA",
  "LDMC",
  "area.m",
  "Photosyn",
  "Conductance",
  "PhiPS2",
  "fenoles",
  "AvgDiam.mm.",
  "RTD",
  "SRL"
  
)


# Check that all traits exist in the dataset

traits %in% names(surv_data)


# INDIVIDUAL COX MODELS FOR FUNCTIONAL TRAITS ----

# Each trait is analysed separately.
#
# The models include the full experimental factorial structure
# to account for differences among species and treatments.
#
# Traits are standardised (mean = 0, SD = 1).
# Hazard ratios therefore represent the change in mortality
# hazard associated with a one-SD increase in each trait.
#
# Models use complete observations for each individual trait.
#
# Given the small sample sizes in some treatment combinations,
# convergence and model stability must be checked carefully.

trait_cox_results <- list()

trait_cox_models <- list()


for(trait in traits) {
  
  trait_data <- surv_data %>%
    
    filter(
      
      !is.na(.data[[trait]]),
      
      is.finite(.data[[trait]])
      
    )
  
  
  # Standardise trait
  
  trait_data$trait_z <- as.numeric(
    
    scale(trait_data[[trait]])
    
  )
  
  
  # Skip traits with no variation
  
  if(
    
    nrow(trait_data) < 10 ||
    
    sd(trait_data$trait_z, na.rm = TRUE) == 0
    
  ) {
    
    next
    
  }
  
  
  # Fit model
  
  formula_trait <- as.formula(
    
    paste(
      
      "Surv(Time, Status) ~",
      
      "Species * Parental * Clipping + trait_z"
      
    )
    
  )
  
  
  model <- tryCatch(
    
    coxph(
      
      formula_trait,
      
      data = trait_data,
      
      ties = "efron"
      
    ),
    
    error = function(e) NULL
    
  )
  
  
  if(is.null(model)) next
  
  
  trait_cox_models[[trait]] <- model
  
  
  # Extract trait effect
  
  result <- broom::tidy(
    
    model,
    
    exponentiate = TRUE,
    
    conf.int = TRUE
    
  ) %>%
    
    filter(term == "trait_z") %>%
    
    mutate(
      
      Trait = trait,
      
      N = nrow(trait_data),
      
      Events = sum(trait_data$Status == 1)
      
    ) %>%
    
    select(
      
      Trait,
      
      N,
      
      Events,
      
      HR = estimate,
      
      CI_low = conf.low,
      
      CI_high = conf.high,
      
      P_value = p.value
      
    )
  
  
  trait_cox_results[[trait]] <- result
  
}


# Combine results

trait_cox_table <- bind_rows(trait_cox_results)


# Multiple testing correction

trait_cox_table <- trait_cox_table %>%
  
  mutate(
    
    P_adjusted = p.adjust(
      
      P_value,
      
      method = "BH"
      
    )
    
  ) %>%
  
  arrange(P_adjusted)


write.csv(
  
  trait_cox_table,
  
  "Drought_survival_results_ChatGPT/Tables/Table5_Trait_Cox_models.csv",
  
  row.names = FALSE
  
)



# PROPORTIONAL HAZARDS DIAGNOSTICS FOR TRAIT MODELS ----

trait_ph_results <- list()


for(trait in names(trait_cox_models)) {
  
  model <- trait_cox_models[[trait]]
  
  
  ph <- tryCatch(
    
    cox.zph(model),
    
    error = function(e) NULL
    
  )
  
  
  if(is.null(ph)) next
  
  
  ph_table <- as.data.frame(ph$table)
  
  ph_table$Term <- rownames(ph_table)
  
  ph_table$Trait <- trait
  
  
  trait_ph_results[[trait]] <- ph_table
  
}


trait_ph_table <- bind_rows(trait_ph_results)


write.csv(
  
  trait_ph_table,
  
  "Drought_survival_results_ChatGPT/Tables/Table6_Trait_PH_diagnostics.csv",
  
  row.names = FALSE
  
)


# ============================================================
# FOREST PLOT: FUNCTIONAL TRAITS
# ============================================================

# Hazard ratios are presented on a logarithmic scale.
# The vertical reference line (HR = 1) indicates no association.
# Colours distinguish associations with adjusted P < 0.05.

forest_plot <- trait_cox_table %>%
  
  mutate(
    
    Trait = factor(
      
      Trait,
      
      levels = rev(Trait)
      
    ),
    
    Significance = ifelse(
      
      P_adjusted < 0.05,
      
      "Significant",
      
      "Not significant"
      
    )
    
  ) %>%
  
  ggplot(
    
    aes(
      
      x = HR,
      
      y = Trait,
      
      colour = Significance
      
    )
    
  ) +
  
  geom_vline(
    
    xintercept = 1,
    
    linetype = "dashed",
    
    colour = "grey40"
    
  ) +
  
  geom_errorbar(
    
    aes(
      
      xmin = CI_low,
      
      xmax = CI_high
      
    ),
    
    width = 0.2,
    
    orientation = "y"
    
  ) +
  
  geom_point(
    
    size = 2.8
    
  ) +
  
  scale_x_log10() +
  
  scale_colour_manual(
    
    values = c(
      
      "Significant" = "#D55E00",
      
      "Not significant" = "grey40"
      
    )
    
  ) +
  
  labs(
    
    x = "Hazard ratio (95% CI)",
    
    y = NULL,
    
    colour = NULL
    
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    
    legend.position = "top"
    
  )


ggsave(
  
  "Drought_survival_results_ChatGPT/Figures/Figure3_Trait_forest_plot.pdf",
  
  forest_plot,
  
  width = 8,
  
  height = 8
  
)


ggsave(
  
  "Drought_survival_results_ChatGPT/Figures/Figure3_Trait_forest_plot.tiff",
  
  forest_plot,
  
  width = 8,
  
  height = 8,
  
  dpi = 600,
  
  compression = "lzw"
  
)


# ============================================================
# SOIL MOISTURE AT DEATH
# ============================================================

# Soil moisture at death is not included as a predictor in
# the primary Cox models because it is measured at the event.
#
# It may provide complementary information about the soil
# moisture conditions associated with mortality.
#
# Analysis is restricted to plants that died and have a
# recorded soil moisture measurement.

humidity_data <- surv_data %>%
  
  filter(
    
    Status == 1,
    
    !is.na(humedad_muerte)
    
  )


humidity_summary <- humidity_data %>%
  
  group_by(Species, Parental, Clipping) %>%
  
  summarise(
    
    N = n(),
    
    Mean_humidity = mean(humedad_muerte),
    
    SD_humidity = sd(humedad_muerte),
    
    Median_humidity = median(humedad_muerte),
    
    .groups = "drop"
    
  )


write.csv(
  
  humidity_summary,
  
  "Drought_survival_results_ChatGPT/Tables/Table7_Soil_humidity_at_death.csv",
  
  row.names = FALSE
  
)


humidity_plot <- ggplot(
  
  humidity_data,
  
  aes(
    
    x = Parental,
    
    y = humedad_muerte,
    
    fill = Clipping
    
  )
  
) +
  
  geom_boxplot(
    
    alpha = 0.7,
    
    outlier.shape = NA
    
  ) +
  
  geom_jitter(
    
    width = 0.12,
    
    alpha = 0.5,
    
    size = 1.5
    
  ) +
  
  facet_wrap(
    
    ~ Species
    
  ) +
  
  labs(
    
    x = "Parental treatment",
    
    y = "Soil moisture at plant death",
    
    fill = "Clipping"
    
  ) +
  
  theme_classic(base_size = 12)


ggsave(
  
  "Drought_survival_results_ChatGPT/Figures/Figure4_Soil_humidity.pdf",
  
  humidity_plot,
  
  width = 9,
  
  height = 7
  
)


# ============================================================
# KAPLAN-MEIER MEDIAN SURVIVAL
# ============================================================

# Median survival time estimated using Kaplan-Meier.
# If survival probability never reaches 0.5, the median
# is not estimable within the observation period.

km_median <- survminer::surv_median(km_fit)


write.csv(
  
  km_median,
  
  "Drought_survival_results_ChatGPT/Tables/Table8_KM_median_survival.csv",
  
  row.names = FALSE
  
)


# ============================================================
# FINAL CHECKS
# ============================================================

# Number of observations
nrow(surv_data)

# Total mortality events
sum(surv_data$Status == 1)

# Total censored observations
sum(surv_data$Status == 0)

# Observed drought survival range
range(surv_data$Time, na.rm = TRUE)

# Number of observations per experimental combination
table(
  
  surv_data$Species,
  
  surv_data$Parental,
  
  surv_data$Clipping
  
)

# Check missing values in functional traits
sapply(
  
  surv_data[, traits],
  
  function(x) sum(is.na(x))
  
)