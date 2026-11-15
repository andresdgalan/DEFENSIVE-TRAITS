# Survival analyses

# Load data and packages ----

library(tidyverse)
library(survival)
library(survminer)
library(broom)
library(patchwork)
library(here)

data <- read.table(here("trait_data.csv"), header =T, sep = ",")


# PREPARE DATA ----

data_surv <- data %>%
  
  mutate(
    
    sp = factor(sp,
                levels = c("AV","HO","PL","CA","ME")),
    
    trat = factor(trat,
                  levels = c("ex","gra")),
    
    clip = factor(clip,
                  levels = c("no","yes")),
    
    # Phenological durations

    periodo_flor =
      dias_fin_flor - dias_flor,
    
    periodo_fruto =
      dias_fin_fruto - dias_fruto,
    
    # Survival during drought

    drought_survival =
      ifelse(dias_muerte > 135,
             dias_muerte - 135,
             NA),
    
    # Event indicator

    status =
      ifelse(!is.na(drought_survival), 1, NA)
    
  ) %>%
  
  filter(!is.na(drought_survival))

# KAPLAN-MEIER CURVES ----
# All species together

km_parental <- survfit(
  Surv(drought_survival, status) ~ trat,
  data = data_surv
)

p_parental <- ggsurvplot(
  km_parental,
  data = data_surv,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("#1b9e77","#d95f02"),
  xlab = "Days after irrigation cessation",
  ylab = "Survival probability",
  legend.title = "Parental treatment",
  legend.labs = c("Ungrazed","Grazed"),
  ggtheme = theme_classic(base_size = 14)
)

p_parental

# By species

km_species <- survfit(
  Surv(drought_survival, status) ~ sp,
  data = data_surv
)

p_species <- ggsurvplot(
  km_species,
  data = data_surv,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = FALSE,
  xlab = "Days after irrigation cessation",
  ylab = "Survival probability",
  ggtheme = theme_classic(base_size = 14)
)

p_species

# Species x parental

p_speciesXparental <- ggsurvplot_facet(
  survfit(
    Surv(drought_survival, status) ~ trat,
    data = data_surv
  ),
  data = data_surv,
  facet.by = "sp",
  pval = TRUE,
  conf.int = FALSE,
  ggtheme = theme_classic(base_size = 12)
)

p_speciesXparental

png("Fig_Kaplan_Meier_parental.png", width = 16, height = 10, units = "in", res = 600)
print(p_parental)
dev.off()


png("Fig_Kaplan_Meier_species.png", width = 16, height = 10, units = "in", res = 600)
print(p_species)
dev.off()

png("Fig_Kaplan_Meier_speciesXparental.png", width = 16, height = 10, units = "in", res = 600)
print(p_speciesXparental)
dev.off()

# LOG-RANK TESTS ----

# Species effect

log_rank_species <- survdiff(
  Surv(drought_survival, status) ~ sp,
  data = data_surv
)

log_rank_species

# Parental effect

log_rank_parental <- survdiff(
  Surv(drought_survival, status) ~ trat,
  data = data_surv
)

log_rank_parental

# Clipping effect

log_rank_clipping <- survdiff(
  Surv(drought_survival, status) ~ clip,
  data = data_surv
)

log_rank_clipping


# Cox proportional hadards model ----

cox_full <- coxph(
  
  Surv(drought_survival, status) ~
    
    sp * trat * clip,
  
  data = data_surv
  
)

summary(cox_full)

# ANOVA
anova(cox_full)

# Publication ready Hazard ratio table

cox_table <- tidy(
  cox_full,
  exponentiate = TRUE,
  conf.int = TRUE
)

cox_table

# Check proportional hazards assumption

ph_test <- cox.zph(cox_full)

ph_test

plot(ph_test)


# TRAIT-BASED SURVIVAL ANALYSIS ----

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
  "SRL"             # HE QUITADO LOS RASGOS RADICULARES PQ AFECTABAN CON ORDENES DE MAGNITUD DIFERENTES Y EN REALIDAD LOS RASGOS RADICULARES ESTÁN AFECTADOS POR LA SEQUÍA AL HABERSE TOMADO DESPUÉS
  
)

# Univariate Cox models

trait_results <- map_dfr(
  
  traits,
  
  function(tr){
    
    form <- as.formula(
      
      paste0(
        "Surv(drought_survival, status) ~ `",
        tr,
        "`"
      )
      
    )
    
    d <- data_surv %>%
      filter(!is.na(.data[[tr]]))
    
    if(nrow(d) < 20){
      
      return(NULL)
      
    }
    
    mod <- coxph(
      form,
      data = d
    )
    
    tidy(
      mod,
      exponentiate = TRUE,
      conf.int = TRUE
    ) %>%
      
      mutate(
        trait = tr
      )
    
  }
  
)


# MULTIPLE-TESTING CORRECTION
trait_results <- trait_results %>%
  
  mutate(
    
    p_fdr =
      p.adjust(
        p.value,
        method = "fdr"
      )
    
  )

trait_results


# FOREST PLOT OF TRAIT EFFECTS

trait_results %>%
  
  mutate(
    trait = reorder(trait, estimate)
  ) %>%
  
  ggplot(
    aes(
      estimate,
      trait
    )
  ) +
  
  geom_vline(
    xintercept = 1,
    linetype = 2
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0
  ) +
  
  theme_classic(base_size = 14) +
  
  labs(
    x = "Hazard ratio",
    y = NULL
  )


# TRAIT MODELS CONTROLING FOR SPECIES ----

trait_results_species <- map_dfr(
  
  traits,
  
  function(tr){
    
    form <- as.formula(
      
      paste0(
        "Surv(drought_survival, status) ~ sp + `",
        tr,
        "`"
      )
      
    )
    
    d <- data_surv %>%
      filter(!is.na(.data[[tr]]))
    
    if(nrow(d) < 20){
      
      return(NULL)
      
    }
    
    mod <- coxph(
      form,
      data = d
    )
    
    tidy(
      mod,
      exponentiate = TRUE,
      conf.int = TRUE
    ) %>%
      
      filter(term == tr) %>%
      
      mutate(
        trait = tr
      )
    
  }
  
)

# MULTIPLE-TESTING CORRECTION
trait_results_species <- trait_results_species %>%
  
  mutate(
    
    p_fdr =
      p.adjust(
        p.value,
        method = "fdr"
      )
    
  )

trait_results_species


# FOREST PLOT OF TRAIT EFFECTS

trait_results_species %>%
  
  mutate(
    trait = reorder(trait, estimate)
  ) %>%
  
  ggplot(
    aes(
      estimate,
      trait
    )
  ) +
  
  geom_vline(
    xintercept = 1,
    linetype = 2
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0
  ) +
  
  theme_classic(base_size = 14) +
  
  labs(
    x = "Hazard ratio",
    y = NULL
  )


