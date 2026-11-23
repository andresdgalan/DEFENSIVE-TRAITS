# Survival analyses

# Load data and packages ----

library(tidyverse)
library(survival)
library(survminer)
library(broom)
library(patchwork)
library(here)
library(performance)

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

# Orden consistente para todas las figuras
species_order <- c(
  "AV",
  "CA",
  "ME",
  "HO",
  "PL"
)

species_labels <- c(
  "AV" = "A. barbata",
  "CA" = "C. arvensis",
  "ME" = "M. polymorpha",
  "HO" = "H. murinum",
  "PL" = "P. lagopus"
)

species_cols <- c(
  "AV" = "#2F9C95",  
  "CA" = "#E69F00",  
  "ME" = "#CC79A7",  
  "HO" = "#003F5C",  
  "PL" = "#C0392B"   
)


# Kaplan-Meier para las 10 combinaciones
fit <- survfit(
  Surv(drought_survival, status) ~ sp + trat,
  data = data_surv
)

# Extraer curvas
km_df <- surv_summary(fit)

# Separar strata
km_df <- km_df %>%
  separate(
    strata,
    into = c("sp", "trat"),
    sep = ", "
  ) %>%
  mutate(
    sp   = sub("sp=", "", sp),
    trat = sub("trat=", "", trat),
    
    sp   = trimws(sp),
    trat = trimws(trat),
    
    sp   = factor(sp, levels = species_order),
    trat = factor(trat, levels = c("ex", "gra"))
  )

# Asegurarse de que todas empiezan en t = 0
km_start <- km_df %>%
  distinct(sp, trat) %>%
  mutate(
    time = 0,
    surv = 1
  )

km_df_plot <- bind_rows(
  
  km_start,
  
  km_df %>%
    select(
      time,
      surv,
      sp,
      trat
    )
  
) %>%
  
  arrange(sp, trat, time)


# Gráfico
p_km <- ggplot(
  km_df_plot,
  aes(
    x = time,
    y = surv,
    colour = sp,
    linetype = trat,
    group = interaction(sp, trat)
  )
) +
  
    geom_step(
    linewidth = 1.1
  ) +
  
  scale_colour_manual(
    values = species_cols,
    labels = species_labels,
    name = "Species"
  ) +
  
  scale_fill_manual(
    values = species_cols,
    guide = "none"
  ) +
  
  scale_linetype_manual(
    values = c(
      ex = "solid",
      gra = "22"
    ),
    labels = c(
      ex = "Ungrazed",
      gra = "Grazed"
    ),
    name = "Parental grazing"
  ) +

  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Days after irrigation cessation",
    y = "Survival probability"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "right",
    
    legend.box = "vertical",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    axis.line = element_line(
      colour = "black"
    )
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype = "solid",
        linewidth = 1.2
      ),
      label.theme = element_text(
        face = "italic"
      )
    ),
    linetype = guide_legend(
      override.aes = list(
        colour = "black"
      )
    )
  )

p_km


# By species

fit_species <- survfit(
  Surv(drought_survival, status) ~ sp,
  data = data_surv
)

species_df <- surv_summary(fit_species) %>%
  mutate(
    sp = sub("sp=", "", strata),
    sp = trimws(sp),
    sp = factor(sp, levels = species_order)
  )

# Asegurarse de que todas las curvas empiezan en t = 0
species_start <- species_df %>%
  distinct(sp) %>%
  mutate(
    time = 0,
    surv = 1
  )

species_df_plot <- bind_rows(
  
  species_start,
  
  species_df %>%
    select(
      time,
      surv,
      sp
    )
  
) %>%
  
  arrange(sp, time)

# Test Log-rank para incluir en el gráfico:
species_lr <- survdiff(
  Surv(drought_survival, status) ~ sp,
  data = data_surv
)

species_p_lab <- paste0(
  "Log-rank: p ",
  format.pval(
    species_lr$pvalue,
    digits = 3,
    eps = 0.001
  )
)

# Gráfico
p_species <- ggplot(
  species_df_plot,
  aes(
    x = time,
    y = surv,
    colour = sp,
    group = sp
  )
) +
  
  geom_step(
    linewidth = 1.1
  ) +
  
  annotate(
    "text",
    x = 5,
    y = 0.1,
    label = species_p_lab,
    hjust = 0.3,
    vjust = 0.3,
    size = 5
  ) +
  
  scale_colour_manual(
    values = species_cols,
    labels = species_labels,
    name = "Species"
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Days after irrigation cessation",
    y = "Survival probability"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    axis.line = element_line(
      colour = "black"
    )
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        linewidth = 1.2
      ),
      label.theme = element_text(
        face = "italic"
      )
    )
  )

p_species

# Species x parental

fit_facet <- survfit(
  Surv(drought_survival, status) ~ sp + trat,
  data = data_surv
)

km_facet <- surv_summary(fit_facet) %>%
  separate(
    strata,
    into = c("sp", "trat"),
    sep = ", "
  ) %>%
  mutate(
    sp   = sub("sp=", "", sp),
    trat = sub("trat=", "", trat),
    
    sp   = trimws(sp),
    trat = trimws(trat),
    
    sp   = factor(sp, levels = species_order),
    trat = factor(trat, levels = c("ex","gra"))
  )

# todas las curvas arrancan en t = 0
facet_start <- km_facet %>%
  distinct(sp, trat) %>%
  mutate(
    time = 0,
    surv = 1
  )

km_facet_plot <- bind_rows(
  facet_start,
  km_facet %>%
    select(
      time,
      surv,
      sp,
      trat
    )
) %>%
  arrange(sp, trat, time)

# P valores por especie
facet_pvals <- data_surv %>%
  group_by(sp) %>%
  summarise(
    p = survdiff(
      Surv(drought_survival, status) ~ trat,
      data = cur_data()
    )$pvalue,
    .groups = "drop"
  ) %>%
  mutate(
    label = ifelse(
      p < 0.001,
      "p < 0.001",
      paste0(
        "p = ",
        sprintf("%.3f", p)
      )
    ),
    x = 10,
    y = 0.1
  )

# Título para cada panel
facet_labels <- c(
  AV = "italic('A. barbata')",
  CA = "italic('C. arvensis')",
  ME = "italic('M. polymorpha')",
  HO = "italic('H. murinum')",
  PL = "italic('P. lagopus')"
)

# Figura
p_speciesXparental <- ggplot(
  km_facet_plot,
  aes(
    x = time,
    y = surv,
    linetype = trat,
    group = interaction(sp, trat)
  )
) +
  
  geom_step(
    linewidth = 1.1,
    colour = "black"
  ) +
  
  geom_text(
    data = facet_pvals,
    aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1.05,
    size = 4
  ) +
  
  facet_wrap(
    ~ sp,
    ncol = 3,
    labeller = as_labeller(
      facet_labels,
      label_parsed
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      ex = "solid",
      gra = "22"
    ),
    labels = c(
      ex = "Ungrazed",
      gra = "Grazed"
    ),
    name = "Parental grazing"
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Days after irrigation cessation",
    y = "Survival probability"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    strip.background = element_rect(
      colour = "black",
      fill = "white"
    ),
    
    strip.text = element_text(
      size = 12
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    axis.line = element_blank()
  )

p_speciesXparental


# Species x clipping

fit_facet <- survfit(
  Surv(drought_survival, status) ~ sp + clip,
  data = data_surv
)

km_facet <- surv_summary(fit_facet) %>%
  separate(
    strata,
    into = c("sp", "clip"),
    sep = ", "
  ) %>%
  mutate(
    sp   = sub("sp=", "", sp),
    clip = sub("clip=", "", clip),
    
    sp   = trimws(sp),
    clip = trimws(clip),
    
    sp   = factor(sp, levels = species_order),
    clip = factor(clip, levels = c("no","yes"))
  )

# todas las curvas arrancan en t = 0
facet_start <- km_facet %>%
  distinct(sp, clip) %>%
  mutate(
    time = 0,
    surv = 1
  )

km_facet_plot <- bind_rows(
  facet_start,
  km_facet %>%
    select(
      time,
      surv,
      sp,
      clip
    )
) %>%
  arrange(sp, clip, time)

# P valores por especie
facet_pvals <- data_surv %>%
  group_by(sp) %>%
  summarise(
    p = survdiff(
      Surv(drought_survival, status) ~ clip,
      data = cur_data()
    )$pvalue,
    .groups = "drop"
  ) %>%
  mutate(
    label = ifelse(
      p < 0.001,
      "p < 0.001",
      paste0(
        "p = ",
        sprintf("%.3f", p)
      )
    ),
    x = 10,
    y = 0.1
  )

# Título para cada panel
facet_labels <- c(
  AV = "italic('A. barbata')",
  CA = "italic('C. arvensis')",
  ME = "italic('M. polymorpha')",
  HO = "italic('H. murinum')",
  PL = "italic('P. lagopus')"
)

# Figura
p_speciesXclipping <- ggplot(
  km_facet_plot,
  aes(
    x = time,
    y = surv,
    linetype = clip,
    group = interaction(sp, clip)
  )
) +
  
  geom_step(
    linewidth = 1.1,
    colour = "black"
  ) +
  
  geom_text(
    data = facet_pvals,
    aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1.05,
    size = 4
  ) +
  
  facet_wrap(
    ~ sp,
    ncol = 3,
    labeller = as_labeller(
      facet_labels,
      label_parsed
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      no = "solid",
      yes = "22"
    ),
    labels = c(
      no = "Unclipped",
      yes = "Clipped"
    ),
    name = "Within-generation grazing"
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    x = "Days after irrigation cessation",
    y = "Survival probability"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    
    strip.background = element_rect(
      colour = "black",
      fill = "white"
    ),
    
    strip.text = element_text(
      size = 12
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    axis.line = element_blank()
  )

p_speciesXclipping




# EXPORTAR
png("Fig_Kaplan_Meier_parental.png", width = 16, height = 10, units = "in", res = 600)
print(p_parental)
dev.off()


png("Fig_Kaplan_Meier_species.png", width = 12, height = 8, units = "in", res = 600)
print(p_species)
dev.off()

png("Fig_Kaplan_Meier_speciesXparental.png", width = 15, height = 8, units = "in", res = 600)
print(p_speciesXparental)
dev.off()

png("Fig_Kaplan_Meier_speciesXclipping.png", width = 15, height = 8, units = "in", res = 600)
print(p_speciesXclipping)
dev.off()

png("Fig_Kaplan_Meier_total.png", width = 12, height = 8, units = "in", res = 600)
print(p_km)
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

# Como las asunciones del modelo se han violado, vamos a 
# hacer un modelo para cada especie:

# COX AV
surv_AV <- data_surv[data_surv$sp == "AV",]
cox_AV <- coxph(
  Surv(drought_survival, status) ~
    trat * clip,
  data = surv_AV
)
anova(cox_AV)
cox.zph(cox_AV)

# COX HO
surv_HO <- data_surv[data_surv$sp == "HO",]
cox_HO <- coxph(
  Surv(drought_survival, status) ~
    trat * clip,
  data = surv_HO
)
anova(cox_HO)
cox.zph(cox_HO)

# COX PL
surv_PL <- data_surv[data_surv$sp == "PL",]
cox_PL <- coxph(
  Surv(drought_survival, status) ~
    trat * clip,
  data = surv_PL
)
anova(cox_PL)
cox.zph(cox_PL)

# COX ME
surv_ME <- data_surv[data_surv$sp == "ME",]
cox_ME <- coxph(
  Surv(drought_survival, status) ~
    trat * clip,
  data = surv_ME
)
anova(cox_ME)
cox.zph(cox_ME)

# COX CA
surv_CA <- data_surv[data_surv$sp == "CA",]
cox_CA <- coxph(
  Surv(drought_survival, status) ~
    trat * clip,
  data = surv_CA
)
anova(cox_CA)
cox.zph(cox_CA)


# TRAIT-BASED SURVIVAL ANALYSIS ----

trait_order <- c(
  "Plant height",
  "Aboveground biomass",
  "Leaf number",
  "Seed number",
  "Seed mass",
  "Leaf area",
  "SLA",
  "LDMC",
  "Root diameter",
  "SRL",
  "Photosynthesis",
  "Conductance",
  "PhiPSII",
  "Flowering start",
  "Flowering period",
  "Flowering end",
  "Leaf phenolics"
)

trait_labels <- c(
  hojas        = "Leaf number",
  altura       = "Plant height",
  massseed     = "Seed mass",
  area.m       = "Leaf area",
  Photosyn     = "Photosynthesis",
  fenoles      = "Leaf phenolics",
  dias_flor    = "Flowering start",
  periodo_flor = "Flowering period",
  dias_fin_flor= "Flowering end",
  biomass      = "Aboveground biomass",
  AvgDiam.mm.  = "Root diameter",
  seedtotal    = "Seed number",
  SLA          = "SLA",
  LDMC         = "LDMC",
  SRL          = "SRL",
  Conductance  = "Conductance",
  PhiPS2       = "PhiPSII"
)

trait_data_surv <- data_surv%>%
  summarise(.by = c(sp, trat), 
            across(everything(), 
                   ~ mean(.x, na.rm = TRUE)))




#----------------------------------------
# Variables
#----------------------------------------

traits <- c(
  "hojas",
  "altura",
  "massseed",
  "area.m",
  "Photosyn",
  "fenoles",
  "dias_flor",
  "periodo_flor",
  "dias_fin_flor",
  "biomass",
  "AvgDiam.mm.",
  "seedtotal",
  "SLA",
  "LDMC",
  "SRL",
  "Conductance",
  "PhiPS2"
)

response <- "drought_survival"


#----------------------------------------
# Ajustar modelos y extraer resultados
#----------------------------------------

results_table <- map_dfr(traits, function(tr){
  
  form <- reformulate(tr, response)
  
  mod <- lm(form, data = trait_data_surv)
  
  coef_tab <- summary(mod)$coefficients
  
  spear <- suppressWarnings(
    cor.test(
      trait_data_surv[[tr]],
      trait_data_surv[[response]],
      method = "spearman",
      exact = FALSE
    )
  )
  
  tibble(
    Trait = tr,
    Estimate = coef_tab[2, "Estimate"],
    SE = coef_tab[2, "Std. Error"],
    t = coef_tab[2, "t value"],
    p = coef_tab[2, "Pr(>|t|)"],
    Adj_R2 = summary(mod)$adj.r.squared,
    F = summary(mod)$fstatistic[1],
    Spearman_rho = unname(spear$estimate),
    Spearman_p = spear$p.value
  )
  
})

#----------------------------------------
# Formatear tabla
#----------------------------------------

results_table <- map_dfr(traits, function(tr){
  
  mod <- lm(
    as.formula(
      paste(response, "~", tr)
    ),
    data = trait_data_surv
  )
  
  coef_tab <- summary(mod)$coefficients
  
  spear <- suppressWarnings(
    cor.test(
      trait_data_surv[[tr]],
      trait_data_surv[[response]],
      method = "spearman",
      exact = FALSE
    )
  )
  
  tibble(
    Trait = tr,
    Estimate = coef_tab[2, "Estimate"],
    SE = coef_tab[2, "Std. Error"],
    t = coef_tab[2, "t value"],
    p = coef_tab[2, "Pr(>|t|)"],
    Adj_R2 = summary(mod)$adj.r.squared,
    F = summary(mod)$fstatistic[1],
    Spearman_rho = unname(spear$estimate),
    Spearman_p = spear$p.value
  )
  
})

results_table <- results_table %>%
  mutate(
    Trait = trait_labels[Trait],
    Trait = factor(Trait, levels = trait_order)
  ) %>%
  arrange(Trait) %>%
  mutate(
    across(
      c(Estimate, SE, t, Adj_R2, F, Spearman_rho),
      ~ round(.x, 3)
    ),
    p = signif(p, 3),
    Spearman_p = signif(Spearman_p, 3)
  ) %>%
  select(
    Trait,
    Estimate,
    Adj_R2,
    p,
    Spearman_rho,
    Spearman_p
  )

results_table

write.table(results_table, file = "trait_survival_table.csv", row.names = F, sep = ",")

#----------------------------------------
# Ajustar todos los modelos y guardarlos
#----------------------------------------

models <- setNames(
  lapply(traits, function(tr){
    
    lm(
      reformulate(tr, response),
      data = trait_data_surv
    )
    
  }),
  traits
)

#----------------------------------------
# Diagnósticos de un modelo concreto
#----------------------------------------

trait_to_check <- "SLA"

mod <- models[[trait_to_check]]

# Diagnósticos clásicos LM
par(mfrow = c(2,2))
plot(mod)

# Diagnósticos modernos
performance::check_model(mod)

#----------------------------------------
# Diagnósticos para TODOS los modelos
# (abre una ventana/gráfico para cada rasgo)
#----------------------------------------

for(tr in names(models)){
  
  cat("\n====================\n")
  cat("Trait:", tr, "\n")
  cat("====================\n\n")
  
  print(summary(models[[tr]]))
  
  print(
    performance::check_model(models[[tr]])
  )
  
}


# FIGURA ----

library(dplyr)
library(tidyr)
library(ggplot2)

# ==========================================================
# Rasgos
# ==========================================================

trait_subset <- c(
  "seedtotal",
  "area.m",
  "SLA",
  "LDMC",
  "dias_flor",
  "dias_fin_flor"
)

# ==========================================================
# Recalcular resumen (medias + SE)
# ==========================================================

trait_summary <- data_surv %>%
  summarise(
    .by = c(sp, trat),
    
    drought_survival_mean =
      mean(drought_survival, na.rm = TRUE),
    
    drought_survival_se =
      sd(drought_survival, na.rm = TRUE) /
      sqrt(sum(!is.na(drought_survival))),
    
    across(
      all_of(trait_subset),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        se = ~sd(.x, na.rm = TRUE) /
          sqrt(sum(!is.na(.x)))
      )
    )
  )

# ==========================================================
# Long format
# ==========================================================

plot_df <- trait_summary %>%
  pivot_longer(
    cols = -c(
      sp,
      trat,
      drought_survival_mean,
      drought_survival_se
    ),
    names_to = c("trait", ".value"),
    names_pattern = "(.*)_(mean|se)"
  )

# ==========================================================
# Nombres bonitos
# ==========================================================

plot_df <- plot_df %>%
  mutate(
    
    trait = recode(
      trait,
      
      seedtotal     = "Seed number",
      area.m        = "Leaf area (cm²)",
      SLA           = "SLA (cm² g⁻¹)",
      LDMC          = "LDMC (mg g⁻¹)",
      dias_flor     = "Flowering start (days since transplant)",
      dias_fin_flor = "Flowering end (days since transplant)"
    ),
    
    trait = factor(
      trait,
      levels = c(
        "Seed number",
        "Leaf area (cm²)",
        "SLA (cm² g⁻¹)",
        "LDMC (mg g⁻¹)",
        "Flowering start (days since transplant)",
        "Flowering end (days since transplant)"
      )
    )
  )

# ==========================================================
# Especies
# ==========================================================

plot_df <- plot_df %>%
  mutate(
    species = recode(
      sp,
      AV = "A. barbata",
      CA = "C. arvensis",
      ME = "M. polymorpha",
      HO = "H. murinum",
      PL = "P. lagopus"
    )
  )

plot_df$species <- factor(
  plot_df$species,
  levels = c(
    "A. barbata",
    "C. arvensis",
    "M. polymorpha",
    "H. murinum",
    "P. lagopus"
  )
)

species_labels <- c(
  "A. barbata"    = "italic('A. barbata')",
  "C. arvensis"   = "italic('C. arvensis')",
  "M. polymorpha" = "italic('M. polymorpha')",
  "H. murinum"    = "italic('H. murinum')",
  "P. lagopus"    = "italic('P. lagopus')"
)

# ==========================================================
# Grazed / Ungrazed
# ==========================================================

plot_df <- plot_df %>%
  mutate(
    parental = case_when(
      trat %in% c("ex","no",0,"0") ~ "Ungrazed",
      TRUE ~ "Grazed"
    )
  )

# ==========================================================
# Colores
# ==========================================================

species_cols <- c(
  "A. barbata"    = "#2F9C95",
  "C. arvensis"   = "#E69F00",
  "M. polymorpha" = "#CC79A7",
  "H. murinum"    = "#003F5C",
  "P. lagopus"    = "#C0392B"
)

# ==========================================================
# Figura
# ==========================================================

p_traits_survival <- ggplot(
  plot_df,
  aes(
    x = mean,
    y = drought_survival_mean
  )
) +
  
  geom_errorbar(
    aes(
      ymin = drought_survival_mean - drought_survival_se,
      ymax = drought_survival_mean + drought_survival_se,
      colour = species
    ),
    width = 0,
    alpha = 0.5,
    linewidth = 0.4
  ) +
  
  geom_segment(
    aes(
      x = mean - se,
      xend = mean + se,
      y = drought_survival_mean,
      yend = drought_survival_mean,
      colour = species
    ),
    alpha = 0.5,
    linewidth = 0.4
  ) +
  
  geom_smooth(
    aes(x = mean, y = drought_survival_mean),
    method = "lm",
    se = TRUE,
    colour = "black",
    fill = "grey85",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    aes(
      fill = species,
      colour = species,
      shape = parental
    ),
    size = 3.5,
    stroke = 1
  ) +
  
  facet_wrap(
    ~trait,
    scales = "free_x",
    ncol = 3
  ) +
  
  scale_colour_manual(
    values = species_cols,
    labels = parse(text = species_labels),
    name = "Species"
  ) +
  
  scale_fill_manual(
    values = species_cols,
    guide = "none"
  ) +
  
  scale_shape_manual(
    values = c(
      "Ungrazed" = 21,  # relleno
      "Grazed"   = 24    # hueco
    ),
    name = "Parental grazing"
  ) +
  
  guides(
    colour = guide_legend(
      order = 1,
      override.aes = list(
        shape = 16,
        size = 4
      )
    ),
    
    shape = guide_legend(
      order = 2,
      override.aes = list(
        shape = c(21, 24),
        fill = c("black", "black"),
        colour = "black",
        size = 4,
        stroke = 1
      )
    )
  )+
  
  labs(
    x = NULL,
    y = "Mean survival under drought (days)"
  ) +
  
  theme_bw(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    strip.background = element_blank(),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    legend.position = "right",
    
    legend.box = "vertical",
    
    legend.title = element_text(
      face = "bold"
    )
  )

p_traits_survival
