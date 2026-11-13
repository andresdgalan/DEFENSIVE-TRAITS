############################################################
# Survival analyses
############################################################

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
