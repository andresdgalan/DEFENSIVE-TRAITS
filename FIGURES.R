# Una última observación: antes de generar la Figura 1, yo inspeccionaría la distribución de effect en los tres heatmaps. Si aparecen valores extremos (por ejemplo ±500% en algún rasgo con medias muy pequeñas), conviene fijar los límites de color con algo como limits = c(-100,100) y oob = scales::squish, porque unos pocos valores extremos pueden ocultar el patrón general del resto de rasgos. Para publicación suele quedar bastante mejor.

###############################################################################
# FIGURAS DEL ARTÍCULO
#
# INPUTS NECESARIOS (25 dataframes):

library(here)



AV_parental_emmeans <- read.table(here("AV_parental_emmeans.csv"), header =T, sep = ",")
HO_parental_emmeans <- read.table(here("HO_parental_emmeans.csv"), header =T, sep = ",")
PL_parental_emmeans <- read.table(here("PL_parental_emmeans.csv"), header =T, sep = ",")
CA_parental_emmeans <- read.table(here("CA_parental_emmeans.csv"), header =T, sep = ",")
ME_parental_emmeans <- read.table(here("ME_parental_emmeans.csv"), header =T, sep = ",")

AV_interaction_emmeans <- read.table(here("AV_interaction_emmeans.csv"), header =T, sep = ",")
HO_interaction_emmeans <- read.table(here("HO_interaction_emmeans.csv"), header =T, sep = ",")
PL_interaction_emmeans <- read.table(here("PL_interaction_emmeans.csv"), header =T, sep = ",")
CA_interaction_emmeans <- read.table(here("CA_interaction_emmeans.csv"), header =T, sep = ",")
ME_interaction_emmeans <- read.table(here("ME_interaction_emmeans.csv"), header =T, sep = ",")

AV_parental_heatmap <- read.table(here("AV_parental_heatmap.csv"), header =T, sep = ",")
HO_parental_heatmap <- read.table(here("HO_parental_heatmap.csv"), header =T, sep = ",")
PL_parental_heatmap <- read.table(here("PL_parental_heatmap.csv"), header =T, sep = ",")
CA_parental_heatmap <- read.table(here("CA_parental_heatmap.csv"), header =T, sep = ",")
ME_parental_heatmap <- read.table(here("ME_parental_heatmap.csv"), header =T, sep = ",")

AV_clipping_heatmap <- read.table(here("AV_clipping_heatmap.csv"), header =T, sep = ",")
HO_clipping_heatmap <- read.table(here("HO_clipping_heatmap.csv"), header =T, sep = ",")
PL_clipping_heatmap <- read.table(here("PL_clipping_heatmap.csv"), header =T, sep = ",")
CA_clipping_heatmap <- read.table(here("CA_clipping_heatmap.csv"), header =T, sep = ",")
ME_clipping_heatmap <- read.table(here("ME_clipping_heatmap.csv"), header =T, sep = ",")

AV_interaction_heatmap <- read.table(here("AV_interaction_heatmap.csv"), header =T, sep = ",")
HO_interaction_heatmap <- read.table(here("HO_interaction_heatmap.csv"), header =T, sep = ",")
PL_interaction_heatmap <- read.table(here("PL_interaction_heatmap.csv"), header =T, sep = ",")
CA_interaction_heatmap <- read.table(here("CA_interaction_heatmap.csv"), header =T, sep = ",")
ME_interaction_heatmap <- read.table(here("ME_interaction_heatmap.csv"), header =T, sep = ",")

##############################################################################

library(tidyverse)
library(patchwork)

###############################################################################
# 1. NORMALIZAR LOS DATAFRAMES DE EMMEANS
#
# Los glm/nbinom2/gamma devuelven:
#   response, asymp.LCL, asymp.UCL
#
# Los lm gaussianos devuelven:
#   emmean, lower.CL, upper.CL
#
# Esta función crea columnas comunes:
#
#   value
#   lower
#   upper
#
###############################################################################

normalize_emmeans <- function(df){
  
  df$value <- ifelse(
    is.na(df$response),
    df$emmean,
    df$response
  )
  
  df$lower <- ifelse(
    is.na(df$asymp.LCL),
    df$lower.CL,
    df$asymp.LCL
  )
  
  df$upper <- ifelse(
    is.na(df$asymp.UCL),
    df$upper.CL,
    df$asymp.UCL
  )
  
  df
  
}

###############################################################################
# 2. AÑADIR IDENTIFICADOR DE ESPECIE
###############################################################################

AV_parental_emmeans$species <- "AV"
HO_parental_emmeans$species <- "HO"
PL_parental_emmeans$species <- "PL"
CA_parental_emmeans$species <- "CA"
ME_parental_emmeans$species <- "ME"

AV_interaction_emmeans$species <- "AV"
HO_interaction_emmeans$species <- "HO"
PL_interaction_emmeans$species <- "PL"
CA_interaction_emmeans$species <- "CA"
ME_interaction_emmeans$species <- "ME"

AV_parental_emmeans <- normalize_emmeans(AV_parental_emmeans)
HO_parental_emmeans <- normalize_emmeans(HO_parental_emmeans)
PL_parental_emmeans <- normalize_emmeans(PL_parental_emmeans)
CA_parental_emmeans <- normalize_emmeans(CA_parental_emmeans)
ME_parental_emmeans <- normalize_emmeans(ME_parental_emmeans)

AV_interaction_emmeans <- normalize_emmeans(AV_interaction_emmeans)
HO_interaction_emmeans <- normalize_emmeans(HO_interaction_emmeans)
PL_interaction_emmeans <- normalize_emmeans(PL_interaction_emmeans)
CA_interaction_emmeans <- normalize_emmeans(CA_interaction_emmeans)
ME_interaction_emmeans <- normalize_emmeans(ME_interaction_emmeans)

###############################################################################
# 3. UNIR TODAS LAS ESPECIES
###############################################################################

parental_emm <- bind_rows(
  AV_parental_emmeans,
  HO_parental_emmeans,
  PL_parental_emmeans,
  CA_parental_emmeans,
  ME_parental_emmeans
)

interaction_emm <- bind_rows(
  AV_interaction_emmeans,
  HO_interaction_emmeans,
  PL_interaction_emmeans,
  CA_interaction_emmeans,
  ME_interaction_emmeans
)

###############################################################################
# 4. UNIR LOS DATOS DE LOS HEATMAPS
###############################################################################

AV_parental_heatmap$species <- "AV"
HO_parental_heatmap$species <- "HO"
PL_parental_heatmap$species <- "PL"
CA_parental_heatmap$species <- "CA"
ME_parental_heatmap$species <- "ME"

AV_clipping_heatmap$species <- "AV"
HO_clipping_heatmap$species <- "HO"
PL_clipping_heatmap$species <- "PL"
CA_clipping_heatmap$species <- "CA"
ME_clipping_heatmap$species <- "ME"

AV_interaction_heatmap$species <- "AV"
HO_interaction_heatmap$species <- "HO"
PL_interaction_heatmap$species <- "PL"
CA_interaction_heatmap$species <- "CA"
ME_interaction_heatmap$species <- "ME"

parental_heatmap <- bind_rows(
  AV_parental_heatmap,
  HO_parental_heatmap,
  PL_parental_heatmap,
  CA_parental_heatmap,
  ME_parental_heatmap
)

clipping_heatmap <- bind_rows(
  AV_clipping_heatmap,
  HO_clipping_heatmap,
  PL_clipping_heatmap,
  CA_clipping_heatmap,
  ME_clipping_heatmap
)

interaction_heatmap <- bind_rows(
  AV_interaction_heatmap,
  HO_interaction_heatmap,
  PL_interaction_heatmap,
  CA_interaction_heatmap,
  ME_interaction_heatmap
)

# Guardar las tablas unidas
write.table(parental_heatmap, file = "parental_heatmap.csv", row.names = F, sep = ",")
write.table(clipping_heatmap, file = "clipping_heatmap.csv", row.names = F, sep = ",")
write.table(interaction_heatmap, file = "interaction_heatmap.csv", row.names = F, sep = ",")

###############################################################################
# 5. ORDEN DE LOS RASGOS
###############################################################################

library(dplyr)
library(tidyr)

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

species_labels <- c(
  AV = "A. barbata",
  HO = "H. murinum",
  PL = "P. lagopus",
  CA = "C. arvensis",
  ME = "M. polymorpha"
)

species_order <- c(
  "A. barbata",
  "H. murinum",
  "P. lagopus",
  "C. arvensis",
  "M. polymorpha"
)

for(df_name in c("parental_heatmap",
                 "clipping_heatmap")) {
  
  df <- get(df_name)
  
  # nombres de rasgos
  df$trait <- recode(df$trait, !!!trait_labels)
  
  # nombres de especies
  df$species <- recode(df$species, !!!species_labels)
  
  # completar combinaciones faltantes especie × rasgo
  df <- df %>%
    complete(
      species = species_order,
      trait = trait_order,
      fill = list(
        effect = NA_real_,
        pvalue = NA_real_,
        symbol = NA_character_
      )
    )
  
  # ordenar factores
  df$trait <- factor(
    df$trait,
    levels = rev(trait_order)
  )
  
  df$species <- factor(
    df$species,
    levels = species_order
  )
  
  assign(df_name, df)
}

# quitamos rasgos que no entran en la selección
parental_heatmap <- parental_heatmap %>%
  filter(!is.na(trait))

clipping_heatmap <- clipping_heatmap %>%
  filter(!is.na(trait))


###############################################################################
# 6. FIGURA 1
#
# HEATMAPS
###############################################################################

heatmap_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      face = "italic"
    )
  )

fill_limit <- 100

parental_heatmap <- parental_heatmap %>%
  mutate(
    symbol = ifelse(
      abs(effect) > fill_limit,
      paste0(symbol, "\n", round(effect), "%"),
      symbol
    )
  )

clipping_heatmap <- clipping_heatmap %>%
  mutate(
    symbol = ifelse(
      abs(effect) > fill_limit,
      paste0(symbol, "\n", round(effect), "%"),
      symbol
    )
  )

p_parental <- ggplot(
  parental_heatmap,
  aes(species, trait, fill = effect)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = symbol), size = 4) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-fill_limit, fill_limit),
    oob = scales::squish,
    breaks = c(-100, -50, 0, 50, 100),
    labels = c("≤−100", "−50", "0", "50", "≥100"),
    name = "% change"
  ) +
  labs(title = "Parental grazing") +
  heatmap_theme

p_clipping <- ggplot(
  clipping_heatmap,
  aes(species, trait, fill = effect)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = symbol), size = 4) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-fill_limit, fill_limit),
    oob = scales::squish,
    breaks = c(-100, -50, 0, 50, 100),
    labels = c("≤−100", "−50", "0", "50", "≥100"),
    name = "% change"
  ) +
  labs(title = "Within-generation grazing") +
  heatmap_theme +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

Figure1 <- p_parental + p_clipping +
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  theme(
    legend.position = "right"
  )
Figure1

ggsave(
  "Figure1_heatmaps.png",
  Figure1,
  width = 8,
  height = 10,
  dpi = 600
)

###############################################################################
# 7. FIGURA 2
#
# EFECTOS PARENTALES
#
# 6 rasgos seleccionados
###############################################################################

traits_parental <- c(
  "LDMC",
  "fenoles",
  "dias_flor",
  "PhiPS2",
  "SRL",
  "biomass"
)

Figure2_data <- parental_emm %>%
  filter(trait %in% traits_parental)

Figure2 <- ggplot(
  Figure2_data,
  aes(
    species,
    value,
    colour = trat
  )
) +
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.4)
  ) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(
    ~ trait,
    scales = "free_y",
    ncol = 3
  ) +
  scale_colour_manual(
    values = c(
      ex = "black",
      gra = "firebrick"
    ),
    labels = c(
      ex = "Ungrazed mothers",
      gra = "Grazed mothers"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.background = element_blank()
  ) +
  labs(
    x = "Species",
    y = "Estimated marginal mean",
    colour = ""
  )

ggsave(
  "Figure2_parental_effects.png",
  Figure2,
  width = 12,
  height = 8,
  dpi = 600
)

###############################################################################
# 8. FIGURA 3
#
# INTERACCIONES
###############################################################################

traits_interaction <- c(
  "SLA (cm²/g)",
  "SRL (m/g)",
  "Conductance (mmol/m²s)",
  "Flowering period (days)",
  "Seed number",
  "Aboveground biomass (g)",
  "Leaf number",
  "LDMC (mg/g)"
)

interaction_emm <- interaction_emm %>%
  mutate(
    trait = case_when(
      trait == "hojas" ~ "Leaf number",
      trait == "massseed" ~ "Plant height",
      trait == "Photosyn" ~ "Photosynthesis",
      trait == "fenoles" ~ "Leaf phenolics",
      trait == "dias_flor" ~ "Flowering start",
      trait == "dias_fin_flor" ~ "Flowering end",
      trait == "periodo_flor" ~ "Flowering period (days)",
      trait == "biomass" ~ "Aboveground biomass (g)",
      trait == "AvgDiam.mm." ~ "Root diameter",
      trait == "SLA" ~ "SLA (cm²/g)",
      trait == "Conductance" ~ "Conductance (mmol/m²s)",
      trait == "SRL" ~ "SRL (m/g)",
      trait == "seedtotal" ~ "Seed number",
      trait == "LDMC" ~ "LDMC (mg/g)",
      TRUE ~ trait
    ),
    species = case_when(
      species == "AV" ~ "A. barbata",
      species == "HO" ~ "H. murinum",
      species == "PL" ~ "P. lagopus",
      species == "CA" ~ "C. arvensis",
      species == "ME" ~ "M. polymorpha",
      TRUE ~ species
    ),
    clip = case_when(
      clip == "no" ~ "Unclipped",
      clip == "yes" ~ "Clipped",
      TRUE ~ as.character(clip)
    )
  )

interaction_heatmap <- interaction_heatmap %>%
  mutate(
    trait = case_when(
      trait == "hojas" ~ "Leaf number",
      trait == "massseed" ~ "Plant height",
      trait == "Photosyn" ~ "Photosynthesis",
      trait == "fenoles" ~ "Leaf phenolics",
      trait == "dias_flor" ~ "Flowering start",
      trait == "dias_fin_flor" ~ "Flowering end",
      trait == "periodo_flor" ~ "Flowering period (days)",
      trait == "biomass" ~ "Aboveground biomass (g)",
      trait == "AvgDiam.mm." ~ "Root diameter",
      trait == "SLA" ~ "SLA (cm²/g)",
      trait == "Conductance" ~ "Conductance (mmol/m²s)",
      trait == "SRL" ~ "SRL (m/g)",
      trait == "seedtotal" ~ "Seed number",
      trait == "LDMC" ~ "LDMC (mg/g)",
      TRUE ~ trait
    ),
    species = case_when(
      species == "AV" ~ "A. barbata",
      species == "HO" ~ "H. murinum",
      species == "PL" ~ "P. lagopus",
      species == "CA" ~ "C. arvensis",
      species == "ME" ~ "M. polymorpha",
      TRUE ~ species
    )
  )

interaction_heatmap <- interaction_heatmap %>%
  mutate(
    trait = factor(
      trait,
      levels = c(
        "Aboveground biomass (g)",
        "Leaf number",
        "Seed number",
        "SLA (cm²/g)",
        "LDMC (mg/g)",
        "SRL (m/g)",
        "Conductance (mmol/m²s)",
        "Flowering period (days)"
      )
    )
  )

Figure3_data <- interaction_emm %>%
  filter(trait %in% traits_interaction)%>%
  mutate(
    trait = factor(
      trait,
      levels = c(
        "Aboveground biomass (g)",
        "Leaf number",
        "Seed number",
        "SLA (cm²/g)",
        "LDMC (mg/g)",
        "SRL (m/g)",
        "Conductance (mmol/m²s)",
        "Flowering period (days)"
      )
    )
  )

sig_labels <- interaction_heatmap %>%
  filter(
    trait %in% traits_interaction,
    symbol != ""
  ) %>%
  left_join(
    Figure3_data %>%
      group_by(trait, species) %>%
      summarise(
        y = max(upper, na.rm = TRUE) * 1.08,
        .groups = "drop"
      ),
    by = c("trait", "species")
  ) %>%
  mutate(
    x = 1.5
  )

Figure3 <- ggplot(
  Figure3_data,
  aes(
    x = clip,
    y = value,
    colour = trat,
    group = trat
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0.08
  ) +
  geom_text(
    data = sig_labels,
    aes(
      x = x,
      y = y,
      label = symbol
    ),
    inherit.aes = FALSE,
    colour = "black",
    size = 4.5,
    fontface = "bold"
  ) +
  facet_grid(
    trait ~ species,
    scales = "free_y"
  ) +
  scale_colour_manual(
    values = c(
      ex = "black",
      gra = "firebrick"
    ),
    labels = c(
      ex = "Ungrazed mothers",
      gra = "Grazed mothers"
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    strip.background = element_blank()
  ) +
  labs(
    x = "Within-generation clipping",
    y = "Estimated marginal mean (95% CI)",
    colour = ""
  )

Figure3

ggsave(
  "Figure3_interactions.png",
  Figure3,
  width = 9,
  height = 15,
  dpi = 600
)

###############################################################################
# OUTPUTS
#
# Figure1_heatmaps.png
# Figure2_parental_effects.png
# Figure3_interactions.png
#
###############################################################################