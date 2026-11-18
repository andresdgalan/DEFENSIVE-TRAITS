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
#
# Modifica este vector si deseas otro orden.
###############################################################################

trait_order <- c(
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

parental_heatmap$trait    <- factor(parental_heatmap$trait,
                                    levels = rev(trait_order))

clipping_heatmap$trait    <- factor(clipping_heatmap$trait,
                                    levels = rev(trait_order))

interaction_heatmap$trait <- factor(interaction_heatmap$trait,
                                    levels = rev(trait_order))

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
    strip.text = element_text(face = "bold")
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
    name = "% change"
  ) +
  labs(title = "Within-generation clipping") +
  heatmap_theme

p_interaction <- ggplot(
  interaction_heatmap,
  aes(species, trait, fill = effect)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = symbol), size = 4) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "% change"
  ) +
  labs(title = "Interaction") +
  heatmap_theme

Figure1 <- p_parental + p_clipping + p_interaction +
  plot_layout(ncol = 3)

ggsave(
  "Figure1_heatmaps.png",
  Figure1,
  width = 16,
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
#
# Solo SLA, SRL y PhiPS2
###############################################################################

traits_interaction <- c(
  "SLA",
  "SRL",
  "Conductance"
)

Figure3_data <- interaction_emm %>%
  filter(trait %in% traits_interaction)

Figure3 <- ggplot(
  Figure3_data,
  aes(
    clip,
    value,
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
    y = "Estimated marginal mean",
    colour = ""
  )

ggsave(
  "Figure3_interactions.png",
  Figure3,
  width = 15,
  height = 9,
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