# PARA USAR ESTE SCRIPT HAY QUE CORRER PRIMERO PLOTTING.Rmd, ESTO ES SOLO UNA ADAPTACIÓN PARA EL CONGRESO DE ALEMANIA

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(patchwork)

### --- P. lagopus (Clipped vs Unclipped) ---
plot_data_PL <- plot_data %>%
  filter(sp == "PL", fill_group %in% c("gra_clip_yes", "gra_clip_no"))

fill_colors_PL <- c("gra_clip_yes" = "#FF69B4", "gra_clip_no" = "#8B3A62")
fill_labels_PL <- c("gra_clip_yes" = "Clipped", "gra_clip_no" = "Unclipped")

make_plot_PL <- function(yvar) {
  ggplot(plot_data_PL, aes(x = fill_group, y = .data[[yvar]])) +
    geom_boxplot(aes(fill = fill_group), outlier.shape = NA) +
    geom_jitter(
      aes(fill = fill_group),
      color = "black", size = 0.4, alpha = 0.9,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
    ) +
    scale_fill_manual(values = fill_colors_PL, labels = fill_labels_PL, 
                      breaks = c("gra_clip_no", "gra_clip_yes")) +
    theme_ipsum() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),     # remove x labels
      axis.ticks.x = element_blank(),    # remove x ticks
      axis.text.y = element_text(size = 10),
      legend.position = "none",
      plot.margin = margin(5,5,5,5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
}

p_PL1 <- make_plot_PL("PhiPS2")
p_PL2 <- make_plot_PL("SLA")
p_PL3 <- make_plot_PL("fenoles")


### --- A. barbata (Ungrazed vs Grazed, clip == no) ---
plot_data_AV <- plot_data %>%
  filter(sp == "AV", trat %in% c("gra", "ex"), clip == "no") %>%
  droplevels()

fill_colors_AV <- c("ex" = "#008B45", "gra" = "#8B3A62")
fill_labels_AV <- c("ex" = "Ungrazed", "gra" = "Grazed")

make_plot_AV <- function(yvar) {
  ggplot(plot_data_AV, aes(x = trat, y = .data[[yvar]], fill = trat)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(
      aes(fill = trat),
      color = "black",
      size = 0.4,
      alpha = 0.9,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
    ) +
    scale_fill_manual(values = fill_colors_AV, labels = fill_labels_AV, 
                      breaks = c("ex", "gra")) +
    theme_ipsum() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),     # remove x labels
      axis.ticks.x = element_blank(),    # remove x ticks
      axis.text.y = element_text(size = 10),
      legend.position = "none",
      plot.margin = margin(5,5,5,5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
}

p_AV1 <- make_plot_AV("PhiPS2")
p_AV2 <- make_plot_AV("SLA")
p_AV3 <- make_plot_AV("fenoles")


### --- Combine all in one figure ---
final_plot <- (p_PL1 | p_PL2 | p_PL3) /
  (p_AV1 | p_AV2 | p_AV3)

final_plot


# Ahora voy a hacer una versión para las jornadas predoctorales

# Crear la variable combinada trat_clip
plot_data_AV <- plot_data %>%
  filter(sp == "AV", trat %in% c("gra", "ex")) %>%
  mutate(trat_clip = paste(trat, clip, sep = "_")) %>%
  droplevels()

# Definir colores y etiquetas para las cuatro combinaciones
fill_colors_AV <- c(
  "ex_no" = "#008B45",    # Ungrazed - no clip
  "ex_yes" = "#00FF7F",   # Ungrazed - clip
  "gra_no" = "#8B3A62",   # Grazed - no clip
  "gra_yes" = "#FF69B4"   # Grazed - clip
)

fill_labels_AV <- c(
  "ex_no" = "Ungrazed - No clip",
  "ex_yes" = "Ungrazed - Clip", 
  "gra_no" = "Grazed - No clip",
  "gra_yes" = "Grazed - Clip"
)

make_plot_AV <- function(yvar) {
  ggplot(plot_data_AV, aes(x = trat_clip, y = .data[[yvar]], fill = trat_clip)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(
      aes(fill = trat_clip),
      color = "black",
      size = 0.4,
      alpha = 0.9,
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
    ) +
    scale_fill_manual(values = fill_colors_AV, labels = fill_labels_AV, 
                      breaks = c("ex_no", "ex_yes", "gra_no", "gra_yes")) +
    theme_ipsum() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),     # remove x labels
      axis.ticks.x = element_blank(),    # remove x ticks
      axis.text.y = element_text(size = 10),
      legend.position = "none",
      plot.margin = margin(5,5,5,5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
    )
}

p_AV1 <- make_plot_AV("PhiPS2")
p_AV2 <- make_plot_AV("SLA")
p_AV3 <- make_plot_AV("fenoles")
p_AV4 <- make_plot_AV("seedtotal")

### --- Combine all in one figure ---
final_plot <- (p_AV1 | p_AV2 | p_AV3 | p_AV4)

final_plot
