############################################################
# PERMANOVA + PCoA + envfit
# Rasgos funcionales
############################################################

# Paquetes
library(tidyverse)
library(vegan)
library(cluster)
library(ape)
library(grid)
library(here)
library(ggrepel)

# datos
traits <- read.table(here("trait_data.csv"), header =T, sep = ",")%>%
  select(-c(massmother, humedad_muerte, fruto, flor,
            dias_fruto, dias_fin_fruto, dias_muerte, RTD, seedfruto)) %>%
  mutate(
    "Flowering period" =
      dias_fin_flor - dias_flor,
    "trat" = 
      ifelse(trat == "gra", "Grazed", "Ungrazed"),
    "sp" = case_when(
      sp == "AV" ~ "A. barbata",
      sp == "HO"  ~ "H. murinum",
      sp == "PL"  ~ "P. lagopus",
      sp == "CA"  ~ "C. arvensis",
      sp == "ME"  ~ "M. polymorpha",
      TRUE ~ sp # Mantiene cualquier otro valor intacto (ej. NAs)
    )) %>%
  rename(Species = sp, "Parental grazing" = trat, 
         "Leaf number" = hojas, "Plant height" = altura,
         "Seed mass" = massseed, "Leaf area" = area.m, 
         "Photosynthesis" = Photosyn, "Leaf phenolics" = fenoles,
         "Flowering start" = dias_flor, "Flowering end" = dias_fin_flor,
         "Aboveground biomass" = biomass, "Root diameter" = AvgDiam.mm.,
         "Seed number" = seedtotal) %>%
  select("id", "Species", "Parental grazing", "clip", "Plant height", "Aboveground biomass", "Leaf number", "Seed number",
         "Seed mass", "Leaf area", "SLA", "LDMC", "Root diameter", "SRL", "Photosynthesis",
         "Conductance", "PhiPS2", "Flowering start", "Flowering period", "Flowering end",
         "Leaf phenolics") # Esto es solo para ordenar las columnas



# FILTRAMOS POR MISSINGNESS
# PRIMERO QUITAMOS VARIABLES CON POCOS DATOS Y VARIABLES CORRELACIONADAS
# missingness <- traits %>%
#        group_by(Species, Parental) %>%
#        summarise(across(everything(), ~sum(is.na(.))), .groups = "drop")
# print(as.data.frame(missingness))
# 
# sample_size <- traits %>%
#   group_by(Species, Parental) %>%
#   summarise(across(everything(), ~sum(!is.na(.))), .groups = "drop")
# print(as.data.frame(sample_size))

# conjunto core de rasgos 
# (ejes funcionales distintos, cobertura razonable, evitan redundancias)
# core <- traits %>%
#   select(-c(flor, seedtotal, massseed, hojas, altura,
#             dias_fruto, dias_fin_fruto, dias_muerte,
#             LDMC, area.m, Conductance, Photosyn, dias_fin_flor,
#             AvgDiam.mm., RTD))

# Quitamos las filas que tienen menos de 3 datos (+4 metadatos)
traits <- traits %>%
       filter(rowSums(!is.na(.)) > 7)

############################################################
# 1. Separar metadatos y rasgos
############################################################

X <- traits %>%
  select(-c(Species, "Parental grazing", id, clip))

# Calculamos, para cada pareja, el número de rasgos comparables
n <- nrow(X)

shared <- matrix(
  0,
  nrow = n,
  ncol = n
)

for(i in 1:n){
  for(j in 1:n){
    
    shared[i,j] <- sum(
      !is.na(X[i,]) &
        !is.na(X[j,])
    )
    
  }
}

head(shared)

# Distribución global

shared_upper <- shared[upper.tri(shared)]

summary(shared_upper)

table(shared_upper)

# ELIMINAR FILAS PROBLEMÁTICAS (260)
which(shared == 0, arr.ind = TRUE)

traits <- traits[-260,]
X <- X[-260,]

meta <- traits %>%
  select(Species, "Parental grazing", id)




############################################################
# 2. Estandarizar rasgos
############################################################

X.scaled <- scale(X)

############################################################
# 3. Matriz de distancias (Gower)
# Admite NAs
############################################################

dist.mat <- daisy(
  X.scaled,
  metric = "gower"
)

############################################################
# 4. PERMANOVA
############################################################

set.seed(123)


perm.terms <- adonis2(
  dist.mat ~ Species * `Parental grazing`,
  data = meta,
  permutations = 9999,
  by = "terms"
)

print(perm.terms)


# Multivariate trait composition differed among species (R² = 0.54, p = 0.001), 
# was weakly affected by parental treatment (R² = 0.015, p = 0.001), 
# and showed a significant species-specific transgenerational response (Species × Parental: R² = 0.044, p = 0.001).

# Cuando existe una interacción fuerte, los efectos principales 
# dejan de ser totalmente interpretables por sí mismos. 
# Por eso adonis2() en muchos casos sólo devuelve 
# la interacción en by="margin"

############################################################
# 5. Comprobar homogeneidad de dispersiones
############################################################

# Especie (las especies difieren en su dispersión multivariante)

beta.species <- betadisper(
  dist.mat,
  meta$Species
)

permutest(beta.species)

# Species affected multivariate trait composition (PERMANOVA, p < 0.001, F = 84.3), 
# although multivariate dispersion also differed among species (betadisper, p < 0.001, F = 19.9).

# Tratamiento parental (los tratamientos no difieren en su dispersión)

beta.parental <- betadisper(
  dist.mat,
  meta$"Parental grazing"
)

permutest(beta.parental)

# los efectos del tratamiento parental
# sse pueden interpretar más tranquilamente como un 
# desplazamiento de centroides y no como un artefacto de dispersión

############################################################
# 6. PCoA
############################################################

pcoa.res <- pcoa(dist.mat)

############################################################
# 7. Coordenadas de individuos
############################################################

scores <- as.data.frame(
  pcoa.res$vectors[, 1:2]
)

colnames(scores) <- c("PCoA1", "PCoA2")

# Orden consistente para todas las figuras
species_order <- c(
  "A. barbata",
  "C. arvensis",
  "M. polymorpha",
  "H. murinum",
  "P. lagopus"
)

scores <- bind_cols(scores, meta)%>%
  mutate(Species   = factor(Species, levels = species_order))

############################################################
# 8. Porcentaje de varianza explicada
############################################################

var1 <- round(
  pcoa.res$values$Relative_eig[1] * 100,
  1
)

var2 <- round(
  pcoa.res$values$Relative_eig[2] * 100,
  1
)

############################################################
# 9. Ajustar vectores de rasgos
############################################################

fit <- envfit(
  pcoa.res$vectors[, 1:2],
  as.data.frame(X.scaled),
  permutations = 9999,
  na.rm = T
)

print(fit)

############################################################
# 10. Extraer vectores
############################################################

vec <- as.data.frame(
  scores(fit, display = "vectors")
)

vec$Trait <- rownames(vec)

############################################################
# 11. Seleccionar únicamente rasgos significativos
############################################################

vec$pval <- fit$vectors$pvals

vec.sig <- vec %>%
  filter(pval <= 0.05)


############################################################
# 12. Escalar flechas para visualización
############################################################

mult <- 0.2

vec.sig$Dim1 <- vec.sig$Axis.1 * mult
vec.sig$Dim2 <- vec.sig$Axis.2 * mult

############################################################
# 13. Gráfico
############################################################



species_cols <- c(
  "A. barbata" = "#2F9C95", 
  "C. arvensis" = "#E69F00",  
  "M. polymorpha" = "#CC79A7",  
  "H. murinum" = "#003F5C",  
  "P. lagopus" = "#C0392B"   
)

offset <- 0.02

vec.sig.lab <- vec.sig |>
  mutate(
    lab_x = Dim1 + ifelse(Dim1 >= 0, offset, -offset),
    lab_y = Dim2 + ifelse(Dim2 >= 0, offset, -offset)
  )

ggplot(
  scores,
  aes(
    x = PCoA1,
    y = PCoA2,
    colour = Species,
    shape = `Parental grazing`
  )
) +
  
  geom_point(
    size = 3,
    alpha = 0.8
  ) +
  
  # stat_ellipse(
  #   aes(
  #     colour = Species,
  #     linetype = "Parental grazing",
  #     group = interaction(Species, "Parental grazing")
  #   ),
  #   linewidth = 1,
  #   level = 0.95
  # ) +
  
  geom_segment(
    data = vec.sig,
    aes(
      x = 0,
      y = 0,
      xend = Dim1,
      yend = Dim2
    ),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 0.6,
    arrow = arrow(
      length = unit(0.20, "cm")
    )
  ) +
  scale_linetype_manual(
    values = c(
      "ex"  = "solid",
      "gra" = "dashed"
    )
  ) +
  
  scale_colour_manual(
    values = species_cols
  ) +


  geom_text_repel(
    data = vec.sig.lab,
    aes(
      x = lab_x,
      y = lab_y,
      label = Trait
    ),
    segment.color = "grey50",
    max.overlaps = Inf,
    inherit.aes = FALSE
  ) +
  
  # geom_text(
  #   aes(label = id),
  #   size = 2,
  #   vjust = -0.5
  # ) +
  
  theme_bw() +
  
  labs(
    x = paste0(
      "PCoA1 (",
      var1,
      "%)"
    ),
    y = paste0(
      "PCoA2 (",
      var2,
      "%)"
    ),
    colour = "Species",
    shape = "Parental grazing"
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(),
      label.theme = element_text(face = "italic")
    )
  )

############################################################
# 14. Guardar figura
############################################################

ggsave(
  "PCoA_traits.png",
  width = 8,
  height = 6,
  dpi = 300
)

############################################################
# 15. Boxplots de rasgos significativos
############################################################

traits.sig <- rownames(vec.sig)

plot.df <- traits %>%
  select(Species, all_of(traits.sig)) %>%
  pivot_longer(
    cols = all_of(traits.sig),
    names_to = "Trait",
    values_to = "Value"
  )

ggplot(
  plot.df,
  aes(
    x = Species,
    y = Value,
    fill = Species
  )
) +
  geom_boxplot() +
  facet_wrap(~ Trait, scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
