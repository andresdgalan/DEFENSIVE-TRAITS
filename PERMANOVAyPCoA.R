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

# datos
traits <- read.table(here("trait_data.csv"), header =T, sep = ",")%>%
  select(-c(massmother, humedad_muerte, seedfruto, flor, seedtotal,
            dias_fruto, dias_fin_fruto, dias_muerte)) %>%
  rename(Species = sp, Parental = trat)

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

# Calculamos, para cada pareja, el número de rasgos comparables
n <- nrow(traits)

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

############################################################
# 1. Separar metadatos y rasgos
############################################################

meta <- traits %>%
  select(Species, Parental, id)

X <- traits %>%
  select(-c(Species, Parental, id, clip))

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

perm <- adonis2(
  dist.mat ~ Species * Parental,
  data = meta,
  permutations = 999
)

print(perm)

perm.terms <- adonis2(
  dist.mat ~ Species * Parental,
  data = meta,
  permutations = 999,
  by = "terms"
)

print(perm.terms)

perm.margin <- adonis2(
  dist.mat ~ Species * Parental,
  data = meta,
  permutations = 999,
  by = "margin"
)

print(perm.margin)

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
  meta$Parental
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

scores <- bind_cols(scores, meta)

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
  permutations = 999,
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
  filter(pval < 0.05)

############################################################
# 12. Escalar flechas para visualización
############################################################

mult <- 0.2

vec.sig$Dim1 <- vec.sig$Axis.1 * mult
vec.sig$Dim2 <- vec.sig$Axis.2 * mult

############################################################
# 13. Gráfico
############################################################

ggplot(
  scores,
  aes(
    x = PCoA1,
    y = PCoA2,
    colour = Species,
    shape = Parental
  )
) +
  
  geom_point(
    size = 3,
    alpha = 0.8
  ) +
  
  stat_ellipse(
    aes(
      colour = Species,
      linetype = Parental,
      group = interaction(Species, Parental)
    ),
    linewidth = 1,
    level = 0.95
  ) +
  
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
  
  geom_text(
    data = vec.sig,
    aes(
      x = Dim1,
      y = Dim2,
      label = Trait
    ),
    inherit.aes = FALSE,
    size = 3.5
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
    shape = "Parental"
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
# 15. Tabla de rasgos significativos
############################################################

vec.sig %>%
  arrange(pval)