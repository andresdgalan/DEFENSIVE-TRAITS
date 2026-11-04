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
  select(Species, Parental)

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

############################################################
# 5. Comprobar homogeneidad de dispersiones
############################################################

# Especie

beta.species <- betadisper(
  dist.mat,
  meta$Species
)

permutest(beta.species)

# Tratamiento parental

beta.parental <- betadisper(
  dist.mat,
  meta$Parental
)

permutest(beta.parental)

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
  X.scaled,
  permutations = 999
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

mult <- 2

vec.sig$Dim1 <- vec.sig$Dim1 * mult
vec.sig$Dim2 <- vec.sig$Dim2 * mult

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