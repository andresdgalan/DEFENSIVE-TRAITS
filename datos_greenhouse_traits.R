# En este script voy a actualizar la matriz de RASGOS incluyendo
# BIOMASA y WINRHIZO

# DATOS Y PAQUETES ----

library(tidyverse)
library(here)

# MATRIZ DE RASGOS
data <- read.table(here("rawdata/datos_sibecol_2025.csv"), header = TRUE, sep = ";")
# BIOMASA
biomasa_frozen <- read.table(here("rawdata/peso_seco_cosecha_congelada.csv"), header = TRUE, sep = ",")
biomasa_resto <- read.table(here("rawdata/pesos_muerta_semillas_resto_irga.csv"), header = TRUE, sep = ",")
# WINRHIZO
winrhizo <- read.table(here("rawdata/winrhizo.csv"), header = T, sep = ",")
raices_pesos_gabi <- read.table(here("rawdata/raices_pesos_gabi.csv"), header = T, sep = ",")

# BIOMASA ----

# Miramos que los ids estén igual escritos que en la matriz principal
setdiff(biomasa_resto$id, data$id)
setdiff(biomasa_frozen$id, data$id)

# Unimos los datos de pesos y los sumamos en una única variable "biomass"
biomass_data <- full_join(biomasa_frozen, biomasa_resto)
# Antes de unir vamos a asegurarnos de que no vayamos a tomar por biomasa total
# La biomasa de las semillas. Es decir, si no hubiese datos de muerta ni frozen,
# Descartaríamos esos datos.
which(is.na(biomass_data$frozen) & is.na(biomass_data$muerta))
# Hay un caso, pero el peso de restos no es descabellado así que lo dejamos
biomass_data <- biomass_data %>% 
  mutate(biomass = rowSums(.[,-1], na.rm = T)) %>%
           select(id, biomass)

# Miramos de nuevo que los nombres coincidan, y unimos con todos los datos
setdiff(biomass_data$id, data$id)
data <- full_join(data, biomass_data)

# Miramos la distribución de los datos de biomasa por especie
plot(density(data$biomass[data$sp == "AV"], na.rm = T))
plot(density(data$biomass[data$sp == "HO"], na.rm = T))
plot(density(data$biomass[data$sp == "ME"], na.rm = T))
plot(density(data$biomass[data$sp == "CA"], na.rm = T))
plot(density(data$biomass[data$sp == "PL"], na.rm = T))


# WINRHIZO ----

# Nos quedamos con las variables que nos interesan
winrhizo_selection <- winrhizo%>%
  select(c(RHIZO.2009, AvgDiam.mm., Length.cm., RootVolume.cm3.))%>%
  rename(full_id = RHIZO.2009)

# creamos full_id en la matriz de pesos de raíces
raices_selection <- raices_pesos_gabi %>%
  mutate(full_id = paste0(sp, id))

# Miramos que tengan los mismos ids
setdiff(winrhizo_selection$full_id, raices_selection$full_id)
setdiff(raices_selection$full_id, winrhizo_selection$full_id)

# Juntamos los datos de raíces en una matriz y calculamos RTD y SRL
roots <- full_join(winrhizo_selection, raices_selection)%>%
  mutate(RTD = paste0(peso_winrhizo/RootVolume.cm3.))%>% # g/cm3
  mutate(SRL = paste0(Length.cm./100*peso_winrhizo)) # cm/g salen números muy grandes, cambiamos a m/g que es más habitual
# Forzamos que RTD y SRL se traten como números
class(roots$RTD) <- "numeric"
class(roots$SRL) <- "numeric"

trait_data <- full_join(data, roots) %>%
  select(-c(full_id, peso_winrhizo, peso_resto, Length.cm., RootVolume.cm3., profundidad_sustrato))


# VISUALIZAMOS LA DISTRIBUCIÓN DE LAS VARIABLES ----

# Identificar variables numéricas (numeric e integer)
numeric_vars <- names(trait_data)[sapply(trait_data, function(x) {
  is.numeric(x) || is.integer(x)
})]

sp <- c("AV", "HO", "PL", "CA", "ME")

par(mfrow = c(5,5))

# Loop para generar los gráficos de densidad
for (species in sp) {
  for (var in numeric_vars) {
    
    x <- na.omit(trait_data[[var]][trait_data$sp == species])
    
    if (length(x) >= 2 && length(unique(x)) > 1) {
      
      plot(
        density(x),
        main = paste(var, "-", species),
        xlab = paste0("N = ", length(x)) 
      )
      
    }
    
  }
}


# VISUALIZAMOS LA CORRELACIÓN DE LAS VARIABLES ----

library(corrplot)

# Seleccionar variables numéricas
numeric_vars <- trait_data[, sapply(trait_data, is.numeric)]

# Calcular matriz de correlaciones
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs",
                  method = "pearson")

# Correlograma
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  col = colorRampPalette(c("red", "white", "blue"))(200),
  addCoef.col = "black",
  number.cex = 0.6,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE
)
