# En este script a recuperar datos de fenolgía que se habían perdido por el camino
# y los voy a incorporar a nuestra base de datos
# también voy a unir datos de los ángulos de crecimiento de las plantas

# DATOS
# la base de datos completa
data <- read.table("rawdata/datos_sibecol_2025.csv", header = TRUE, sep = ";")
# datos perdidos de fenología
fenologia <- read.table("rawdata/fenologia.csv", header = TRUE, sep = ",")
fenologia_clean <- fenologia %>%
  filter(!(id == "378" & rowSums(!is.na(across(starts_with("dias_")))) == 0))

# Ángulos de crecimiento
angulos <- read.table("rawdata/angles.csv", header = TRUE, sep = ",")


# I want to compare values for five variables across two data frames (data and fenologia) using a common key (id) to
# identify which ids have data for each variable:
    # 1. In data but not in fenologia
    # 2. In fenologia but not in data
    # 3. In both but values are different


# Step 1: Define the variables of interest
vars <- c("dias_flor", "dias_fruto", "dias_fin_flor", "dias_fin_fruto", "dias_muerte")

# Step 2: Merge both dataframes by id with suffixes to distinguish
merged <- merge(data[, c("id", vars)],
                fenologia_clean[, c("id", vars)],
                by = "id",
                suffixes = c("_data", "_feno"),
                all = TRUE)

# Step 3: Function to compare values for each variable
library(dplyr)

compare_results <- lapply(vars, function(var) {
  var_data <- paste0(var, "_data")
  var_feno <- paste0(var, "_feno")
  
  tibble(
    id = merged$id,
    data_value = merged[[var_data]],
    feno_value = merged[[var_feno]],
    only_in_data = !is.na(merged[[var_data]]) & is.na(merged[[var_feno]]),
    only_in_feno = is.na(merged[[var_data]]) & !is.na(merged[[var_feno]]),
    in_both_different = !is.na(merged[[var_data]]) & !is.na(merged[[var_feno]]) & merged[[var_data]] != merged[[var_feno]]
  ) %>% 
    filter(only_in_data | only_in_feno | in_both_different) %>%
    mutate(variable = var)
})


#NOW I HAVE DETECTED THE PROBLEMS AND I COMBINE BOTH DATAFRAMES
#TAKING THESE PROBLEMS INTO ACCOUNT:

#3.If value is only in fenologia_clean, use it to fill missing value in data.
#2.If both have different values:
  # For dias_muerte: keep the highest.
  # For dias_fin_fruto: keep the lowest, except for id == 63, where it should be NA.

#3.For id == 63, set dias_fin_flor to NA, regardless.

# First, merge the data with suffixes
merged <- merge(data[, c("id", vars)],
                fenologia_clean[, c("id", vars)],
                by = "id",
                suffixes = c("_data", "_feno"),
                all.x = TRUE)

# Now apply the update logic for each variable
updated <- merged %>% mutate(
  dias_flor = ifelse(!is.na(dias_flor_data), dias_flor_data, dias_flor_feno),
  dias_fruto = ifelse(!is.na(dias_fruto_data), dias_fruto_data, dias_fruto_feno),
  
  dias_fin_flor = case_when(
    id == 63 ~ NA,  # override manually
    !is.na(dias_fin_flor_data) ~ dias_fin_flor_data,
    TRUE ~ dias_fin_flor_feno
  ),
  
  dias_fin_fruto = case_when(
    id == 63 ~ NA,  # override manually
    !is.na(dias_fin_fruto_data) & !is.na(dias_fin_fruto_feno) ~ pmin(dias_fin_fruto_data, dias_fin_fruto_feno),
    !is.na(dias_fin_fruto_data) ~ dias_fin_fruto_data,
    TRUE ~ dias_fin_fruto_feno
  ),
  
  dias_muerte = case_when(
    !is.na(dias_muerte_data) & !is.na(dias_muerte_feno) ~ pmax(dias_muerte_data, dias_muerte_feno),
    !is.na(dias_muerte_data) ~ dias_muerte_data,
    TRUE ~ dias_muerte_feno
  )
)

# Now create the updated version of `data`:
data_updated <- data
data_updated <- data_updated %>%
  select(-all_of(vars)) %>%  # remove the old versions of the variables
  left_join(
    updated %>% select(id, all_of(vars)),
    by = "id"
  )

write.table(x = data_updated, file = "rawdata/datos_sibecol_2025.csv", sep = ";", row.names = F)
