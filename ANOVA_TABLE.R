library(tidyverse)
library(car)
library(here)

files <- list.files(
  "anova_rds",
  pattern = "\\.rds$",
  full.names = TRUE
)

extract_anova <- function(file){
  
  x <- readRDS(file)
  
  file_name <- basename(file)
  
  file_name <- str_remove(
    file_name,
    "\\.rds$"
  )
  
  species <- str_extract(
    file_name,
    "^[A-Z]+"
  )
  
  trait <- file_name %>%
    str_remove(
      "^[A-Z]+_anova_"
    )
  
  df <- as.data.frame(x)
  
  df <- df %>%
    rownames_to_column("Predictor") %>%
    mutate(
      Species = species,
      Trait = trait
    )
  
  names(df) <- gsub(
    "Pr\\(>Chisq\\)",
    "pvalue",
    names(df)
  )
  
  names(df) <- gsub(
    "Pr\\(>F\\)",
    "pvalue",
    names(df)
  )
  
  statistic_column <- names(df)[
    grepl(
      "Chisq|F value",
      names(df)
    )
  ]
  
  test_type <- ifelse(
    statistic_column == "Chisq",
    "\u03c7\u00b2",
    "F"
  )
  
  df$Statistic <- paste0(
    test_type,
    " = ",
    sprintf(
      "%.3f",
      df[[statistic_column]]
    )
  )
  
  df %>%
    
    # eliminar residuos
    filter(
      Predictor %in%
        c(
          "trat",
          "clip",
          "trat:clip"
        )
    ) %>%
    
    mutate(
      
      pvalue = case_when(
        
        is.na(pvalue) ~ NA_character_,
        
        pvalue < 0.001 ~ "<0.001***",
        
        pvalue < 0.01 ~ paste0(
          sprintf("%.3f", pvalue),
          "**"
        ),
        
        pvalue < 0.05 ~ paste0(
          sprintf("%.3f", pvalue),
          "*"
        ),
        
        pvalue < 0.1 ~ paste0(
          sprintf("%.3f", pvalue),
          "·"
        ),
        
        TRUE ~ sprintf(
          "%.3f",
          pvalue
        )
        
      )
      
    ) %>%
    
    select(
      Species,
      Trait,
      Predictor,
      Statistic,
      pvalue
    )
  
}

TableS2 <- map_dfr(
  files,
  extract_anova
)

write.csv(
  TableS2,
  "TableS2_ANOVA_results.csv",
  row.names = FALSE
)