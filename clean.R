library(tidyverse)

# Note: This function will only work for the most recent Guardian datasets (from 2018)
save_cleaned_data <- function(raw_data) {
  
  # Read in raw data
  df <- read_csv(raw_data)
  
  # Change column names
  colnames(df) <- str_remove_all(colnames(df), "sheets.players.")
  
  # Filter out NA columns
  df <- df[,colSums(is.na(df)) < 100]
  
  # Filter out irrelevant columns
  df <- df %>% 
    select(-Filters, -Subbed, -Revised, -sheets.header_image)
  
  # Change column names
  colnames(df) <- str_remove_all(colnames(df), "sheets.players.")
  
  # Save cleaned data
  write.csv(df, str_replace(raw_data, "raw", "clean"), row.names = FALSE)
  
}

save_cleaned_data("data/male-raw-2021.csv")
save_cleaned_data("data/female-raw-2021.csv")
