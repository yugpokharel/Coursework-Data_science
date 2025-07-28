library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(stringr)

# Define paths
base_path <- "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/School_Data"
output_folder <- "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data"

# Create output folder if missing
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Read and process school data from folders
process_school_data <- function(base_path) {
  year_folders <- c("Performancetables_21_22", "Performancetables_22_23", "Performancetables_23_24")
  all_data <- list()
  
  for (folder in year_folders) {
    folder_path <- file.path(base_path, folder)
    
    if (!dir.exists(folder_path)) {
      message("Folder not found: ", folder_path)
      next
    }
    
    files <- list.files(folder_path, full.names = TRUE, pattern = "\\.(csv|xls|xlsx)$")
    
    if (length(files) == 0) {
      message("No CSV or Excel files found in: ", folder_path)
      next
    }
    
    # Extract academic year in form "2021_2022"
    academic_year <- str_extract(folder, "\\d{2}_\\d{2}") %>%
      str_replace("(\\d{2})_(\\d{2})", "20\\1_20\\2")
    
    for (file in files) {
      tryCatch({
        if (str_detect(file, "\\.csv$")) {
          df <- read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE) %>% clean_names()
        } else if (str_detect(file, "\\.xls[x]?$")) {
          df <- read_excel(file, .name_repair = "universal") %>%
            clean_names() %>%
            select(-matches("^x_|^\\.\\.\\.")) # Drop unnamed cols
        } else {
          message("Unknown file type: ", basename(file))
          next
        }
        
        file_type <- case_when(
          str_detect(file, regex("ks4final", ignore_case = TRUE)) ~ "ks4_final",
          str_detect(file, regex("ks4provisional", ignore_case = TRUE)) ~ "ks4_provisional",
          str_detect(file, regex("underlying", ignore_case = TRUE)) ~ "underlying_data",
          str_detect(file, regex("school_information", ignore_case = TRUE)) ~ "school_info",
          TRUE ~ "other"
        )
        
        df <- df %>%
          mutate(
            source_file = basename(file),
            academic_year = academic_year,
            file_type = file_type
          )
        
        all_data[[paste(academic_year, basename(file), sep = "_")]] <- df
        
      }, error = function(e) {
        message("Error processing file: ", file)
        message(e$message)
      })
    }
  }
  all_data
}

# Clean individual data frames
clean_school_df <- function(df) {
  df %>%
    # Convert special codes to NA
    mutate(across(everything(), ~ na_if(., "NP")),
           across(everything(), ~ na_if(., "NE")),
           across(everything(), ~ na_if(., "SUPP")),
           across(everything(), ~ na_if(., "NULL"))) %>%
    # Remove commas and convert percentages & numeric columns
    mutate(across(matches("pt|percent|pct|p_|proportion|rate"), 
                  ~ case_when(
                    is.na(.) ~ NA_real_,
                    str_detect(., "%") ~ as.numeric(str_remove_all(., "[%,]")) / 100,
                    TRUE ~ as.numeric(str_remove_all(., ",")) 
                  ))) %>%
    mutate(across(any_of(c("attainment_8_score", "progress_8_score", "percentage", "score", 
                           "value", "average", "total", "count", "number", "amount", "sum")), 
                  ~ as.numeric(str_remove_all(., ",")))) %>%
    # Clean character columns and trim whitespace
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    mutate(across(where(is.character), str_trim)) %>%
    # Title case school_name if present
    {if ("school_name" %in% names(.)) mutate(., school_name = str_to_title(school_name)) else .} %>%
    # Convert date columns if present (dd/mm/yyyy)
    {if ("opendate" %in% names(.)) mutate(., opendate = as.Date(opendate, format = "%d/%m/%Y")) else .} %>%
    {if ("closedate" %in% names(.)) mutate(., closedate = as.Date(closedate, format = "%d/%m/%Y")) else .}
}

# Main function to clean all school data
clean_all_school_data <- function(base_path) {
  raw_data <- process_school_data(base_path)
  clean_data <- map(raw_data, clean_school_df)
  bind_rows(clean_data)
}

# Run cleaning pipeline
school_clean_data <- clean_all_school_data(base_path)

# Save combined cleaned data
write_csv(school_clean_data, file.path(output_folder, "school_clean_data.csv"))

# Summary output
message("\nCleaning complete! Combined data saved to:")
message(file.path(output_folder, "school_clean_data.csv"))
message("\nDataset dimensions: ", nrow(school_clean_data), " rows, ", ncol(school_clean_data), " columns")

# Inspect cleaned data structure
glimpse(school_clean_data)

