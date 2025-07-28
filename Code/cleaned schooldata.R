library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(purrr)

# Define folder paths and year mappings
data_paths <- c(
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/School_Data/Performancetables_21_22",
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/School_Data/Performancetables_22_23",
  "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Obtained_Data/School_Data/Performancetables_23_24"
)

year_mappings <- c(
  "Performancetables_21_22" = 2022,
  "Performancetables_22_23" = 2023,
  "Performancetables_23_24" = 2024
)

region_codes <- c("370", "371", "372", "373",  # South Yorkshire
                  "380", "381", "382", "383", "384")  # West Yorkshire

# Process and clean school data
school_data_combined <- map_dfr(data_paths, function(path) {
  file_path <- file.path(path, "england_ks4final.csv")
  folder_id <- basename(path)
  year_val <- year_mappings[folder_id]
  
  if (file.exists(file_path)) {
    df <- read_csv(file_path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      clean_names() %>%
      mutate(academic_year = year_val) %>%
      mutate(
        att8scr = na_if(att8scr, "SUP"),
        att8scr = na_if(att8scr, "NE"),
        att8scr = as.numeric(att8scr),
        p8mea = na_if(p8mea, "SUP"),
        p8mea = na_if(p8mea, "NE"),
        p8mea = as.numeric(p8mea)
      )
    
    return(df)
  } else {
    warning(paste("File not found at:", path))
    return(NULL)
  }
})

# Filter by region codes
school_data_filtered <- school_data_combined %>%
  filter(lea %in% region_codes)

# Verify lea column type
print(class(school_data_filtered$lea))

# Save processed data
write_csv(school_data_filtered, "/Users/yugpokharel/Documents/College/sem 4/Data science/Coursework_yugpokharel35D_230484/Cleaned_Data/cleaned_school_data.csv")

# Chart for Attainment 8 Scores by Year