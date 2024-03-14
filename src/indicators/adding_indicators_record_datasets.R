rm(list = ls())
source("src/functions/install_dependencies.R")
source("src/functions/read_school_info.R")

pacman::p_load(tidyverse,
               hypegrammaR,
               rio,
               readxl,
               openxlsx,
               sjmisc,
               dplyr,
               tibble,
               tidyr,
               illuminate,
               srvyr,
               purrr,
               readxl)

# Use the functions
school_variables_sheet    <- read_xlsx('contextspecific/context_info.xlsx', sheet = "School levels and grades")

# Extract the necessary values
start_school_year <- school_variables_sheet[which(school_variables_sheet['variable name'] == "start_school_year"), "country variable name"]
data_collection   <- school_variables_sheet[which(school_variables_sheet['variable name'] == "data_collection"), "country variable name"]
numer_levels      <- as.numeric(school_variables_sheet[which(school_variables_sheet['variable name'] == "numer_levels"), "country variable name"])

# Calculate age_correction and create levels_df
age_correction       <- calculate_age_correction(start_school_year, data_collection)
levels_grades        <- create_levels_df(school_variables_sheet, which(school_variables_sheet['variable name'] == "numer_levels"), numer_levels)

## example how to extract -->
#primary_level <- levels_df_result %>% filter(level_name == "primary") %>%  select(entrance, duration)
