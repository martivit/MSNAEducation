rm(list = ls())
source("src/functions/install_dependencies.R")
#source("src/functions/read_school_info.R")
source("src/functions/internals.R")
source("src/functions/education_internals.R")
source("src/indicators/adding_indicators_record_datasets.R")


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



## example data 
file_name <- paste0('input/dataset/', 'data.xlsx')
household_data  <- read_xlsx(file_name,
                             guess_max = 50000,
                             na = c("NA","#N/A",""," ","N/A"),
                             sheet = 'HH loop')
roster <- read_xlsx(file_name,
                    guess_max = 50000,
                    na = c("NA","#N/A",""," ","N/A"),
                    sheet = 'edu loop')
wgss <- read_xlsx(file_name,
                  guess_max = 50000,
                  na = c("NA","#N/A",""," ","N/A"),
                  sheet = 'wgss')


## to replace the levels' codes with the precise school-cycle label of the MSNA country 
get_level_label_mapping <- function(country_code_or_name) {
  file_school_cycle <- "contextspecific/UNESCO ISCED Mappings_MSNAcountries_consolidated.xlsx"
  info_country_school_structure <- read_school_level_grade_age(file_school_cycle, country_code_or_name)
  summary_info_school <- info_country_school_structure$df1    # DataFrame 1: level code, Learning Level, starting age, duration

  level_label_mapping <- setNames(summary_info_school$name_level, summary_info_school$level_code)
  
  return(level_label_mapping)
}


education_assestment_country <- "SYR"

edu_data    <-     roster_education_core_function(education_assestment_country,
                                                  roster, household_data,
                                                  'admin1',
                                                  'status',
                                                  'age_member',
                                                  'sex_member', 
                                                  'education_access',
                                                  'education_niveau',
                                                  'education_disrupted_climate',
                                                  'education_disrupted_teacher',
                                                  'education_disrupted_displaced',
                                                  'education_disrupted_occupation',
                                                  'education_barrier', 
                                                  start_school_year = 'september',
                                                  beginning_data_collection = 'may',
                                                  roster_wgss = wgss,
                                                  disability_seeing ='difficulty_seeing',
                                                  disability_hearing ='difficulty_hearing',
                                                  disability_walking ='difficulty_walking',
                                                  disability_remembering ='difficulty_remembering',
                                                  disability_selfcare ='difficulty_self_care',
                                                  disability_communicating ='difficulty_communicating',
                                                  severity_labeling = list(no_difficulty = 'none_difficulty', some_difficulty = 'some_difficulty', a_lot_of_difficulty = 'a_lot_of_difficulty', cannot_do_at_all = 'cannot_do_at_all')
)

level_label_mapping <- get_level_label_mapping(education_assestment_country)



## examples on how to calculate the MSNA 2024 education indicators 





