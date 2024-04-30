rm(list = ls())
source("src/functions/install_dependencies.R")
#source("src/functions/read_school_info.R")
source("src/functions/internals.R")
source("src/functions/education_internals.R")
#source("src/indicators/adding_indicators_record_datasets.R")
source("src/indicators/test_function.R")


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
## svy_analysis needs a survey design from srvyr
#edu_data_survey <- as_survey(edu_data, weights = weight)

## list of available strata
list_strata <- colnames(edu_data)[grep("^stratum", colnames(edu_data))]
gender_s <- 'stratum_gender'
admin_s <- 'stratum_admin'
status_s <- 'stratum_status'
age_cycle_s <- 'stratum_school_cycle_level_age_category'
wgss_s <- 'stratum_severity_wgss'


## correct definition of the school-cycles as recorded in https://acted.sharepoint.com/:x:/r/sites/IMPACT-Humanitarian_Planning_Prioritization/_layouts/15/Doc.aspx?sourcedoc=%7B4925184A-EFF5-47AA-9687-D9CE0E00DD70%7D&file=UNESCO%20ISCED%20Mappings_MSNAcountries_consolidated.xlsx&action=default&mobileredirect=true&CID=A55770C3-4B6F-4D8E-AF24-32DC3F98E4FE&wdLOR=c8E8DBEC5-3E34-4F0B-9802-796C66A340A6
## to be used for the output tables and plots
level_label_mapping <- get_level_label_mapping(education_assestment_country)


indicator_label_mapping_EN <- c(school_aged_children_accessing = '% of children 5 to 18 y.o. who attended school or any early childhood education program at any time during the 2023-2024 school year',
                                school_aged_children_NON_accessing = '% of children who  who are not attending any level of education',
                                pre_school_age_accessing = '% of children (one year before the official primary school entry age) who are attending an early childhood education programme or primary school',
                                early_enrolment = 'Early enrolment in primary grade: Percentage of children in the relevant age group (one year before the official primary school entry age) who are attending primary school',
                                net_attendance_level1 = "Net attendance rate (adjusted) - Percentage of school-aged children of primary school age currently attending primary, lower or upper secondary school",
                                net_attendance_level2 = "Net attendance rate (adjusted) - Percentage of school-aged children of lower secondary school age currently attending lower secondary school or higher",
                                net_attendance_level3 = "Net attendance rate (adjusted) - Percentage of school-aged children of upper secondary school age currently attending upper secondary school or higher",
                                overage_learners_level1 = 'Percentage of school-aged children attending school who are at least 2 years above the intended age for grade: primary',
                                overage_learners_level2 = 'Percentage of school-aged children attending school who are at least 2 years above the intended age for grade: lower secondary',
                                disruption_climate = '% children 5 to 18 y.o. whose education was disrupted due natural hazard during the 2023-2024 school year',
                                disruption_teacher = '% children 5 to 18 y.o. whose education was disrupted due to teacher s absence during the 2023-2024 school year',
                                disruption_displaced = '% children 5 to 18 y.o. whose education was disrupted due the school being used as a shelter by IDPs during the 2023-2024 school year',
                                disruption_occupation = '% children 5 to 18 y.o. whose education was disrupted due the school being occupied by armed groups during the 2023-2024 school year',
                                education_barrier = '% children 5 to 18 y.o. not attending school or any early childhood education program at any time during the 2023-2024 school year, by main reason'
                            
                                   )

                                
                                
indicator_den <- c(school_aged_children_accessing = 'school_5_18_age',
                   school_aged_children_NON_accessing = 'school_5_18_age',
                   pre_school_age_accessing  = 'level1_minus_one_age',
                   early_enrolment = 'level1_minus_one_age',
                   net_attendance_level1 = 'level1_age',
                   net_attendance_level2 = 'level2_age',
                   net_attendance_level3 = 'level3_age',
                   overage_learners_level1 = 'attending_level1',
                   overage_learners_level2 = 'attending_level2',
                   disruption_climate = 'school_5_18_age',
                   disruption_teacher = 'school_5_18_age',
                   disruption_displaced = 'school_5_18_age',
                   disruption_occupation = 'school_5_18_age',
                   education_barrier = 'school_5_18_age_NON_accessing'
                   
                   
                   )

indicator_num <- c(school_aged_children_accessing = 'school_5_18_age_accessing',
                   school_aged_children_NON_accessing = 'school_5_18_age_NON_accessing',
                   pre_school_age_accessing = 'attending_level0_level1_and_level1_minus_one_age',
                   early_enrolment = 'attending_level1_and_level1_minus_one_age',
                   net_attendance_level1 = 'attending_level123_and_level1_age',
                   net_attendance_level2 = 'attending_level23_and_level2_age',
                   net_attendance_level3 = 'attending_level3_and_level3_age',
                   overage_learners_level1 = 'level1_overage_learners',
                   overage_learners_level2 = 'level2_overage_learners',
                   disruption_climate = 'education_disrupted_climate',
                   disruption_teacher = 'education_disrupted_teacher',
                   disruption_displaced = 'education_disrupted_displaced',
                   disruption_occupation = 'education_disrupted_occupation',
                   education_barrier = 'education_barrier'
                   )




## examples on how to calculate the MSNA 2024 education indicators 




run_edu_analysis_prop <- function(df, den, num, strata_string = NULL) {
  # Ensure that col1 and col2 are present in df
  if (!(den %in% names(df)) || !(num %in% names(df))) {
    stop("One or both specified columns do not exist in the dataframe.")
  }
  df <- df %>%  filter(!is.na(.[[den]]) & .[[den]] == 1)
  
  df <- df %>% mutate(!!num := case_when(
    .[[num]] == 1 ~ 'yes',
    .[[num]] == 0 ~ 'no',
    .[[num]] == 50 ~ 'dont know',
    .[[num]] == 99 ~ 'prefer not to answer',
    TRUE ~ as.character(.[[num]]) # Keep other values as they are
  ))
 
  # Handle strata_string if not NULL
  strata_columns <- if (!is.null(strata_string)) {
    if (is.character(strata_string)) {
      # If strata_string is a single string or comma-separated string, split and trim spaces
      unlist(strsplit(strata_string, ",\\s*"))
    } else if (is.vector(strata_string)) {
      # If strata_string is already a vector
      strata_string
    } else {
      # Fallback case, should not happen but for safety
      stop("strata_string must be either a character string or a vector of character strings.")
    }
  } else {
    # If strata_string is NULL, use an empty character vector
    character(0)
  }
  
  # Ensure strata columns exist in df
  missing_strata <- setdiff(strata_columns, names(df))
  if (length(missing_strata) > 0) {
    stop("The following strata columns do not exist in the dataframe: ", paste(missing_strata, collapse = ", "))
  }
  
  # Select columns dynamically
  columns_to_select <- c(num, den, "weight", strata_columns)
  df <- df %>% select(all_of(columns_to_select))
  
  
  ## svy_analysis needs a survey design from srvyr
  df_survey <- as_survey(df, weights = weight)
  result_df <- svy_analysis(design = df_survey, analysis = 'prop', vars = num,  group = strata_string)
  
  return(result_df)
}

all_indicators_results <- list()

# Extract the keys (names) of the indicators from indicator_den or indicator_num (both should have the same keys).
indicator_keys <- names(indicator_den)

# Loop over each indicator by its key.
for (indicator_key in indicator_keys) {
  # Extract the specific denominator and numerator for the current indicator.
  den <- indicator_den[[indicator_key]]
  num <- indicator_num[[indicator_key]]
  
  # Initialize an empty list to hold results for this indicator.
  results_for_current_indicator <- list(all = run_edu_analysis_prop(edu_data, den, num))

  # Perform the analysis for each stratum.
  for (stratum in list_strata) {
    # Run the analysis for the current stratum.
    stratum_result <- run_edu_analysis_prop(edu_data, den, num, stratum)

    # Store the result in the list.
    results_for_current_indicator[[stratum]] <- stratum_result
  }

  # Combine all stratum results into one, and store it under the indicator_key in the all_indicators_results list.
  combined_results <- bind_rows(results_for_current_indicator, .id = "source")
  all_indicators_results[[indicator_key]] <- combined_results
}


