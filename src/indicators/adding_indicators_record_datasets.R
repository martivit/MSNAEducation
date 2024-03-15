rm(list = ls())
source("src/functions/install_dependencies.R")
source("src/functions/read_school_info.R")
source("src/functions/internals.R")

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




  file_name <- paste0('input/dataset/', 'data.xlsx')

  
  household_data  <- read_xlsx(file_name,
                                        guess_max = 50000,
                                        na = c("NA","#N/A",""," ","N/A"),
                                        sheet = 'HH loop')
  roster <- read_xlsx(file_name,
                      guess_max = 50000,
                      na = c("NA","#N/A",""," ","N/A"),
                      sheet = 'edu loop')
 



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



roster_education_core_function <- function(
    roster,
    household_data,
    ind_age = 'ind_age',
    ind_gender = 'ind_gender',
    education_access = 'education_access',
    education_level_grade = 'education_level_grade',
    education_disrupted_climate = 'education_disrupted_climate',
    education_disrupted_teacher = 'education_disrupted_teacher',
    education_disrupted_displaced = 'education_disrupted_displaced',
    education_disrupted_occupation = 'education_disrupted_occupation',
    education_barrier = 'education_barrier',
    start_school_year = 'september',
    beginning_data_collection = 'may',
    levels_grades = NULL, # DataFrame input by the user
    # Default vectors as hints for the user if levels_grades is not provided
    starting_age = c(kindergarden = 4, primary = 6, `lower secondary` = 14, `upper secondary` = 17),
    duration = c(kindergarden = 2, primary = 8, `lower secondary` = 3, `upper secondary` = 1)
) {
  
 
  
  #------ Enquos
  edu_cols <- rlang::enquos(education_access, education_disrupted_climate, education_disrupted_teacher, education_disrupted_displaced, education_disrupted_occupation)
  edu_cols <- purrr::map_chr(edu_cols, rlang::as_name)
  
  ind_age_col <- rlang::as_name(rlang::enquo(ind_age)) 
  ind_gender_col <- rlang::as_name(rlang::enquo(ind_gender)) 
  
  #------ Check if in_age exist
  if_not_in_stop(roster, ind_age_col, "roster")
  if_not_in_stop(roster, edu_cols, "roster")
  
  are_cols_numeric (roster, ind_age_col)
  
  
  #------ modify yes/no to numeric variables --> 
  #-- yes = 1
  #-- no = 0
  roster <- modify_column_value_yes_no(roster, edu_cols)
  
  #------ standardize the gender labels --> 
  #-- female/femme = girl
  #-- male/home = boy
  roster <- modify_gender_values(roster, ind_gender_col)
  
  
  #------ compute the logic variable for the age correction and Add new corrected age column to the roster
  age_correction  <- calculate_age_correction(start_school_year, beginning_data_collection)
  #
  roster <- dplyr::mutate(
    roster,
    corrected_ind_age = dplyr::case_when(
      is.na(!!rlang::sym(ind_age_col)) ~ NA_real_, # Keeps NA values as NA
      age_correction == TRUE ~ !!rlang::sym(ind_age_col) - 1, # Subtract 1 if age_correction is TRUE
      TRUE ~ !!rlang::sym(ind_age_col) # Copies the same value if age_correction is FALSE
    )
  )
  true_age_col <- "corrected_ind_age"  # Direct assignment
  education_access_col <- "education_access"
  
  

  
  #------ Definition of School Age, Age Limits for Each School Level, and Methods for Counting School-Age Children
  level_names <- c('kindergarden', 'primary', 'lower secondary', 'upper secondary') # Fixed level names 
  # Ensure that starting_age and duration have entries for all levels
  if (!is.null(levels_grades) && all(c("level", "starting_age", "duration") %in% names(levels_grades))) {
    # Use levels_grades as the primary source of level info
    school_level_info <- levels_grades
  } else {
    # Fallback to using the provided starting_age and duration vectors
    # Transforming the vectors into a data frame
    school_level_info <- data.frame(
      level = names(starting_age),
      starting_age = unname(starting_age),
      duration = unname(duration),
      stringsAsFactors = FALSE
    )
  }
  school_level_info <- school_level_info %>% mutate(ending_age = starting_age + duration - 1 )
  
  primary_info <- school_level_info %>% filter(level == "primary")  %>% select(starting_age, ending_age)
  lower_secondary_info <- school_level_info %>% filter(level == "lower secondary") %>% select(starting_age,  ending_age)
  upper_secondary_info <- school_level_info %>% filter(level == "upper secondary") %>% select(starting_age,  ending_age)
  kindergarden_info <- school_level_info %>% filter(level == "kindergarden") %>% select(starting_age,  ending_age)
  

  print(school_level_info)


  #------ adding school-cycles age columns, these are the denominators needed to calculate properly the education indicators
  roster <- roster %>%
    mutate(
      # Use if_else to preserve NA values. if_else's third argument (false) defaults to NA when unspecified
      school_5_18_age = if_else(between(!!rlang::sym(true_age_col), 5, 18), 1, 0, missing = NA_integer_),
      primary_age = if_else(between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age), 1, 0, missing = NA_integer_),
      lower_secondary_age = if_else(between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age), 1, 0, missing = NA_integer_),
      upper_secondary_age = if_else(between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1), 1, 0, missing = NA_integer_),
      primary_minus_one_age = if_else(!!rlang::sym(true_age_col) == (primary_info$starting_age - 1), 1, 0, missing = NA_integer_),

      # For gender-specific calculations, add the gender condition to the logical vector
      school_5_18_age_girl = if_else(between(!!rlang::sym(true_age_col), 5, 18) & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),
      primary_age_girl = if_else(between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age) & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),
      lower_secondary_age_girl = if_else(between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age) & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),
      upper_secondary_age_girl = if_else(between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1) & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),
      primary_minus_one_age_girl = if_else(!!rlang::sym(true_age_col) == (primary_info$starting_age - 1) & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),

      school_5_18_age_boy = if_else(between(!!rlang::sym(true_age_col), 5, 18) & !!rlang::sym(ind_gender_col) == 1, 1, 0, missing = NA_integer_),
      primary_age_boy = if_else(between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age) & !!rlang::sym(ind_gender_col) == 1, 1, 0, missing = NA_integer_),
      lower_secondary_age_boy = if_else(between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age) & !!rlang::sym(ind_gender_col) == 1, 1, 0, missing = NA_integer_),
      upper_secondary_age_boy = if_else(between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1) & !!rlang::sym(ind_gender_col) == 1, 1, 0, missing = NA_integer_),
      primary_minus_one_age_boy = if_else(!!rlang::sym(true_age_col) == (primary_info$starting_age - 1) & !!rlang::sym(ind_gender_col) == 1, 1, 0, missing = NA_integer_),
      
    )

  #------ adding indicators columns, these are the numerators needed to calculate properly the education indicators
  roster <- roster %>%
    mutate(
      school_5_18_accessing = if_else(between(!!rlang::sym(true_age_col), 5, 18)  & !!rlang::sym(education_access) == 1, 1, 0, missing = NA_integer_),
      primary_age_accessing = if_else(between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age)  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      lower_secondary_age_accessing = if_else(between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age)  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      upper_secondary_age_accessing = if_else(between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1)  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),

      school_5_18_age_accessing_girl = if_else(between(!!rlang::sym(true_age_col), 5, 18) & !!rlang::sym(ind_gender_col) == 2  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      primary_age_accessing_girl = if_else(between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age) & !!rlang::sym(ind_gender_col) == 2  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      lower_secondary_age_accessing_girl = if_else(between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age) & !!rlang::sym(ind_gender_col) == 2  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      upper_secondary_age_accessing_girl = if_else(between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1) & !!rlang::sym(ind_gender_col) == 2  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),

      school_5_18_age_accessing_boy = if_else(between(!!rlang::sym(true_age_col), 5, 18) & !!rlang::sym(ind_gender_col) == 1  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      primary_age_accessing_boy = if_else(between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age) & !!rlang::sym(ind_gender_col) == 1  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      lower_secondary_age_accessing_boy = if_else(between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age) & !!rlang::sym(ind_gender_col) == 1  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_),
      upper_secondary_age_accessing_boy = if_else(between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1) & !!rlang::sym(ind_gender_col) == 1  & !!rlang::sym(education_access_col) == 1, 1, 0, missing = NA_integer_)
    )


  roster <- roster %>%
    mutate(
      school_5_18_NON_accessing = school_5_18_age - school_5_18_accessing,
      primary_age_NON_accessing = primary_age - primary_age_accessing, 
      lower_secondary_age_NON_accessing = lower_secondary_age - lower_secondary_age_accessing,
      upper_secondary_age_NON_accessing = upper_secondary_age - upper_secondary_age_accessing,
      
      school_5_18_age_NON_accessing_girl = school_5_18_age_girl - school_5_18_age_accessing_girl,
      primary_age_NON_accessing_girl = primary_age_girl - primary_age_accessing_girl,
      lower_secondary_age_NON_accessing_girl = lower_secondary_age_girl - lower_secondary_age_accessing_girl,
      upper_secondary_age_NON_accessing_girl = upper_secondary_age_girl - upper_secondary_age_accessing_girl,
      
      school_5_18_age_NON_accessing_boy = school_5_18_age_boy - school_5_18_age_accessing_boy,
      primary_age_NON_accessing_boy = primary_age_boy - primary_age_accessing_boy,
      lower_secondary_age_NON_accessing_boy = lower_secondary_age_boy - lower_secondary_age_accessing_boy,
      upper_secondary_age_NON_accessing_boy = upper_secondary_age_boy - upper_secondary_age_accessing_boy,
      
    )
  
  
# 
#   roster <- dplyr::mutate(
#     roster,
# 
#     school_5_18_age = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_, # Keeps NA values as NA
#       !!rlang::sym(true_age_col) >= 5 & !!rlang::sym(true_age_col) <= 18 ~ 1, # Flags as 1 if age is between 5 and 18
#       TRUE ~ 0 # Flags as 0 otherwise
#     ),
#     primary_age = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age) ~ 1,
#       TRUE ~ 0
#     ),
#     lower_secondary_age = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age) ~ 1,
#       TRUE ~ 0
#     ),
#     upper_secondary_age = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1) ~ 1,
#       TRUE ~ 0
#     ),
#     primary_minus_one_age = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       !!rlang::sym(true_age_col) == (primary_info$starting_age - 1) ~ 1,
#       TRUE ~ 0
#     ),
# 
# 
#     school_5_18_age_girl = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_, # Keeps NA values as NA
#       !!rlang::sym(true_age_col) >= 5 & !!rlang::sym(true_age_col) <= 18 &  !!rlang::sym(ind_gender_col) == 2 ~ 1, # Flags as 1 if age is between 5 and 18
#       TRUE ~ 0 # Flags as 0 otherwise
#     ),
#     primary_age_girl = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age)  &  !!rlang::sym(ind_gender_col) == 2 ~ 1,
#       TRUE ~ 0
#     ),
#     lower_secondary_age_girl = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age) &  !!rlang::sym(ind_gender_col) == 2  ~ 1,
#       TRUE ~ 0
#     ),
#     upper_secondary_age_girl = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1) &  !!rlang::sym(ind_gender_col) == 2  ~ 1,
#       TRUE ~ 0
#     ),
#     primary_minus_one_age_girl = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       !!rlang::sym(true_age_col) == (primary_info$starting_age - 1) &  !!rlang::sym(ind_gender_col) == 2  ~ 1,
#       TRUE ~ 0
#     ),
# 
# 
#     school_5_18_age_boy = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_, # Keeps NA values as NA
#       !!rlang::sym(true_age_col) >= 5 & !!rlang::sym(true_age_col) <= 18 &  !!rlang::sym(ind_gender_col) == 1 ~ 1, # Flags as 1 if age is between 5 and 18
#       TRUE ~ 0 # Flags as 0 otherwise
#     ),
#     primary_age_boy = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), primary_info$starting_age, primary_info$ending_age)  &  !!rlang::sym(ind_gender_col) == 1 ~ 1,
#       TRUE ~ 0
#     ),
#     lower_secondary_age_boy = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), lower_secondary_info$starting_age, lower_secondary_info$ending_age) &  !!rlang::sym(ind_gender_col) == 1  ~ 1,
#       TRUE ~ 0
#     ),
#     upper_secondary_age_boy = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       between(!!rlang::sym(true_age_col), upper_secondary_info$starting_age, upper_secondary_info$ending_age + 1) &  !!rlang::sym(ind_gender_col) == 1  ~ 1,
#       TRUE ~ 0
#     ),
#     primary_minus_one_age_boy = dplyr::case_when(
#       is.na(!!rlang::sym(true_age_col)) ~ NA_real_,
#       !!rlang::sym(true_age_col) == (primary_info$starting_age - 1) &  !!rlang::sym(ind_gender_col) == 1  ~ 1,
#       TRUE ~ 0
#     )
# 
# 
#   )
# 
# 
# 
# 
#   
  
 
  
  return(roster)
}## end roster_education_core_function



modified_roster <- roster_education_core_function(roster, household_data,
                                                  'age_member',
                                                  'sex_member', 
                                                  'education_access',
                                                  'education_niveau',
                                                  'education_disrupted_climate',
                                                  'education_disrupted_teacher',
                                                  'education_disrupted_displaced',
                                                  'education_disrupted_occupation',
                                                  'education_barrier', 
                                                  'june',
                                                  'august',
                                                  #levels_grades,
                                                  starting_age = c(kindergarden = 4, primary = 6, `lower secondary` = 12, `upper secondary` = 17),
                                                  duration = c(kindergarden = 2, primary = 6, `lower secondary` = 5, `upper secondary` = 2)
                                                  )


                               
