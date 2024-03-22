rm(list = ls())
source("src/functions/install_dependencies.R")
source("src/functions/read_school_info.R")
source("src/functions/internals.R")
source("src/functions/education_internals.R")


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
file_school_cycle <- "contextspecific/UNESCO ISCED Mappings_MSNAcountries_consolidated.xlsx"
country_assessment <- "SYR" # Can input either country code or name, case-insensitive


info_country_school_structure <- read_school_level_grade_age(file_school_cycle, country_assessment)

summary_school_levels <- info_country_school_structure$df1    # DataFrame 1: level code, Learning Level, starting age, duration
levels_grades_age_ranges <-info_country_school_structure$df2  # DataFrame 2: level code, Learning Level, Year/Grade, Theoretical Start age, limit age

print(summary_school_levels)
print(levels_grades_age_ranges)



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
    summary_info_school = summary_school_levels,    # level code, Learning Level, starting age, duration
    levels_grades_ages = levels_grades_age_ranges   # level code, Learning Level, Year/Grade, Theoretical Start age, limit age
) {
  

  
  #------ Enquos and checks
  edu_cols <- rlang::enquos(education_access, education_disrupted_climate, education_disrupted_teacher, education_disrupted_displaced, education_disrupted_occupation)
  edu_cols <- purrr::map_chr(edu_cols, rlang::as_name)
  
  ind_age_col <- rlang::as_name(rlang::enquo(ind_age)) 
  ind_gender_col <- rlang::as_name(rlang::enquo(ind_gender)) 
  
  #-- Check if in_age exist
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
  # TRUE --> more than 6 months difference between start of the school and data collection
  # TRUE --> new age columns with (ind_age - 1)
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
  education_level_grades_col <- education_level_grade

 
  ## ------ Modify the data set to have clear level and grade definition and the recorded limit for the matching ages
  roster <- roster %>%
    rename(!!"name_level_grade" := !!sym(education_level_grades_col))
  

  #roster_enhanced <- left_join(roster, dataframe_levels_grades_ages, by = "name_level_grade")
  roster <- left_join(roster, levels_grades_ages, by = "name_level_grade") %>%
    select(uuid, person_id, everything(), -name_level_grade)


  #Adjusting level_code, name_level, and grade Based on education_access
  roster <- roster %>%
    mutate(across(c(level_code, name_level, grade), ~if_else(is.na(education_access) | education_access == 0, NA_character_, .)))
  
  # reducing the dataset to contain only school-aged children
  roster <- roster %>%
    filter(between(!!rlang::sym(true_age_col), 5, 18))
  
  
  #------ Dynamically create info data frames for each school level based on the number of levels
  school_level_infos <- list()

  # Extract unique level codes sorted if needed
  unique_levels <- sort(unique(summary_school_levels$`level code`))

  for (level_code in unique_levels) {
    # Extract relevant information for each level
    level_info <- summary_school_levels %>%
      filter(`level code` == level_code) %>%
      summarise(starting_age = min(starting_age),
                ending_age = max(starting_age) + max(duration) - 1) %>%
      mutate(level = level_code) %>%
      select(level, starting_age, ending_age)

    # Store the extracted info in a list
    school_level_infos[[level_code]] <- level_info
  }


  # Ensure continuous age ranges between levels and all levels being present
  validate_age_continuity_and_levels(school_level_infos, unique_levels)


  roster <- roster %>%
    mutate(
      school_5_18_age = if_else(between(!!rlang::sym(true_age_col), 5, 18), 1, 0, missing = NA_integer_),
      # For gender-specific calculations, add the gender condition to the logical vector
      school_5_18_age_girl = if_else(between(!!rlang::sym(true_age_col), 5, 18) & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),
      school_5_18_age_boy = if_else(between(!!rlang::sym(true_age_col), 5, 18) & !!rlang::sym(ind_gender_col) == 1, 1, 0, missing = NA_integer_)
    )
  

  filtered_levels <- unique_levels[-1]
  
  
  for (level in filtered_levels) {
    
    accessing_level_col_name <- paste0(level, "_accessing")
    roster[[accessing_level_col_name]]  <-  if_else(roster[['level_code']] == level, 1, 0, missing = NA_integer_)
    
    genders <- c("girl" = 2, "boy" = 1)
    for (gender in names(genders)) {
      gender_val <- genders[gender]
      accessing_level_gender_col_name <- paste0(accessing_level_col_name, "_", gender)
      roster[[accessing_level_gender_col_name]]  <-  if_else(roster[['level_code']] == level & roster[[ind_gender_col]] == gender_val, 1, 0, missing = NA_integer_)
    }
  }
  
#level_code == level

  for (level in filtered_levels) {
    # Extract info for current level
    starting_age <- as.numeric(school_level_infos[[level]]$starting_age)
    ending_age <- as.numeric(school_level_infos[[level]]$ending_age)
    # if (level == tail(filtered_levels, n = 1)) {
    #   ending_age <- ending_age + 1  # Adjust for last level
    # }

    # Define dynamic column names
    age_col_name <- paste0(level, "_age")
    age_accessing_col_name <- paste0(level, "_age_accessing")
    age_non_accessing_col_name <- paste0(level, "_NON_accessing")
    
    roster[[age_col_name]] <- ifelse(roster[[true_age_col]] >= starting_age & roster[[true_age_col]] <= ending_age, 1, 0)
    roster[[age_accessing_col_name]] <- ifelse(roster[[age_col_name]] == 1 & roster[[education_access_col]] == 1, 1, 0)
    roster[[age_non_accessing_col_name]] <- roster[[age_col_name]] - roster[[age_accessing_col_name]]
  
    genders <- c("girl" = 2, "boy" = 1)
    for (gender in names(genders)) {
      gender_val <- genders[gender]
      age_gender_col_name <- paste0(age_col_name, "_", gender)
      accessing_gender_col_name <- paste0(age_accessing_col_name, "_", gender)
      non_accessing_gender_col_name <- paste0(age_non_accessing_col_name, "_", gender)
      
      # Direct assignment for gender-specific conditions
      roster[[age_gender_col_name]] <- ifelse( roster[[true_age_col]]>= starting_age &  roster[[true_age_col]] <= ending_age & roster[[ind_gender_col]] == gender_val, 1, 0)
      roster[[accessing_gender_col_name]] <- ifelse(roster[[age_gender_col_name]] == 1 & roster[[education_access_col]] == 1, 1, 0)
      roster[[non_accessing_gender_col_name]] <- roster[[age_gender_col_name]] - roster[[accessing_gender_col_name]]
    }
  }

  
  # adding the indicators for the single year before of the starting of the primary school
  roster <- roster %>%
    mutate(
      level1_minus_one_age = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1), 1, 0, missing = NA_integer_),
      level1_minus_one_age_girl = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1) & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),
      level1_minus_one_age_boy = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1) & !!rlang::sym(ind_gender_col) == 1, 1, 0, missing = NA_integer_),

      # Participation rate in organised learning
      attending_level0_level1_and_level1_minus_one_age = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1)
                                            & (!!rlang::sym('level_code') == 'level0' | !!rlang::sym('level_code') == 'level1'),
                                            1, 0, missing = NA_integer_),
      # for early enrolment
      attending_level1_and_level1_minus_one_age = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1)
                                                                 & !!rlang::sym('level_code') == 'level1',
                                                                 1, 0, missing = NA_integer_),
      
      attending_level0_level1_and_level1_minus_one_age_girl = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1) & (!!rlang::sym('level_code') == 'level0' | !!rlang::sym('level_code') == 'level1') & !!rlang::sym(ind_gender_col) == 2, 1, 0, missing = NA_integer_),
      attending_level1_and_level1_minus_one_age_girl = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1)  & !!rlang::sym('level_code') == 'level1' & !!rlang::sym(ind_gender_col) == 2,  1, 0, missing = NA_integer_),
      attending_level0_level1_and_level1_minus_one_age_boy = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1) & (!!rlang::sym('level_code') == 'level0' | !!rlang::sym('level_code') == 'level1')& !!rlang::sym(ind_gender_col) == 1,  1, 0, missing = NA_integer_),
      attending_level1_and_level1_minus_one_age_boy = if_else(!!rlang::sym(true_age_col) == (school_level_infos[['level1']]$starting_age - 1) & !!rlang::sym('level_code') == 'level1'& !!rlang::sym(ind_gender_col) == 1,1, 0, missing = NA_integer_)
    )
  
  
  # adding the indicators for Net attendance rate (adjusted)
  level_numeric <- seq_along(filtered_levels)
  names(level_numeric) <- filtered_levels

  for (level in filtered_levels) {
    starting_age <- as.numeric(school_level_infos[[level]]$starting_age)
    ending_age <- as.numeric(school_level_infos[[level]]$ending_age)
    #
    higher_levels_numeric <- gsub("level", "",  paste(filtered_levels[which(filtered_levels >= level)], collapse = ""))
    attending_col_name <- paste0("attending_level", higher_levels_numeric, "_and_", level, "_age")
    #
    roster[[attending_col_name]] <- ifelse(  roster[[true_age_col]] >= starting_age & roster[[true_age_col]] <= ending_age & as.integer(as.factor(roster$level_code)) >= as.integer(as.factor(level)),1, 0 )   
    
    genders <- c("girl" = 2, "boy" = 1)
    for (gender in names(genders)) {
      gender_val <- genders[gender]
      attending_gender_col_name <- paste0(attending_col_name, "_", gender)
      # Direct assignment for gender-specific conditions
      roster[[attending_gender_col_name]] <- ifelse(  roster[[true_age_col]] >= starting_age & roster[[true_age_col]] <= ending_age & as.integer(as.factor(roster$level_code)) >= as.integer(as.factor(level)) & roster[[ind_gender_col]] == gender_val,1, 0 )   
    }
    
  }
  
  # overage learners
  for (level in filtered_levels) {
    
    overage_level_col_name <- paste0(level, "_overage_learners")
    roster[[overage_level_col_name]] <- ifelse(  roster[['level_code']] == level  & (roster[[true_age_col]] - roster[['limit_age']]) >= 2, 1, 0)
    genders <- c("girl" = 2, "boy" = 1)
    for (gender in names(genders)) {
      gender_val <- genders[gender]
      overage_level_gender_col_name <- paste0(level, "_overage_learners", "_", gender)
      
      roster[[overage_level_gender_col_name]] <- ifelse(  roster[['level_code']] == level  & (roster[[true_age_col]] - roster[['limit_age']]) >= 2  & roster[[ind_gender_col]] == gender_val, 1, 0)
      
    }
  }
  
  
  
  

  
 
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
                                                  start_school_year = 'september',
                                                  beginning_data_collection = 'may',
                                                  summary_school_levels,
                                                  levels_grades_age_ranges
                                                  )


                               
