#' @title Prepare dummy variables for education indicators (individual data)
#'
#' @param roster A data frame of individual-level data.
#' @param household_data A data frame of hh-level data.
#' @param ind_age The individual age column.
#' @param ind_gender The individual gender column.
#' @param education_access The individual access indicator column.
#' @param education_level_grade The individual level and grade column.
#' @param education_disrupted_climate The education disrupted by climate-related hazards such as flood, cyclone, drought or wildfire  column.
#' @param education_disrupted_teacher The education disrupted by teacher's absence column.
#' @param education_disrupted_displaced The education disrupted by school used as a shelter by displaced persons column.
#' @param education_disrupted_occupation The education disrupted by school occupied by armed forces/ non-state armed groups column.
#' @param education_barrier the barrier column, select one, only for individual analysis.
#' 
#' @param start_school_year Character with the name of the month when the school year has started.
#' @param beginning_data_collection Character with the name of the month when the data collection has started.
#' @param school_level_info dataframe: 'kindergarden, 4, 2' ,  'primary, 6, 8' , 'lower secondary, 14, 3', 'upper secondary, 17, 1'.

#' 
#' @return X new columns which are essential for the calculation of education indicators
#' @export


roster_education_core_function <- function(
    roster,
    household_data,
    ind_age  =  'ind_age',
    ind_gender  =  'ind_gender',
    education_access  =  'education_access',
    education_level_grade  =  'education_level_grade',
    education_disrupted_climate  =  'education_disrupted_climate',
    education_disrupted_teacher  =  'education_disrupted_teacher',
    education_disrupted_displaced  =  'education_disrupted_displaced',
    education_disrupted_occupation  =  'education_disrupted_occupation',
    education_barrier  =  'education_barrier',
    start_school_year  =  'september',
    beginning_data_collection  =  'may', 
    school_level_info  =  data.frame(
      level = c ('kindergarden', 'primary', 'lower secondary', 'upper secondary'),
      starting_age = c (4,6,14,17),
      duration = c (2,8,3,1))){
  
  #------ Enquos
  edu_cols <- rlang::enquos(education_access, education_level_grade, education_disrupted_climate, education_disrupted_teacher, education_disrupted_displaced, education_disrupted_occupation, education_barrier)
  edu_cols <- purrr::map_chr(edu_cols, rlang::as_name)
  
  ind_age_col <- rlang::as_name(rlang::enquo(ind_age)) 
  ind_gender_col <- rlang::as_name(rlang::enquo(ind_gender)) 
  
  #------ Check if in_age exist
  if_not_in_stop(roster, "ind_age_col", "roster")
  
  #------ Are level_codes in set
  are_values_in_set(roster, edu_cols)
  
  
  
  
  
  
  
    month_lookup <- setNames(seq(1, 12), tolower(substr(month.name, 1, 3)))
  
    age_correction  <- calculate_age_correction(start_school_year, beginning_data_collection)
    

  
    #------------------------------------------------ Function to determine if age correction should be applied
    calculate_age_correction <- function(start_month, collection_month) {
      # Convert month names to their numeric equivalents using the predefined lookup
      start_month_num <- month_lookup[tolower(substr(trimws(start_month), 1, 3))]
      collection_month_num <- month_lookup[tolower(substr(trimws(collection_month), 1, 3))]
      
      # Adjust the start month number for a school year starting in the previous calendar year
      adjusted_start_month_num <- if(start_month_num > 6) start_month_num - 12 else start_month_num
      
      # Determine if the age correction should be applied based on the month difference
      age_correction <- (collection_month_num - adjusted_start_month_num) > 6
      return(age_correction)
    }
    
  
}## end roster_education_core_function