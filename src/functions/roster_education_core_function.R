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
#' @param school_level_info Character vector of levels and starting age and duration separated by commas: 'kindergarden, 4, 2' ,  'primary, 6, 8' , 'lower secondary, 14, 3', 'upper secondary, 17, 1'.
#' @param start_school_year Character with the name of the month when the school year has started.
#' @param beginning_data_collection Character with the name of the month when the data collection has started.
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
    school_level_info  =  c('kindergarden, 4, 2' ,  'primary, 6, 8' , 'lower secondary, 14, 3', 'upper secondary, 17, 1'),
    start_school_year  =  'september',
    beginning_data_collection  =  'may'){
  
  
  
  
  
  
  
  
  
  
  
}## end roster_education_core_function