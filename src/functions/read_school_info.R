library(readxl)

read_school_level_grade_age <- function(file_path, country_input) {
  # Read the Excel file
  df <- readxl::read_excel(file_path, sheet = "Compiled_Levels_Grades")
  
  # Convert the country input and dataframe columns to lowercase for case-insensitive comparison
  country_input_lower <- tolower(country_input)
  
  # Check if the country exists in the dataframe
  if(sum(tolower(df$`country code`) == country_input_lower | tolower(df$country) == country_input_lower) == 0){
    warning(sprintf("The country '%s' does not exist in the dataset.", country_input))
    return(NULL)
  }
  
  # Filter data for the specified country by code or name, case-insensitive
  country_df <- dplyr::filter(df, tolower(`country code`) == country_input_lower | tolower(country) == country_input_lower)
  
  # DataFrame 1: level code, Learning Level, starting age, duration
  df1 <- country_df %>%
    dplyr::group_by(`level code`, `learning level`) %>%
    dplyr::summarise(starting_age = min(`theoretical start age`),
                     duration = dplyr::n(),
                     .groups = 'drop')
  
  # Adjust for level0 duration if both level0 and level1 exist
  if ("level0" %in% df1$`level code` && "level1" %in% df1$`level code`) {
    starting_age_level0 <- df1$starting_age[df1$`level code` == "level0"]
    starting_age_level1 <- df1$starting_age[df1$`level code` == "level1"]
    duration_level0 <- starting_age_level1 - starting_age_level0
    
    df1 <- df1 %>%
      mutate(duration = ifelse(`level code` == "level0", duration_level0, duration))
  }
  
  # Correctly adjust limit_age to be starting_age + 2 for all entries
  country_df <- country_df %>%
    dplyr::mutate(limit_age = `theoretical start age` + 2)
  
  # DataFrame 2: level code, Learning Level, Year/Grade, Theoretical Start age, limit age
  df2 <- country_df %>%
    dplyr::select(`level code`, `learning level`, `year-grade`, `theoretical start age`, limit_age, `name -- for kobo`)
  
  df2 <- df2 %>%
    rename(
      level_code = `level code`,
      name_level = `learning level`,
      starting_age = `theoretical start age`,
      name_level_grade = `name -- for kobo`,
      grade = `year-grade`
    )
  
  return(list(df1 = df1, df2 = df2))
} # Closing bracket for the function




#------------------------------------------------ Function to create the levels dataframe
create_levels_df <- function(school_sheet, num_levels_row, num_levels) {
  # Initialize an empty data frame with the appropriate structure
  levels_df <- data.frame(
    level = character(num_levels),
    starting_age = numeric(num_levels),
    duration = numeric(num_levels),
    stringsAsFactors = FALSE
  )
  
  # Populate the new data frame
  for (i in 1:num_levels) {
    # The row where the current level information should be located
    level_info_row <- num_levels_row + i
    # Extract the level information as a character string
    level_info <- as.character(school_sheet[level_info_row, "country variable name"])
    
    # Ensure commas have spaces after them for correct splitting, without removing spaces within level names
    level_info <- gsub(",", ", ", level_info)
    level_info <- gsub(",  ", ", ", level_info) # Correct double spaces that may occur after adding spaces after commas
    
    # Check for non-NA values and split the string into components
    if (!is.na(level_info) && level_info != "NA" && level_info != "na") {
      level_details <- strsplit(level_info, ", ")[[1]]
      
      # Fill in the details for the current level
      levels_df[i, "level"] <- level_details[1]
      
      # Check if starting_age and duration information is available and numeric
      if(length(level_details) >= 3) {
        levels_df[i, "starting_age"] <- as.numeric(level_details[2])
        levels_df[i, "duration"] <- as.numeric(level_details[3])
      }
    } else {
      warning(paste("You stated that there are", num_levels, 
                    "levels, but did not provide complete information for level", i))
    }
  }
  return(levels_df)
}


