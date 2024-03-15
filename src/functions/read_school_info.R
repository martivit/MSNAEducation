library(readxl)



#------------------------------------------------ Function to determine if age correction should be applied
calculate_age_correction <- function(start_month, collection_month) {
  month_lookup <- setNames(seq(1, 12), tolower(substr(month.name, 1, 3)))
  
  # Convert month names to their numeric equivalents using the predefined lookup
  start_month_num <- month_lookup[tolower(substr(trimws(start_month), 1, 3))]
  collection_month_num <- month_lookup[tolower(substr(trimws(collection_month), 1, 3))]
  
  # Adjust the start month number for a school year starting in the previous calendar year
  adjusted_start_month_num <- if(start_month_num > 6) start_month_num - 12 else start_month_num
  
  # Determine if the age correction should be applied based on the month difference
  age_correction <- (collection_month_num - adjusted_start_month_num) > 6
  return(unname(age_correction))
}
#--------------------------------------------------------------------------------------------------------


###### needed functions ---> 
modify_column_value_yes_no <- function(data, target_cols) {
  # Ensure target_cols is a character vector
  target_cols <- as.character(target_cols)
  
  # Check if target_cols exist in data
  missing_cols <- target_cols[!target_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    warning("The following columns were not found in data: ", paste(missing_cols, collapse = ", "), ". No changes made.")
    return(data)
  }
  
  # Process each target column
  data <- data %>%
    mutate(across(all_of(target_cols), 
                  ~ case_when(
                    is.na(.) ~ NA_character_,
                    . %in% c("yes", "Yes", "oui") ~ "1",
                    . %in% c("no", "No", "non") ~ "0",
                    . %in% c("dnk",'idk', "nsp", "dont_know", "no_answer") ~ "50",
                    . %in% c("pnta", "pnpr", "prefer_not_to_answer") ~ "99",
                    TRUE ~ as.character(.)
                  ))) %>%
    mutate(across(all_of(target_cols), as.numeric))
  
  return(data)
}



# Replace all the different gender labels with 1 == MALE and 2 == FEMALE
modify_gender_values <- function(data, target_col) {
  # Check if target_col exists in data
  if (!target_col %in% names(data)) {
    warning(paste("Column", target_col, "not found in data. No changes made."), call. = FALSE)
    return(data)
  }
  
  # Replace gender labels with standardized numeric codes
  data <- data %>%
    mutate(!!target_col := case_when(
      is.na(.data[[target_col]]) ~ NA_integer_, # Keeps NA values as NA
      tolower(.data[[target_col]]) %in% c("femme", "féminin", "feminin", "female", "woman", "girl", "2") ~ 2L,
      tolower(.data[[target_col]]) %in% c("homme", "masculin", "masculin", "male", "man", "boy", "1") ~ 1L,
      .data[[target_col]] %in% c("no_answer") ~ 50L,
      .data[[target_col]] %in% c("other", "prefer_not_to_answer") ~ 99L,
      TRUE ~ NA_integer_ # If none of the above conditions are met, set to NA
    ))
  
  return(data)
}


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


#--------------------------------------------------------------------------------------------------------