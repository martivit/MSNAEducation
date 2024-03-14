library(readxl)


#------------------------------------------------ Function to determine if age correction should be applied
calculate_age_correction <- function(start_month, collection_month) {
  # Convert month names to lowercase for comparison
  start_month <- tolower(trimws(start_month))
  collection_month <- tolower(trimws(collection_month))
  
  # Match the provided month names to their numeric equivalent
  start_month_num <- match(substr(start_month, 1, 3), tolower(substr(month.name, 1, 3)))
  collection_month_num <- match(substr(collection_month, 1, 3), tolower(substr(month.name, 1, 3)))
  
  # Adjust the start month number for a school year starting in the previous calendar year
  adjusted_start_month_num <- ifelse(start_month_num > 6, start_month_num - 12, start_month_num)
  
  # Calculate the difference in months
  month_difference <- collection_month_num - adjusted_start_month_num
  
  # Determine if the age correction should be applied
  age_correction <- month_difference > 6
  return(age_correction)
}
#--------------------------------------------------------------------------------------------------------

#------------------------------------------------ Function to create the levels dataframe
create_levels_df <- function(school_sheet, num_levels_row, num_levels) {
  # Initialize an empty data frame with the appropriate structure
  levels_df <- data.frame(
    level = character(num_levels),
    level_name = character(num_levels),
    entrance = numeric(num_levels),
    duration = numeric(num_levels),
    stringsAsFactors = FALSE
  )
  
  # Populate the new data frame
  for (i in 1:num_levels) {
    # The row where the current level information should be located
    level_info_row <- num_levels_row + i
    # Extract the level information as a character string
    level_info <- as.character(school_sheet[level_info_row, "country variable name"])
    
    # Clean the string to ensure uniform formatting
    level_info <- gsub(" ", "", level_info)
    level_info <- gsub(",", ", ", level_info)
    
    # Check for non-NA values and split the string into components
    if (!is.na(level_info) && level_info != "NA" && level_info != "na") {
      level_details <- strsplit(level_info, ", ")[[1]]
      
      # Fill in the details for the current level
      levels_df[i, "level"] <- paste0("level", i)
      levels_df[i, "level_name"] <- level_details[1]
      
      # Check if entrance and duration information is available and numeric
      if(length(level_details) >= 3) {
        levels_df[i, "entrance"] <- as.numeric(level_details[2])
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