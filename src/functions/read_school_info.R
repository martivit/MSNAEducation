library(readxl)




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