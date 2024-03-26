
#------------------------------------------------ Function to Replace yes/no with 0 and 1
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
#--------------------------------------------------------------------------------------------------------

#------------------------------------------------ Function to Replace all the different gender labels with 1 == MALE and 2 == FEMALE
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
#--------------------------------------------------------------------------------------------------------

#------------------------------------------------ Function to Ensure Continuous Age Ranges Between Levels
validate_age_continuity_and_levels <- function(school_level_infos, required_levels) {
  # Flatten the list into a single data frame for easier manipulation
  all_levels_df <- bind_rows(school_level_infos, .id = "level")
  
  # Check for missing levels
  existing_levels <- unique(all_levels_df$level)
  missing_levels <- setdiff(required_levels, existing_levels)
  if (length(missing_levels) > 0) {
    stop(sprintf("Missing required educational levels: %s", paste(missing_levels, collapse=", ")), call. = FALSE)
  }
  
  # Sorting might be necessary depending on your data
  all_levels_df <- all_levels_df %>% arrange(starting_age)
  
  # Continue with the continuity check
  for (i in 2:nrow(all_levels_df)) {
    if (all_levels_df$starting_age[i] != all_levels_df$ending_age[i-1] + 1) {
      stop(sprintf("Age range discontinuity between levels: %s ends at %d, but %s starts at %d", 
                   all_levels_df$level[i-1], all_levels_df$ending_age[i-1], 
                   all_levels_df$level[i], all_levels_df$starting_age[i]), call. = FALSE)
    }
  }
  
  return(TRUE)
}

#--------------------------------------------------------------------------------------------------------


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

# Define a helper function to safely rename columns if they exist
safe_rename <- function(dataframe, old_name, new_name) {
  if(old_name %in% names(dataframe)) {
    dataframe <- rename(dataframe, !!new_name := !!sym(old_name))
  }
  return(dataframe)
}