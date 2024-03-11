###################################################################
####         READING contextspecific/context_info.xlsx         ####
###################################################################
rm(list = ls())
source("src/functions/install_dependencies.R")

file_context_name <- 'contextspecific/context_info.xlsx'

pacman::p_load(tidyverse,hypegrammaR,rio,readxl,openxlsx,sjmisc,dplyr,tibble,tidyr,srvyr,purrr,readxl)


dataset_variables_sheet <- read_xlsx(file_context_name, sheet = "Individual variable names")
indicators_variables_sheet <- read_xlsx(file_context_name, sheet = "Question names")
school_variables_sheet <- read_xlsx(file_context_name, sheet = "School levels and grades")
