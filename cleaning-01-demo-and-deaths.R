# README -----------------------------------------------------------------------
#
#   name: cleaning-01-demo.R
#   goal: clean + merge data related to demographics for steatosis cohort
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)
  library(openxlsx)

  data = read.csv('data/original-data/steato-all-bystudy-2024-05-13.csv')
  
  ids   = read.xlsx('data/data-direct-exports/steatosis-demo.xlsx', sheet=2)
  demo  = read.xlsx('data/data-direct-exports/steatosis-demo.xlsx', sheet=3)
  death = read.xlsx('data/data-direct-exports/steatosis-deaths.xlsx', sheet=2)
  
  
# functions --------------------------------------------------------------------
  
  add_leading_zeroes <- function(MRN) {
    
    total_length = 9
    current_size = nchar(MRN)
    
    num_zeroes = total_length - current_size 
    
    formatted_MRN = paste0(paste(rep('0', num_zeroes), collapse = ''), MRN)
    
    return(formatted_MRN)
    
  }
  
  
# preprocessing ----------------------------------------------------------------
  
  ## fix MRN formatting 
  
    working_data <- data %>% 
      mutate(MRN = sapply(MRN, add_leading_zeroes))
  
  ## fix date columns 
  
    working_data$date     <- as.Date(working_data$date, format = "%Y-%m-%d")
    ids$DOB               <- as.Date(ids$DOB, format = "%m/%d/%Y %H:%M")
    death$MDIDeceasedDate <- as.Date(death$MDIDeceasedDate, format = '%m/%d/%Y %H:%M')
    
    
# demographics -----------------------------------------------------------------
  
  ## join ids and demographics
  
    working_data <- working_data %>% 
      left_join(ids, by = 'MRN') %>% 
      left_join(demo, by = 'PatientID')
    
  ## engineer race / ethnicity 
    
    working_data <- working_data %>% 
      mutate(
        race.ethnicity = case_when(
          EthnicityCode == 'HL' ~ 'H', 
          RaceCode == 'C' ~ 'C', 
          RaceCode == 'AA' ~ 'Af', 
          RaceCode == 'A' ~ 'As', 
          TRUE ~ 'O'
        )
      )
    
  ## engineer age 
    
    working_data <- working_data %>% 
      mutate(
        age.at.index = difftime(date, DOB, units = 'days') %>% as.numeric() / 365.25
      )
    
  ## recode sex 
    
    working_data <- working_data %>% 
      mutate(
        sex = case_when(
          GenderCode == 'M' ~ 0, 
          GenderCode == 'F' ~ 1, 
          is.na(GenderCode) | GenderCode == 'U' ~ NA
        )
      )
    
    
# death data -------------------------------------------------------------------
    
  ## remove records with missing death date 
    
    death <- death %>% filter(!is.na(MDIDeceasedDate))
    
  ## some patients have duplicate deaths? keep earlier record
    
    duplicate_death <- death %>% filter(duplicated(PatientID) | duplicated(PatientID, fromLast = TRUE))
    
    death <- death %>% 
      group_by(PatientID) %>% 
      filter(MDIDeceasedDate == min(MDIDeceasedDate, na.rm = TRUE))
    
  ## join death data
    
    working_data <- working_data %>% 
      left_join(death, by = 'PatientID')
    
    
# final cleaning ---------------------------------------------------------------
    
  ## reorder + subset columns 
    
    working_data <- working_data %>% 
      select(
        PatientID, MRN, type, date.index = date, date.birth = DOB, age.at.index, sex, 
        race.ethnicity, LSM.v:CAP, date.death = MDIDeceasedDate, UnderlyingCOD_ICD10:RelatedCOD7_ICD10
      )
    
    
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-01.csv', row.names = FALSE)
    
  