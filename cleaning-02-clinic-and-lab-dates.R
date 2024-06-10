# README -----------------------------------------------------------------------
# 
#   name: cleaning-02-dates.R
#   goal: gather remaining dates needed for final dataset, including: 
#         - clinic dates: first/last clinic visit on file 
#         - "any" dates:  first/last clinic OR lab visit on file 
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)
  library(openxlsx)
  library(data.table)
  library(glue)

  source('code/cleaning-functions.R')

  working_data = read.csv(file = 'data/working-data/data-post-01.csv')
  enc_data = load_multiple_csv('data/data-direct-exports/encounter-data', verbose = TRUE)
  
  lab_data = data.frame()
  labs = list.files('data/data-direct-exports/labs')
  for(lab in labs) {
    
    temp = load_multiple_csv(glue('data/data-direct-exports/labs/{lab}'))
    lab_data = rbind(lab_data, temp)
    
    message(glue('Finished loading {lab}: {nrow(temp)} rows'))
    
  }

  
# get clinic dates -------------------------------------------------------------
  
  ## filter to clinic visits
    
    clinic_enc_types <- c('VISIT', 'HOSP ENC', 'Virtual Vst', 'Video Visit', 'RADONC OTV', 'EConsult', 'POSTPARTUM V')
    clinic_data <- enc_data %>% 
      filter(EncounterTypeCode %in% clinic_enc_types)
    
  ## fix date type 
    
    clinic_data$AdmitDate <- as.Date(clinic_data$AdmitDate, format = '%m/%d/%Y %H:%M')
    lab_data$COLLECTION_DATE <- as.Date(lab_data$COLLECTION_DATE, format = '%m/%d/%Y %H:%M')
    
  ## for each patient, get first and last clinic visit
  
    clinic_dates <- clinic_data %>% 
      group_by(PatientID) %>% 
      summarize(
        date.first.clinic = min(AdmitDate, na.rm = TRUE), 
        date.last.clinic = max(AdmitDate, na.rm = TRUE)
      )
    
  ## for each patient, get first and last lab visit
    
    lab_dates <- lab_data %>% 
      group_by(PatientID) %>% 
      summarize(
        date.first.lab = min(COLLECTION_DATE, na.rm = TRUE), 
        date.last.lab = max(COLLECTION_DATE, na.rm = TRUE)
      )
    
    any_dates <- lab_dates %>% 
      full_join(clinic_dates, by = c('PatientID')) %>% 
      rowwise() %>% 
      mutate(
        date.first.any = min(date.first.clinic, date.first.lab, na.rm = TRUE), 
        date.last.any = max(date.last.clinic, date.last.lab, na.rm = TRUE)
      )
    
  ## bind to working data 
    
    working_data <- working_data %>% 
      left_join(clinic_dates, by = 'PatientID') %>% 
      left_join(any_dates %>% select(PatientID, contains('any')), by = 'PatientID')

  
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data_post_02.csv', row.names = FALSE)
    
    