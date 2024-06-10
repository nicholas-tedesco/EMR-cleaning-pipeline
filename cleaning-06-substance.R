# README -----------------------------------------------------------------------
# 
#   name: cleaning-06-substance.R
#   goal: derive substance-related variables from raw DD data
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)
  library(data.table)
  library(openxlsx)

  source('code/cleaning-functions.R')

  data = read.csv('data/working-data/data-post-05.csv')
  enc_data = load_multiple_csv('data/data-direct-exports/encounter-data', verbose = TRUE)
    
  substance = read_excel_multiple_sheets('data/data-direct-exports/steatosis-substance.xlsx')
  
  
# preprocessing ----------------------------------------------------------------
  
  ## clean date variables 
    
    enc_data$AdmitDate <- as.Date(enc_data$AdmitDate, format = "%m/%d/%Y %H:%M")
    data$date.index <- as.Date(data$date.index, format = '%Y-%m-%d')
  
  ## match encounter dates to substance records  
  
    substance <- join_enc_date(substance, enc_data)
    
    
# substance cleaning -----------------------------------------------------------
    
  ## derive smoking status 
    
    smoking_substance <- substance %>% 
      select(PatientID, EncounterID, SmokingStatusSource, AdmitDate) %>% 
      filter(!is.na(SmokingStatusSource))
      # subset substance data to non-missing smoking records only 
    
    smoking_substance <- smoking_substance %>% 
      left_join(
        data %>% select(PatientID, date.index), 
        by = 'PatientID', 
        relationship = 'many-to-many'
      ) %>% 
      mutate(datediff = abs(difftime(AdmitDate, date.index, units = 'days')))
      # join index dates, calculate difference between index and smoking encounter dates
    
    smoking_per_index <- smoking_substance %>% 
      # for each patient + index date combo...
      group_by(PatientID, date.index) %>% 
      # filter to closest smoking record relative to index date 
      filter(datediff == min(datediff)) %>% 
      # for index dates with multiple "closest" records, keep one before visit (as opposed to after)
      filter(AdmitDate == min(AdmitDate)) %>%
      select(-AdmitDate, -EncounterID) %>% 
      # get distinct rows. we may have same smoking status on multiple dates 
      distinct() %>% 
      mutate(
        SmokingStatusSourceCollapsed = case_when(
          is.na(SmokingStatusSource) ~ 'Unknown', 
          SmokingStatusSource %in% c('Never Assessed', 'Unknown If Ever Smoked', 'Unknown') ~ 'Unknown', 
          SmokingStatusSource %in% c('Never Smoker', 'Passive Smoke Exposure - Never Smoker', 'Never') ~ 'Never', 
          SmokingStatusSource %in% c('Former Smoker', 'Former') ~ 'Former', 
          SmokingStatusSource %in% c('Some Days', 'Heavy Smoker', 'Light Smoker', 'Current Some Day Smoker', 'Current Every Day Smoker', 'Light Tobacco Smoker', 'Heavy Tobacco Smoker', 'Smoker, Current Status Unknown', 'Every Day') ~ 'Current', 
          TRUE ~ 'error'
        )
      ) %>% 
      summarize(
        smoking.status.values = paste0(SmokingStatusSourceCollapsed, collapse = ' + '), 
      )
    
    smoking_final <- smoking_per_index %>% 
      mutate(
        smoking.status = case_when(
          # if only one element in list, we don't have conflicting values 
          !grepl('\\+', smoking.status.values) ~ smoking.status.values, 
          # otherwise: current > former > never > unknown
          grepl('Current', smoking.status.values) ~ 2, 
          grepl('Former', smoking.status.values) ~ 1, 
          grepl('Never', smoking.status.values) ~ 0, 
          grepl('Unknown', smoking.status.values) ~ NA, 
          TRUE ~ 'error'
        )
      ) %>% 
      select(PatientID, date.index, smoking.status) %>% 
      distinct()
    
    working_data <- data %>% 
      left_join(
        smoking_final %>% select(PatientID, date.index, smoking.status), 
        by = c('PatientID', 'date.index'), 
        relationship = 'one-to-one'
      )
      # join to working data; should be 1:1 for PatientID/Index:status
    
  ## derive illegal substance use status 
    
    illegal_substance <- substance %>% 
      select(PatientID, EncounterID, IllegalDrugUserStatusSource, AdmitDate) %>% 
      filter(!is.na(IllegalDrugUserStatusSource))
      # subset substance data to non-missing illegal drug use status records only 
    
    illegal_substance <- illegal_substance %>% 
      left_join(
        data %>% select(PatientID, date.index), 
        by = 'PatientID'
      ) %>% 
      mutate(datediff = abs(difftime(AdmitDate, date.index, units = 'days')))
      # join index dates, calculate difference between index and smoking encounter dates
    
    illegal_per_index <- illegal_substance %>% 
      # for each patient + index date combo...
      group_by(PatientID, date.index) %>% 
      # filter to closest illegal drug use record relative to index date 
      filter(datediff == min(datediff)) %>% 
      # for index dates with multiple "closest" records, keep one before visit (as opposed to after)
      filter(AdmitDate == min(AdmitDate)) %>%
      select(-AdmitDate, -EncounterID) %>% 
      # get distinct rows. we may have same illegal drug use status status on multiple dates 
      distinct() %>% 
      mutate(
        IllegalDrugUserStatusSourceCollapsed = case_when(
          IllegalDrugUserStatusSource %in% c('Not Asked', '') ~ 'Unknown', 
          IllegalDrugUserStatusSource %in% c('No', 'Never') ~ 'Never', 
          IllegalDrugUserStatusSource == 'Not Currently' ~ 'Former', 
          IllegalDrugUserStatusSource == 'Yes' ~ 'Current', 
          TRUE ~ 'error'
        )
      ) %>% 
      summarize(
        illegal.drug.user.status.values = paste0(IllegalDrugUserStatusSourceCollapsed, collapse = ' + '), 
      )
    
    illegal_final <- illegal_per_index %>% 
      mutate(
        illegal.drug.user.status = case_when(
          # if only one element in list, we don't have conflicting values 
          !grepl('\\+', illegal.drug.user.status.values) ~ illegal.drug.user.status.values, 
          # otherwise: current > former > never > unknown
          grepl('Current', illegal.drug.user.status.values) ~ 'Current', 
          grepl('Former', illegal.drug.user.status.values) ~ 'Former', 
          grepl('Never', illegal.drug.user.status.values) ~ 'Never', 
          grepl('Unknown', illegal.drug.user.status.values) ~ 'Unknown', 
          TRUE ~ 'error'
        )
      ) %>% 
      select(PatientID, date.index, illegal.drug.user.status) %>% 
      distinct()
    
    working_data <- working_data %>% 
      left_join(
        illegal_final %>% select(PatientID, date.index, illegal.drug.user.status), 
        by = c('PatientID', 'date.index')
      )
      # join to working data; should be 1:1 for PatientID/Index:status
    
  ## derive alcohol use 
    
    alcohol_substance <- substance %>% 
      select(PatientID, EncounterID, AlcoholUseStatusSource, AlcoholOZPerWeek, AdmitDate) %>% 
      filter(!is.na(AlcoholUseStatusSource))
      # subset alcohol data to non-missing alcohol records only 
    
    alcohol_substance <- alcohol_substance %>% 
      left_join(
        data %>% select(PatientID, date.index), 
        by = 'PatientID'
      ) %>% 
      mutate(datediff = abs(difftime(AdmitDate, date.index, units = 'days')))
      # join index dates, calculate difference between index and smoking encounter dates
    
    alcohol_per_index <- alcohol_substance %>% 
      # for each patient + index date combo...
      group_by(PatientID, date.index) %>% 
      # filter to closest alcohol use record relative to index date 
      filter(datediff == min(datediff)) %>% 
      # for index dates with multiple "closest" records, keep one before visit (as opposed to after)
      filter(AdmitDate == min(AdmitDate)) %>%
      select(-AdmitDate, -EncounterID) %>% 
      # get distinct rows. we may have same alcohol use status on multiple dates 
      distinct() %>% 
      mutate(
        AlcoholUseStatusCollapsed = case_when(
          AlcoholUseStatusSource %in% c('Not Asked', '') ~ 'Unknown', 
          AlcoholUseStatusSource %in% c('No', 'Never') ~ 'Never', 
          AlcoholUseStatusSource == 'Not Currently' ~ 'Former', 
          AlcoholUseStatusSource == 'Yes' ~ 'Current', 
          TRUE ~ 'error'
        )
      ) %>% 
      summarize(
        alcohol.status.values = paste0(AlcoholUseStatusCollapsed, collapse = ' + '), 
      )
    
    alcohol_final <- alcohol_per_index %>% 
      mutate(
        alcohol.status = case_when(
          # if only one element in list, we don't have conflicting values 
          !grepl('\\+', alcohol.status.values) ~ alcohol.status.values, 
          # otherwise: current > former > never > unknown
          grepl('Current', alcohol.status.values) ~ 'Current', 
          grepl('Former', alcohol.status.values) ~ 'Former', 
          grepl('Never', alcohol.status.values) ~ 'Never', 
          grepl('Unknown', alcohol.status.values) ~ 'Unknown', 
          TRUE ~ 'error'
        )
      ) %>% 
      select(PatientID, date.index, alcohol.status) %>% 
      distinct()
    
    alcohol_current <- alcohol_substance %>% 
      # for each patient + index date combo... 
      group_by(PatientID, date.index) %>% 
      # filter to alcohol use records within one year 
      filter(datediff < 365.25) %>% 
      # remove records with no alcohol OZ value 
      filter(!is.na(AlcoholOZPerWeek)) %>% 
      # fix records that have range instead of single value
      mutate(
        num1 = case_when(
          grepl('\\-', AlcoholOZPerWeek) ~ gsub('\\-.*', '', AlcoholOZPerWeek), 
          TRUE ~ AlcoholOZPerWeek
        ) %>% as.numeric(), 
        num2 = case_when(
          grepl('\\-', AlcoholOZPerWeek) ~ gsub('.*\\-', '', AlcoholOZPerWeek), 
          TRUE ~ AlcoholOZPerWeek
        ) %>% as.numeric(), 
        FixedAlcoholOZPerWeek = mean(c(num1, num2))
      ) %>% 
      # for each patient/index date, take average of alcohol OZ records
      group_by(PatientID, date.index) %>% 
      summarize(
        alcohol.OZ.per.week.average = mean(FixedAlcoholOZPerWeek)
      ) %>% 
      mutate(
        etoh.gm = alcohol.OZ.per.week.average * 23.33/7
      )
    
    alcohol_final <- alcohol_final %>% 
      left_join(alcohol_current, by = c('PatientID', 'date.index')) %>% 
      mutate(
        alcohol.OZ.per.week.average = ifelse(alcohol.status == 'Current', alcohol.OZ.per.week.average, NA), 
        etoh.gm = ifelse(alcohol.status == 'Current', alcohol.OZ.per.week.average, NA), 
      )
    
    
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-06.csv', row.names = FALSE)
    
  