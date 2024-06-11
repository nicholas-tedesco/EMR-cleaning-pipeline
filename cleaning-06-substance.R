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

  options(dplyr.summarise.inform = FALSE)

  source('code/cleaning/cleaning-functions.R')

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
    
  ## status variables
    
    working_data <- add_substance_data('SmokingStatusSource', 'smoking.status', substance, data)
    working_data <- add_substance_data('IllegalDrugUserStatusSource', 'illegal.drug.user.status', substance, working_data)
    working_data <- add_substance_data('AlcoholUseStatusSource', 'alcohol.status', substance, working_data)
    
  ## for alcohol, add usage amount variables 
    
    working_data <- add_alcohol_data(substance, working_data)
    
    working_data <- working_data %>% 
      mutate(
        alcohol.OZ.per.week = ifelse(alcohol.status == 'Never' | is.na(alcohol.status), 0, alcohol.OZ.per.week), 
        etoh.gm = ifelse(alcohol.status == 'Never' | is.na(alcohol.status), 0, etoh.gm), 
      )

    
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-06.csv', row.names = FALSE)
    
  