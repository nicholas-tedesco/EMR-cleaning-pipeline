# README -----------------------------------------------------------------------
# 
#   name: cleaning-05-bmi.R
#   goal: import + process BMI data from DD, then join to working data
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)
  library(data.table)

  source('code/cleaning-functions.R')
  
  data = read.csv('data/working-data/data-post-04.csv')
  bmi_data = load_multiple_csv('data/data-direct-exports/bmi-data', verbose = TRUE)
  enc_data = load_multiple_csv('data/data-direct-exports/encounter-data', verbose = TRUE)
    
    
# preprocessing ----------------------------------------------------------------
    
  ## fix date types 
    
    enc_data$AdmitDate <- as.Date(enc_data$AdmitDate, format = "%m/%d/%Y %H:%M")
    data$date.index <- as.Date(data$date.index, format = '%Y-%m-%d')
    
  ## remove missing + implausible values 
    
    bmi_data <- bmi_data[!is.na(bmi_data$BMI), ]
    bmi_data <- bmi_data[bmi_data$BMI >= 15, ]
    bmi_data <- bmi_data[bmi_data$BMI <= 100, ]
    
  ## join dates to BMI data 
    
    bmi_data <- join_enc_date(bmi_data, enc_data)
  
  
# clean + join BMI data --------------------------------------------------------
    
  ## for each patient, remove BMI values > 3SD from median
    
    bmi_stats <- bmi_data %>% 
      group_by(PatientID) %>% 
      summarize(
        median_BMI = median(BMI, na.rm = TRUE), 
        sd_BMI = sd(BMI, na.rm = TRUE)
      ) %>% 
      mutate(sd_BMI = ifelse(is.na(sd_BMI), 0, sd_BMI))
    
    filtered_bmi <- bmi_data %>% 
      left_join(bmi_stats, by = 'PatientID') %>% 
      mutate(
        median_BMI_diff = abs(BMI - median_BMI), 
        remove_flag = ifelse(median_BMI_diff > 3 * sd_BMI, 1, 0)
      ) %>% 
      filter(remove_flag == 0)
    
  ## for each patient, get median BMI +/- one year from index date 
    
    bmi_index <- filtered_bmi %>% 
      left_join(
        data %>% select(PatientID, date.index) %>% distinct(), 
        by = 'PatientID', 
        relationship = 'many-to-many'
      ) %>% 
      mutate(
        datediff = abs(difftime(AdmitDate, date.index, units = 'days')), 
        year_remove_flag = ifelse(datediff > 365.25, 1, 0)
      ) %>% 
      filter(year_remove_flag == 0) %>% 
      group_by(PatientID, date.index) %>% 
      summarize(
        final_BMI = median(BMI, na.rm = TRUE)
      )
    
  ## join to working data 
    
    working_data <- data %>% 
      left_join(bmi_index, by = c('PatientID', 'date.index'))
    
    
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-05.csv', row.names = FALSE)
      
       
    