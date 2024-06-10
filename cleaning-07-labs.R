# README -----------------------------------------------------------------------
# 
#   file: cleaning-07-labs.R
#   goal: derive lab variables from raw DD data, merge to working data 
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------
  
  library(openxlsx)
  library(tidyverse)
  library(glue)
  library(hash)

  options(dplyr.summarise.inform = FALSE)

  source('code/cleaning-functions.R')

  data = read.csv('data/working-data/data-post-06.csv')
  
  labs = list.files('data/data-direct-exports/labs')
  for(lab in labs) {
    
    assign(lab, load_multiple_csv(glue('data/data-direct-exports/labs/{lab}')))
    
    message(glue('Finished loading {lab}: {nrow(get(lab))} rows'))
    
  }
  
  
# preprocessing ----------------------------------------------------------------
  
  ## convert lab value to numeric 
  
    for(lab in labs) {
      
      temp = get(lab) 
      temp_filtered = temp %>% 
        mutate(
          VALUE = gsub('<|>|,|-', '', VALUE), 
          value_num = ifelse(grepl('[A-Z]|[a-z]', VALUE) | VALUE == '' | VALUE == '2.00.14', NA, VALUE), 
          value_num = as.numeric(value_num)
        ) %>% 
        filter(!is.na(value_num))
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows with missing data for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp)
      
    }
  
  ## apply feasibility criteria to lab data 
  
    lab_criteria = hash()
      # for each lab, store criteria as list of c(low, high)
      # low and high are included in feasible bounds 
  
    lab_criteria['SOD']   = c(115, 150)
    lab_criteria['UN']    = c(0, 200)
    lab_criteria['CREAT'] = c(0, 10)
    lab_criteria['GLUC']  = c(20, 1000)
    lab_criteria['A1C']   = c(0, 20)
    lab_criteria['WBC']   = c(0, 100)
    lab_criteria['HGB']   = c(4, 25)
    lab_criteria['PLT']   = c(0, 1000)
    lab_criteria['MCV']   = c(40, 200)
    lab_criteria['MCHC']  = c(25, 50)
    lab_criteria['RDW']   = c(5, 25)
    lab_criteria['EOS']   = c(0, 20)
    lab_criteria['NEUT']  = c(0, 100)
    lab_criteria['LYMPH'] = c(0, 100)
    lab_criteria['BASO']  = c(0, 20)
    lab_criteria['MONO']  = c(0, 20)
    lab_criteria['LDLC']  = c(0, 300)
    lab_criteria['HDL']   = c(0, 150)
    lab_criteria['TRIG']  = c(0, 2000)
    lab_criteria['TBIL']  = c(0, 50)
    lab_criteria['AST']   = c(0, 2000)
    lab_criteria['ALT']   = c(0, 2000)
    lab_criteria['ALK']   = c(0, 2000)
    lab_criteria['ALB']   = c(1, 6)
    lab_criteria['GLOB']  = c(1, 16)

    for(lab in labs) {
      
      bounds = lab_criteria[[lab]]
      bounds = str_split(bounds, ' ')
      
      lower  = bounds[[1]] %>% as.numeric()
      upper  = bounds[[2]] %>% as.numeric()
      
      temp = get(lab) 
      temp_filtered = temp %>% 
        filter(value_num >= lower & value_num <= upper)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows outside of range {lower}-{upper} for {lab}'))
      
      assign(paste0(lab, '_test'), temp_filtered) 
      rm(temp, temp_filtered)
      
    }
    
  ## clean date variables 
  
    for(lab in labs) {
      
      temp = get(lab)
      temp$date = as.Date(temp$COLLECTION_DATE, format = "%m/%d/%Y %H:%M")
      
      assign(lab, temp)
      rm(temp)
      
    }
    
    working_data <- data %>% mutate(date.index = as.Date(date.index, format = '%Y-%m-%d'))
    
    
# get final lab values ---------------------------------------------------------
    
  ## remove values >3SD from patient's median 
    
    for(lab in labs) {
      
      temp = get(lab) 
      
      # calculate patient medians + SDs 
      temp_stats = temp %>% 
        group_by(PatientID) %>% 
        summarize(
          median = median(value_num, na.rm = TRUE), 
          sd     = sd(value_num, na.rm = TRUE), 
          sd     = ifelse(is.na(sd), 0, sd)
        )
      
      # join stats to original lab data, then filter out records >SD from median
      temp_filtered = temp %>% 
        left_join(temp_stats, by = 'PatientID', relationship = 'many-to-one') %>% 
        mutate(
          diff_value  = abs(median - value_num), 
          filter_flag = ifelse(diff_value > 3*sd, 1, 0)
        ) %>% 
        filter(filter_flag == 0)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows >3SD from median for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp, temp_stats, temp_filtered)
      
    }
    
  ## join index dates to labs, filter to +/- one year from index
    
    for(lab in labs) {
      
      temp = get(lab)
      temp_dates = temp %>% 
        left_join(
          working_data %>% select(PatientID, date.index) %>% distinct(), 
          by = 'PatientID', 
          relationship = 'many-to-many'
        ) %>% 
        mutate(
          abs_date_diff = difftime(date, date.index, units = 'days') %>% as.numeric() %>% abs()
        ) %>% 
        filter(
          abs_date_diff < 365.25
        )
      
      assign(lab, temp_dates)
      rm(temp, temp_dates)
      
    }
    
  ## calculate median of values +/- one year from index 
    
    for(lab in labs) {
      
      lab_name = paste0('MEDIAN_', lab)
      
      temp = get(lab) 
      temp_medians = temp %>% 
        group_by(PatientID, date.index, .groups = 'drop') %>% 
        summarize({{lab_name}} := median(value_num))
      
      message(glue('Finished calculation for {lab}'))
      
      assign(lab, temp_medians) 
      rm(temp, temp_medians)
      
    }
    
  ## join to working data 
    
    for(lab in labs) {
      
      temp = get(lab)
      
      working_data = working_data %>% 
        left_join(
          temp %>% select(PatientID, date.index, contains('MEDIAN')), 
          by = c('PatientID', 'date.index')
        )
      
      lab_varname = paste0('MEDIAN_', lab)
      num_nonmissing = working_data[!is.na(working_data[, lab_varname]), ] %>% nrow()
      message(glue('Data has {num_nonmissing} nonmissing rows for {lab}'))
      
      rm(temp)
      
    }
    
    
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-07.csv', row.names = FALSE)
  
  
# testing ----------------------------------------------------------------------
    

  
  
  