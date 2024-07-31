# README -----------------------------------------------------------------------
# 
#   file: cleaning-07-labs-part-2.R
#   goal: perform remainder of cleaning (filters, summary stats) for lab data 
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------
  
  library(openxlsx)
  library(tidyverse)
  library(glue)
  library(hash)

  options(dplyr.summarise.inform = FALSE)
  
  source('code/cleaning/cleaning-functions.R')
  
  data = read.csv('data/working-data/data-post-06.csv')
  hosp = load_multiple_csv('data/data-direct-exports/hospitalizations', verbose = TRUE)
  
  ## cleaned labs (post lab script 1)
  
    labs = list.files('data/clean/labs')
    for(lab in labs) {
      
      assign(lab, read.csv(glue('data/clean/labs/{lab}/cleaned-{lab}.csv')))
      message(glue('Finished loading {lab}: {nrow(get(lab))} rows'))
      
    }
    
  
# preprocessing ----------------------------------------------------------------
  
  ## fix dates for working data
  
    working_data <- data %>% mutate(date.index = as.Date(date.index, format = '%Y-%m-%d'))  
  
  ## fix dates for hospitalizations
  
    hosp = hosp %>% 
      mutate(
        AdmitDateTime = as.POSIXct(AdmitDate, format = '%m/%d/%Y %H:%M'), 
        AdmitDate = as.Date(AdmitDate, format = '%m/%d/%Y %H:%M'),
        DischargeDateTime = as.POSIXct(DischargeDate, format = '%m/%d/%Y %H:%M'),
        DischargeDate = as.Date(DischargeDate, format = '%m/%d/%Y %H:%M'), 
      )
    
  ## only keep hospitalizations with >= one day admission length 
    
    hosp_filtered = hosp %>% 
      mutate(
        admission_length = difftime(DischargeDateTime, AdmitDateTime, units = 'days'), 
        remove_flag = ifelse(admission_length < 1 | admission_length > 365, 1, 0)
      ) %>% 
      filter(remove_flag == 0)
    

# step 5: apply feasibility criteria -------------------------------------------

  ## boundary definitions 
  
    lab_criteria = hash()
    
    lab_criteria['SOD']   = c(115, 150)
    lab_criteria['UN']    = c(NA, 200)
    lab_criteria['CREAT'] = c(NA, 10)
    lab_criteria['GLUC']  = c(20, 1000)
    lab_criteria['A1C']   = c(NA, 20)
    lab_criteria['WBC']   = c(NA, 100)
    lab_criteria['HGB']   = c(4, 25)
    lab_criteria['PLT']   = c(NA, 1000)
    lab_criteria['MCV']   = c(40, 200)
    lab_criteria['MCHC']  = c(25, 50)
    lab_criteria['RDW']   = c(5, 25)
    lab_criteria['EOS']   = c(NA, 20)
    lab_criteria['NEUT']  = c(NA, 100)
    lab_criteria['LYMPH'] = c(NA, 100)
    lab_criteria['BASO']  = c(NA, 20)
    lab_criteria['MONO']  = c(NA, 20)
    lab_criteria['LDLC']  = c(NA, 300)
    lab_criteria['HDL']   = c(NA, 150)
    lab_criteria['TRIG']  = c(NA, 2000)
    lab_criteria['TBIL']  = c(NA, 50)
    lab_criteria['AST']   = c(NA, 2000)
    lab_criteria['ALT']   = c(NA, 2000)
    lab_criteria['ALK']   = c(NA, 2000)
    lab_criteria['ALB']   = c(1, 6)
    lab_criteria['GLOB']  = c(1, 16)
    
  ## initialize exclusion trackers
  
    n_records  = c() 
    n_patients = c()
    
  ## for each lab, apply exclusion criteria
    
    for(lab in labs) {
      
      temp = get(lab)
      
      # extract boundaries  
      bounds = lab_criteria[[lab]]
      bounds = str_split(bounds, ' ')
      
      lower  = bounds[[1]] %>% as.numeric()
      upper  = bounds[[2]] %>% as.numeric()
      
      # perform filter. account for missing upper / lower boundary 
      if (!is.na(lower) & !is.na(upper)) {
        temp_filtered = temp %>% 
          filter(VALUE >= lower & VALUE <= upper)
      } else if (!is.na(lower)) {
        temp_filtered = temp %>% 
          filter(VALUE >= lower)
      } else if (!is.na(upper)) {
        temp_filtered = temp %>% 
          filter(VALUE <= upper)
      }
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data 
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows outside of range {lower}-{upper} for {lab}'))
      
      assign(lab, temp_filtered) 
      rm(temp, temp_filtered)
      
    }
  
  ## update exclusion counts
    
    record_exclusions  = data.frame()
    patient_exclusions = data.frame()
  
    n_records  = c('Step 5: Apply Feasibility Criteria', n_records)
    n_patients = c('Step 5: Apply Feasibility Criteria', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 


# step 6: filter based on hospitalization date ---------------------------------
    
  ## initialize exclusion trackers 
    
    n_records  = c()
    n_patients = c()

  ## for each lab, exclude records within 1 week of hospitalization 
  
    for(lab in labs) {
      
      temp = get(lab)
      
      # mark records for exclusion. exclusion defined as: 
      # - date within 1 week before admission 
      # - date within 1 week after discharge 
      exclude_records = temp %>%
        left_join(hosp_filtered %>% select(-EncounterID), by = 'PatientID', relationship = 'many-to-many') %>%
        mutate(
          admit_diff = difftime(CollectionDate, AdmitDateTime, units = 'days'),
          discharge_diff = difftime(CollectionDate, DischargeDateTime, units = 'days'),
          hosp_flag = ifelse(admit_diff > -7 & discharge_diff < 7, 1, 0)
        ) %>%
        filter(hosp_flag == 1) %>%
        select(PatientID, EncounterID, CollectionDate) %>%
        distinct()
      
      # apply filter 
      temp_filtered = temp %>%
        anti_join(exclude_records, by = c('PatientID', 'EncounterID', 'CollectionDate'))
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',')
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data 
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows within 1w pre - 1w post hospitalization {lab}'))
      
      assign(lab, temp_filtered)
      
    }
  
  ## update exclusion counts
  
    n_records  = c('Step 6: Exclude Records Within 1 Week Pre through 1 Week Post Hospitalization', n_records)
    n_patients = c('Step 6: Exclude Records Within 1 Week Pre through 1 Week Post Hospitalization', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients)


# step 7: remove outliers (relative to patient) --------------------------------
    
  ## initialize exclusion trackers 
    
    n_records  = c() 
    n_patients = c()

  ## for each lab, remove values >3SD from patient's median 
  
    for(lab in labs) {
      
      temp = get(lab) 
      
      # calculate patient medians + SDs 
      temp_stats = temp %>% 
        group_by(PatientID) %>% 
        summarize(
          median = median(VALUE, na.rm = TRUE), 
          sd     = sd(VALUE, na.rm = TRUE), 
          sd     = ifelse(is.na(sd), 0, sd)
        )
      
      # join stats to original lab data, then filter out records >SD from median
      temp_filtered = temp %>% 
        left_join(temp_stats, by = 'PatientID', relationship = 'many-to-one') %>% 
        mutate(
          diff_value  = abs(median - VALUE), 
          filter_flag = ifelse(diff_value > 3*sd, 1, 0)
        ) %>% 
        filter(filter_flag == 0)
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data  
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows >3SD from median for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp, temp_stats, temp_filtered)
      
    }
  
  ## update exclusion counts
  
    n_records  = c('Step 7: Filter to <= 3SD', n_records)
    n_patients = c('Step 7: Filter to <= 3SD', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 


# step 8: filter based on index date -------------------------------------------
    
  ## initialize exclusion trackers 
    
    n_records  = c() 
    n_patients = c()

  ## join index dates to labs, filter to +/- one year from index
    
    for(lab in labs) {
      
      temp = get(lab)
      
      # join index dates, then mark records within one year 
      temp = temp %>% 
        left_join(  
          working_data %>% select(PatientID, date.index) %>% distinct(), 
          by = 'PatientID', 
          relationship = 'many-to-many'
        ) %>% 
        mutate(
          abs_date_diff = difftime(CollectionDate, date.index, units = 'days') %>% as.numeric() %>% abs(),
          one_year_flag = ifelse(abs_date_diff < 365.25, 1, 0)
        )
      
      # apply filter 
      temp_filtered = temp %>% filter(one_year_flag == 1)
      
      # exclusion tracking  
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered %>% select(-date.index), working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
     
      # update local environment with new lab data  
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows outside index date range for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp, temp_filtered)
      
    }
  
  ## update exclusion counts
    
    n_records  = c('Step 8: Filter to +/- 1 Year Index', n_records)
    n_patients = c('Step 8: Filter to +/- 1 Year Index', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
    
# step 9: consistency filter for elevated lab values ---------------------------
    
  ## elevated definitions 
    
    elevated_map = hash()
    
    elevated_map['ALT']  = 400
    elevated_map['AST']  = 400
    elevated_map['ALK']  = 300 
    elevated_map['TBIL'] = 2.5
    
  ## if patient has specified lab elevations, must have >5 occurrences at least 
  ## one year (in total) apart  
    
    labs_sub = c('ALT', 'AST', 'ALK', 'TBIL')
    
    start_time = Sys.time() 
    
    for (lab in labs_sub) {
      
      temp = get(lab)
      elevated_value = values(elevated_map, lab)
      
      temp_final = apply_consistency_filter(temp, elevated_value)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_final)} records failing to meet elevated consistency criteria for {lab}.'))

      assign(lab, temp_final)
      rm(temp, temp_normal, temp_elevated, temp_elevated_final, include_patients)
      
    }
    

# step 10: final transformation ------------------------------------------------

  ## calculate median of values +/- one year from index 
    
    for(lab in labs) {
      
      lab_name = paste0('MEDIAN_', lab)
      
      # calculate median of all remaining records 
      temp = get(lab) 
      temp_medians = temp %>% 
        group_by(PatientID, date.index) %>% 
        summarize({{lab_name}} := median(VALUE))
      
      # update local environment with new lab data 
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

  ## write new working data 
  
    write.csv(working_data, 'data/working-data/data-post-07.csv', row.names = FALSE)
  
  ## bind working data to existing final data 
    
    old_data = read.csv('data/clean/final-cohort/steatosis-cohort-0611.csv')
    updated_data = old_data %>% select(-contains('MEDIAN')) %>% 
      left_join(
        working_data %>% select(PatientID, date.index, type, contains('MEDIAN')) %>% 
          mutate(date.index = as.character(date.index)), 
        by = c('PatientID', 'date.index', 'type'), 
        relationship = 'one-to-one'
      )
    
    write.csv(updated_data, 'data/clean/final-cohort/steatosis-cohort-0731.csv', row.names = FALSE)
    
  ## exclusion trackers 
  
    write.csv(record_exclusions, 'output/labs-record-exclusion-tracker-part-2.csv', row.names = FALSE) 
    write.csv(patient_exclusions, 'output/labs-patient-exclusion-tracker-part-2.csv', row.names = FALSE)
    
    
# testing ----------------------------------------------------------------------
    
  ## step 7 - consistency filter 
    
    test_df = data.frame(
      PatientID = c('A', 'A', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 'C ', 'C', 'C', 'C', 'C', 'C', 'C'), 
      EncounterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
      VALUE = c(405, 500, 505, 600, 600, 600, 450, 425, 400, 406, 604, 545, 566, 700, 606, 20), 
      CollectionDate = c(
        "2023-02-23", "2023-05-23", "2023-06-20", "2024-05-10", "2026-08-02", "2023-05-06", 
        "2022-05-07", "2023-06-16", "2025-03-02", "2022-06-01", "2022-07-02", "2022-08-03", 
        "2022-03-03", "2022-02-02", "2022-01-01", "2021-02-01"
      ), 
      CollectionDateTime = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    ) %>% mutate_at('CollectionDate', function(x) as.Date(x, format = "%Y-%m-%d"))
    
    test_filtered = apply_consistency_filter(test_df, 200)
    
