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

  source('code/cleaning/cleaning-functions.R')

  data = read.csv('data/working-data/data-post-06.csv')
  hosp = load_multiple_csv('data/data-direct-exports/hospitalizations', verbose = TRUE)
  
  labs = list.files('data/data-direct-exports/labs')
  labs = labs[labs != 'GLOB']
  for(lab in labs) {
    
    assign(lab, load_multiple_csv(glue('data/data-direct-exports/labs/{lab}')))
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
        remove_flag = ifelse(admission_length < 1, 1, 0)
      ) %>% 
      filter(remove_flag == 0)


# step 0: date prep + baseline counts ------------------------------------------
  
  ## for each lab, fix date variables 
    
    n_records  = c() 
    n_patients = c()
  
    for(lab in labs) {
      
      temp = get(lab)
      temp = temp %>% 
        rename(CollectionDateTime = COLLECTION_DATE) %>% 
        mutate(CollectionDate = as.Date(CollectionDateTime, format = "%m/%d/%Y %H:%M")) %>% 
        distinct() 
      
      temp_n_records  = get_n_records(temp) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
     
      assign(lab, temp)
      rm(temp)
      
    }
    
  ## initialize exclusion trackers 
    
    record_exclusions  = data.frame()
    patient_exclusions = data.frame()
    
    n_records  = c('Step 0: Baseline Counts', n_records)
    n_patients = c('Step 0: Baseline Counts', n_patients)
  
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
    colnames(record_exclusions)  = c('CleaningStep', labs)
    colnames(patient_exclusions) = c('CleaningStep', labs)
    
  
# step 1: convert lab to numeric -----------------------------------------------
    
  ## for each lab, clean up lab value column 
    
    n_records  = c()
    n_patients = c()
    
    for(lab in labs) {
      
      temp = get(lab) 
      temp_filtered = temp %>% 
        mutate(
          VALUE = gsub('<|>|,|-', '', VALUE), 
          VALUE = ifelse(grepl('[A-Z]|[a-z]', VALUE) | VALUE == '' | VALUE == '2.00.14', NA, VALUE), 
          VALUE = as.numeric(VALUE)
        ) %>% 
        filter(!is.na(VALUE))
      
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows with missing data for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp)
      
    }
  
  ## update exclusion counts
    
    n_records  = c('Step 1: Convert to Numeric', n_records)
    n_patients = c('Step 1: Convert to Numeric', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
  
# step 2: get single record per collection datetime ----------------------------
    
  ## for each lab, take mean of values across ID/CollectionDate combo 
    
    n_records  = c()
    n_patients = c()
      
    for (lab in labs) {
      
      temp = get(lab) 
      
      temp = temp %>% 
        select(PatientID, EncounterID, CollectionDate, VALUE, CollectionDateTime)
      
      temp_counts = temp %>% 
        group_by(PatientID, EncounterID, CollectionDate, CollectionDateTime) %>% 
        summarize(count = n())
  
      temp_no_dups = temp %>% 
        inner_join(
          temp_counts %>% filter(count < 2) %>% select(-count), 
          by = c('PatientID', 'EncounterID', 'CollectionDate', 'CollectionDateTime'), 
          relationship = 'one-to-one'
        )
      
      temp_dups = temp %>% 
        inner_join(
          temp_counts %>% filter(count >= 2) %>% select(-count), 
          by = c('PatientID', 'EncounterID', 'CollectionDate', 'CollectionDateTime'), 
          relationship = 'many-to-one'
        )
        
      temp_dups_resolved = temp_dups %>% 
        group_by(PatientID, EncounterID, CollectionDate, CollectionDateTime) %>% 
        summarize(VALUE = mean(VALUE))
      
      temp_resolved = rbind(temp_no_dups, temp_dups_resolved) 
      
      temp_n_records  = get_n_records(temp_resolved) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_resolved, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_resolved)} rows with duplicate data for {lab}'))
      
      assign(lab, temp_resolved) 
      rm(temp, temp_resolved, temp_dups_resolved, temp_no_dups, temp_counts)
      
    }
    
  ## update exclusion counts
    
    n_records  = c('Step 2: Single Lab per DateTime', n_records)
    n_patients = c('Step 2: Single Lab per DateTime', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
    
# BREAK: derive GLOB from PROT - ALB -------------------------------------------
    
  ## join PROT and ALB data + calculate GLOB 
    
    PROT = PROT %>% rename(prot_VALUE = VALUE)
    
    PROT = PROT %>% 
      left_join(ALB %>% rename(alb_VALUE = VALUE), by = c('PatientID', 'EncounterID', 'CollectionDate', 'CollectionDateTime')) %>% 
      mutate(
        glob_VALUE = prot_VALUE - alb_VALUE
      )
    
    message(glue('Dropping another {nrow(PROT) - nrow(PROT %>% filter(!is.na(glob_VALUE)))} rows for GLOB'))
    
  ## overwrite existing GLOB df 
    
    GLOB = PROT %>% 
      select(PatientID, EncounterID, CollectionDate, CollectionDateTime, VALUE = glob_VALUE)
    
  ## PROT is now GLOB - update trackers accordingly 
    
    record_exclusions  = record_exclusions %>% rename(GLOB = PROT)
    patient_exclusions = patient_exclusions %>% rename(GLOB = PROT)
    
    new_labs = c() 
    for(lab in labs) {
      if(lab == 'PROT') {
        new_labs = c(new_labs, 'GLOB')
      } else {
        new_labs = c(new_labs, lab)
      }
    }
    
    labs = new_labs 
    
    
# step 3: apply feasibility criteria -------------------------------------------
    
  ## boundary definitions 
  
    lab_criteria = hash()
      # for each lab, store criteria as list of c(low, high)
      # low and high are included in feasible bounds 
  
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
    
  ## respectively apply criteria 
    
    n_records  = c() 
    n_patients = c()

    for(lab in labs) {
      
      bounds = lab_criteria[[lab]]
      bounds = str_split(bounds, ' ')
      
      lower  = bounds[[1]] %>% as.numeric()
      upper  = bounds[[2]] %>% as.numeric()
      
      temp = get(lab)
      
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
      
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows outside of range {lower}-{upper} for {lab}'))
      
      assign(lab, temp_filtered) 
      rm(temp, temp_filtered)
      
    }
  
  ## update exclusion counts
    
    n_records  = c('Step 3: Apply Feasibility Criteria', n_records)
    n_patients = c('Step 3: Apply Feasibility Criteria', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 

    
# step 4: filter based on hospitalization date ---------------------------------
    
  ## join hospitalization dates to labs, flag if within +/- 1 week 

    n_records  = c()
    n_patients = c()

    for(lab in labs) {

      temp = get(lab)

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

      temp_filtered = temp %>%
        anti_join(exclude_records, by = c('PatientID', 'EncounterID', 'CollectionDate'))

      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',')
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')

      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)

      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows within 1w pre - 1w post hospitalization {lab}'))

      assign(lab, temp_filtered)

    }

  ## update exclusion counts

    n_records  = c('Step 4: Exclude Records Within 1 Week Pre through 1 Week Post Hospitalization', n_records)
    n_patients = c('Step 4: Exclude Records Within 1 Week Pre through 1 Week Post Hospitalization', n_patients)

    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients)
    
    
# step 5: remove outliers (relative to overall lab) ----------------------------
    
  ## for each lab, remove values outside the 0.25 and 99.75 percentile bounds
    
    n_records  = c() 
    n_patients = c()
    
    for (lab in labs) {
      
      temp = get(lab) 
      
      lower_bound = quantile(temp$VALUE, 0.0025)
      upper_bound = quantile(temp$VALUE, 0.9975)
      
      temp_filtered = temp %>% 
        filter(
          VALUE >= lower_bound & VALUE <= upper_bound
        )
      
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',')
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows outside of {lower_bound} - {upper_bound} for {lab}'))
      
      assign(lab, temp_filtered)
      
    }
    
  ## update exclusion counts
    
    n_records  = c('Step 5: Exclude Records Outside 0.25 and 99.75 percentile bounds', n_records)
    n_patients = c('Step 5: Exclude Records Outside 0.25 and 99.75 percentile bounds', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients)
    
    
# step 6: remove outliers (relative to patient) --------------------------------
    
  ## remove values >3SD from patient's median 
    
    n_records  = c() 
    n_patients = c()
    
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
      
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows >3SD from median for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp, temp_stats, temp_filtered)
      
    }
    
    ## update exclusion counts
    
    n_records  = c('Step 6: Filter to <= 3SD', n_records)
    n_patients = c('Step 6: Filter to <= 3SD', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
    
    
# step 7: filter based on index date -------------------------------------------
    
  ## join index dates to labs, filter to +/- one year from index
    
    n_records  = c() 
    n_patients = c()
    
    for(lab in labs) {
      
      temp = get(lab)
      
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
      
      temp_filtered = temp %>% filter(one_year_flag == 1)
      
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered %>% select(-date.index), working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows outside index date range for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp, temp_filtered)
      
    }
    
  ## update exclusion counts
    
    n_records  = c('Step 7: Filter to +/- 1 Year Index', n_records)
    n_patients = c('Step 7: Filter to +/- 1 Year Index', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    

# step 8: final transformation -------------------------------------------------
    
  ## calculate median of values +/- one year from index 
    
    for(lab in labs) {
      
      lab_name = paste0('MEDIAN_', lab)
      
      temp = get(lab) 
      temp_medians = temp %>% 
        group_by(PatientID, date.index) %>% 
        summarize({{lab_name}} := median(VALUE))
      
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
    
    # write.csv(working_data, 'data/working-data/data-post-07.csv', row.names = FALSE)
  
  ## bind working data to existing final data 
    
    old_data = read.csv('data/final-data/steatosis-cohort-0611.csv')
    updated_data = old_data %>% select(-contains('MEDIAN')) %>% 
      left_join(
        working_data %>% select(PatientID, date.index, type, contains('MEDIAN')) %>% 
          mutate(date.index = as.character(date.index)), 
        by = c('PatientID', 'date.index', 'type'), 
        relationship = 'one-to-one'
      )
    
    write.csv(updated_data, 'data/final-data/steatosis-cohort-0616.csv', row.names = FALSE)
    
  ## exclusion trackers 
    
    write.csv(record_exclusions, 'output/labs-record-exclusion-tracker.csv', row.names = FALSE) 
    write.csv(patient_exclusions, 'output/labs-patient-exclusion-tracker.csv', row.names = FALSE)
    
  
  