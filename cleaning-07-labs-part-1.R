# README -----------------------------------------------------------------------
# 
#   file: cleaning-07-labs-part-1.R
#   goal: perform basic cleaning (data types, collapsing) for lab data
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------
  
  library(openxlsx)
  library(tidyverse)
  library(glue)
  library(hash)
  library(data.table)

  options(dplyr.summarise.inform = FALSE)

  source('code/cleaning/cleaning-functions.R')

  data = read.csv('data/working-data/data-post-06.csv')
  
  ## load raw lab data 
    
    labs = list.files('data/data-direct-exports/labs')
    labs = labs[labs != 'GLOB']   # replacing raw GLOB with PROT - ALB 
    
    for(lab in labs) {
      
      assign(lab, load_multiple_csv(glue('data/data-direct-exports/labs/{lab}')))
      message(glue('Finished loading {lab}: {nrow(get(lab))} rows'))
      
    }
    
  ## load encounters 
    
    enc_data = load_multiple_csv('data/data-direct-exports/encounter-data', verbose = TRUE)
  
  
# preprocessing ----------------------------------------------------------------
  
  ## fix dates for working data
  
    working_data <- data %>% mutate(date.index = as.Date(date.index, format = '%Y-%m-%d'))  


# step 0: date prep + baseline counts ------------------------------------------
    
  ## intialize exclusion trackers 
    
    n_records  = c() 
    n_patients = c()
    
    record_exclusions  = data.frame()
    patient_exclusions = data.frame()
  
  ## for each lab, fix date variables 
  
    for(lab in labs) {
      
      temp = get(lab)
      
      # fix date variables 
      temp = temp %>% 
        rename(CollectionDateTime = COLLECTION_DATE) %>% 
        mutate(CollectionDate = as.Date(CollectionDateTime, format = "%m/%d/%Y %H:%M")) %>% 
        distinct() 
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data
      assign(lab, temp)
      rm(temp)
      
    }
    
  ## add counts to exclusion trackers 
    
    n_records  = c('Step 0: Baseline Counts', n_records)
    n_patients = c('Step 0: Baseline Counts', n_patients)
  
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
    colnames(record_exclusions)  = c('CleaningStep', labs)
    colnames(patient_exclusions) = c('CleaningStep', labs)
    
    
# step 1: filter to outpatient encounters --------------------------------------
    
  ## what are outpatient encounters? 
    
    unique_codes = unique(enc_data$PatientClassCode)
    print(unique_codes)
    
  ## initialize exclusion trackers 
    
    n_records  = c() 
    n_patients = c()
    
  ## for each lab, filter to outpatient records only 
    
    for(lab in labs) {
      
      temp = get(lab) 
      
      # filter to PatientClassCode == 'Outpatient'
      temp_filtered = temp %>% 
        left_join(
          enc_data %>% select(PatientID, EncounterID, PatientClassCode), 
          by = c('PatientID', 'EncounterID'), 
          relationship = 'many-to-one'
        ) %>% 
        filter(PatientClassCode == 'Outpatient') %>% 
        select(-PatientClassCode)
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} non-outpatient rows for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp)
      
    }
    
    ## update exclusion counts
      
      n_records  = c('Step 1: Filter to Outpatient Only', n_records)
      n_patients = c('Step 1: Filter to Outpatient Only', n_patients)
      
      record_exclusions  = rbind(record_exclusions, n_records)
      patient_exclusions = rbind(patient_exclusions, n_patients) 
    
  
# step 2: convert lab to numeric -----------------------------------------------
    
  ## initialize exclusion trackers
    
    n_records  = c()
    n_patients = c()
    
  ## for each lab, clean up lab value column 
    
    for(lab in labs) {
      
      temp = get(lab) 
      
      # remove non-numeric characters, then coerce to numeric 
      temp_filtered = temp %>% 
        mutate(
          VALUE = gsub('<|>|,|-', '', VALUE), 
          VALUE = ifelse(grepl('[A-Z]|[a-z]', VALUE) | VALUE == '' | VALUE == '2.00.14', NA, VALUE), 
          VALUE = as.numeric(VALUE)
        ) %>% 
        filter(!is.na(VALUE))
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp_filtered) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_filtered, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data
      message(glue('Dropped {nrow(temp) - nrow(temp_filtered)} rows with missing data for {lab}'))
      
      assign(lab, temp_filtered)
      rm(temp)
      
    }
  
  ## update exclusion counts
    
    n_records  = c('Step 2: Convert to Numeric', n_records)
    n_patients = c('Step 2: Convert to Numeric', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
  
# step 3: get single record per collection datetime ----------------------------
    
  ## initialize exclusion trackers
    
    n_records  = c()
    n_patients = c()
    
  ## for each lab, take mean of values across ID/CollectionDate combo 
      
    for (lab in labs) {
      
      temp = get(lab) 
      
      temp = temp %>% 
        select(PatientID, EncounterID, CollectionDate, VALUE, CollectionDateTime)
      
      # identify duplicate labs per datetime 
      temp_counts = temp %>% 
        group_by(PatientID, EncounterID, CollectionDate, CollectionDateTime) %>% 
        summarize(count = n())
    
      # subset all lab records to non-duplicates vs duplicates
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
      
      # for duplicates, resolve by taking mean of value 
      temp_dups_resolved = temp_dups %>% 
        group_by(PatientID, EncounterID, CollectionDate, CollectionDateTime) %>% 
        summarize(VALUE = mean(VALUE))
      
      # rejoin non-duplicates to resolved duplicates 
      temp_resolved = rbind(temp_no_dups, temp_dups_resolved) 
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp_resolved) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_resolved, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data 
      message(glue('Dropped {nrow(temp) - nrow(temp_resolved)} rows with duplicate data for {lab}'))
      
      assign(lab, temp_resolved) 
      rm(temp, temp_resolved, temp_dups_resolved, temp_no_dups, temp_counts)
      
    }
    
  ## update exclusion counts
    
    n_records  = c('Step 3: Single Lab per DateTime', n_records)
    n_patients = c('Step 3: Single Lab per DateTime', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
    
# step 4: remove proximal (within 48hr) labs -----------------------------------
    
    # patient A | lab = 20 | collection_date = 04/02/2023
    # patient A | lab = 26 | collection_date = 04/03/2023
    # patient A | lab = 30 | collection_date = 04/05/2023
    
    # -> collapse -> 
    
    # patient A | lab = 30 | collection_date = 04/05/2023     # note how this record is not collapsed, despite being within 48hr of 04/03
   
  ## initialize exclusion trackers
    
    n_records  = c()
    n_patients = c()
    
  ## for each lab, remove records within 48hr of one another 
    
    for (lab in labs) {
      
      temp = get(lab)
      
      # call remove48, which performs 48hr removal operation. see cleaning functions for details  
      temp_collapsed = remove48(temp)
      
      # exclusion tracking 
      temp_n_records  = get_n_records(temp_collapsed) %>% format(big.mark = ',') 
      temp_n_patients = get_n_patients_within_index(temp_collapsed, working_data) %>% format(big.mark = ',')
      
      n_records  = c(n_records, temp_n_records)
      n_patients = c(n_patients, temp_n_patients)
      
      # update local environment with new lab data 
      message(glue('Dropped {nrow(temp) - nrow(temp_collapsed)} rows during collapse for {lab}'))
      
      assign(lab, temp_collapsed)
      rm(temp, temp_collapsed)
      
    }
    
  ## update exclusion counts
    
    n_records  = c('Step 4: Single Lab per 2-Day Window', n_records)
    n_patients = c('Step 4: Single Lab per 2-Day Window', n_patients)
    
    record_exclusions  = rbind(record_exclusions, n_records)
    patient_exclusions = rbind(patient_exclusions, n_patients) 
    
    
# step 5: derive GLOB from PROT - ALB ------------------------------------------
    
  ## join PROT and ALB data + calculate GLOB 
    
    PROT = PROT %>% rename(prot_VALUE = VALUE)
    
    PROT = PROT %>% 
      left_join(ALB %>% rename(alb_VALUE = VALUE), by = c('PatientID', 'EncounterID', 'CollectionDate', 'CollectionDateTime')) %>% 
      mutate(
        glob_VALUE = prot_VALUE - alb_VALUE
      )
    
    message(glue('Dropping another {nrow(PROT) - nrow(PROT %>% filter(!is.na(glob_VALUE)))} rows for GLOB'))
    
  ## create new lab df 
    
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
    

# export -----------------------------------------------------------------------
    
  ## labs 
    
    for(lab in labs) {
      
      temp = get(lab)
      
      output_path = glue('data/clean/labs/{lab}/')
      if (!(lab %in% list.dirs('data/clean/labs/'))) {
        dir.create(output_path)
      }
      
      lab_path = paste0(output_path, glue('/cleaned-{lab}.csv'))
      write.csv(temp, lab_path, row.names = FALSE)
      
    }
    
  ## exclusion trackers 
    
    write.csv(record_exclusions, 'output/labs-record-exclusion-tracker-part-1.csv', row.names = FALSE) 
    write.csv(patient_exclusions, 'output/labs-patient-exclusion-tracker-part-1.csv', row.names = FALSE)
    
    
# testing ----------------------------------------------------------------------
    
  ## step 4
    
    test_df = data.frame(
      PatientID   = c('A', 'A', 'A', 'A', 'B', 'B', 'B'), 
      EncounterID = c('1', '2', '3', '4', '5', '6', '7'), 
      VALUE = c(5, 20, 74, 3, 6, 80, 3), 
      CollectionDate = c(
        "2024-01-05", "2024-03-02", "2024-03-03", "2024-03-05", 
        "2026-06-24", "2026-07-04", "2026-07-05"
      ), 
      CollectionDateTime = c(1, 2, 3, 4, 5, 6, 7)
    ) %>% 
      mutate_at('CollectionDate', function(x) as.Date(x, format = "%Y-%m-%d"))
    
    test_collapsed = remove48(test_df)
    
    