# README -----------------------------------------------------------------------
# 
#   file: cleaning-functions.R
#   goal: create centralized file for all cleaning functions used in project
#
# ------------------------------------------------------------------------------

# data loading functions -------------------------------------------------------

  ## read in excel doc with multiple sheets

    read_excel_multiple_sheets <- function(fname) {
      
      sheets = getSheetNames(fname)
      data   = data.frame() 
      
      for (i in 2:length(sheets)) {
        temp_data <- read.xlsx(fname, sheet = i)
        data      <- rbind(data, temp_data)
      }
      
      return(data)
      
    }
    
    
  ## read and aggregate all csvs in directory 
  
    load_multiple_csv <- function(csv_dir, verbose=FALSE) {
      
      # purpose: load and aggregate data from multiple csvs  
      # --------------------------------------------------- 
      
      output = data.frame()
      
      files = list.files(csv_dir)
      files = files[grepl('.csv', files)]
      
      for(i in 1:length(files)) {
        
        path    = paste0(csv_dir, '/', files[i])
        temp_df = data.table::fread(path)
        
        output = rbind(output, temp_df)
        
        if(verbose) message(paste0('Successfully read ', files[i]))
        
      }
      
      return(output)
      
    }
  
    
  ## read in all csvs in directory, filtering to specific conditions as you go
  
    load_filtered_dx <- function(dx_dir, icd_names, icd_lists, verbose=FALSE) {
      
      # purpose: filter dx data to only relevant ICDs when loading
      # ----------------------------------------------------------
      
      files  = list.files(dx_dir)
      output = data.frame()
      
      for(i in 1:length(files)) {
        
        path    = paste0(dx_dir, '/', files[i])
        temp_df = data.table::fread(path)
        
        ## filter to ICD codes of interest 
        
        for(j in 1:length(icd_lists)) {
          
          icd_name = paste0(icd_names[j], '_status')
          icd_list = icd_lists[[j]]
  
          
          condition = paste0(icd_list, collapse = '|')
          
          temp_df = temp_df %>% 
            mutate({{icd_name}} := ifelse(grepl(condition, TermCodeMapped), 1, 0))
          
        }
        
        temp_df = temp_df %>% 
          mutate(status_sum = rowSums(select(., contains('status')))) %>% 
          filter(status_sum > 0)
        
        output = rbind(output, temp_df)
        
        if(verbose) message(paste0('Successfully read ', files[i]))
        
      }
      
      return(output)
      
    }
  
    
# data cleaning functions ------------------------------------------------------
    
  ## join encounter date to dataset
  
    join_enc_date <- function(df, enc_df) {
      
      # purpose: match encounter dates to dx records 
      # ------------------------------------------ # 
      
      starting_rows = nrow(df)
      
      matched_date_df <- df %>% 
        left_join(
          enc_df %>% select(-c('PatientClassCode', 'EncounterTypeCode')),
          by = c('PatientID', 'EncounterID')
        ) %>% 
        filter(!is.na(AdmitDate))
      
      ending_rows  = nrow(matched_date_df)
      dropped_rows = (starting_rows - ending_rows)
      
      message(
        sprintf(
          'Dropped %s / %s rows with missing date.', 
          format(dropped_rows, big.mark = ','), 
          format(starting_rows, big.mark = ',')
        )
      )
      
      return(matched_date_df)
      
    } 
  
    
  ## add diagnosis variables to existing dataset 
    
    add_dx_data <- function(dx_str, dx_df, working_df) {
      
      # purpose: calculate + add diagnosis variables to data 
      # -------------------------------------------------- # 
      
      # define variable names based off input 
      date_var_name   = paste0(dx_str, '.date')
      status_var_name = paste0(dx_str, '.status')
      time_var_name   = paste0(dx_str, '.time')
      
      # for each patient, get diagnosis date (and define status as 1)
      dx_date <- dx_df %>% 
        group_by(PatientID) %>% 
        summarize(
          {{date_var_name}} := min(AdmitDate, na.rm = TRUE)
        ) %>% 
        mutate(
          {{status_var_name}} := 1
        )
      
      # join to data; NAs represent diagnosis of 0
      output_df <- working_df %>% 
        left_join(dx_date, by = 'PatientID')
      
      output_df[is.na(output_df[, status_var_name]), status_var_name] = 0
      
      ## derive time variable: if diagnosis: diff(dx, index), else: diff(last_clinic, index)
      output_df <- output_df %>% 
        mutate(
          {{time_var_name}} := case_when(
            .[, status_var_name] == 1 ~ difftime(.[, date_var_name], date.index, units = 'days'),
            .[, status_var_name] == 0 ~ difftime(date.last.clinic, date.index, units = 'days')
          )
        )
      
      return(output_df)
      
    }
    
  ## add substance variables to existing dataset 
    
    add_substance_data <- function(source_str, final_status_str, substance_df, working_df) {
      
      # purpose: calculate + add substance status variables to data 
      # --------------------------------------------------------- # 
      
      # collapse substance statuses into four categories, with unknown -> NA 
      # filter substance df to only nonmissing records
      
      unknown = c('Never Assessed', 'Unknown If Ever Smoked', 'Unknown', 'Not Asked', '') 
      never   = c('Never Smoker', 'Passive Smoke Exposure - Never Smoker', 'Never', 'No')
      former  = c('Former Smoker', 'Former', 'Not Currently')
      current = c('Some Days', 'Heavy Smoker', 'Light Smoker', 'Current Some Day Smoker', 'Current Every Day Smoker', 'Light Tobacco Smoker', 'Heavy Tobacco Smoker', 'Smoker, Current Status Unknown', 'Every Day', 'Yes')
      
      collapsed_substance_df <- substance_df %>% 
        select(PatientID, EncounterID, {{source_str}}, AdmitDate) %>% 
        mutate(
          status.collapsed = case_when(
            is.na(.[, source_str]) ~ 'Unknown',
            .[, source_str] %in% unknown ~ 'Unknown',
            .[, source_str] %in% never ~ 'Never',
            .[, source_str] %in% former ~ 'Former',
            .[, source_str] %in% current ~ 'Current',
            TRUE ~ 'error'
          )
        ) %>% 
        filter(status.collapsed != 'Unknown') 
      
      # join index dates and calculate difference in time
      # filter to records within +/- one year of index date 
      dates_substance_df <- collapsed_substance_df %>% 
        left_join(
          working_df %>% select(PatientID, date.index), 
          by = 'PatientID', 
          relationship = 'many-to-many'
        ) %>% 
        mutate(datediff = abs(difftime(AdmitDate, date.index, units = 'days'))) %>% 
        group_by(PatientID, date.index) %>% 
        filter(datediff < 365.25) %>% 
        ungroup()
      
      # for records before index date defined as "current", redefine as "former"
      dates_substance_df <- dates_substance_df %>% 
        mutate(
          status.collapsed.test = case_when(
            AdmitDate < date.index & status.collapsed == 'Current' ~ 'Former', 
            TRUE ~ status.collapsed
          )
        )
      
      # collapse all values for patient/index combo into one location  
      substance_per_index <- dates_substance_df %>% 
        # for each patient + index date combo...
        group_by(PatientID, date.index) %>% 
        ## collapse all status values into single variable 
        summarize(
          status.values = paste0(status.collapsed, collapse = ' + '), 
        ) %>% 
        ungroup() 
      
      # create final variable using most positive class 
      substance_final <- substance_per_index %>% 
        mutate(
          {{final_status_str}} := case_when(
            # current > former > never > unknown
            grepl('Current', status.values) ~ 'Current', 
            grepl('Former', status.values) ~ 'Former', 
            grepl('Never', status.values) ~ 'Never', 
            TRUE ~ 'error'
          )
        ) %>% 
        select(PatientID, date.index, {{final_status_str}}) %>% 
        distinct()
      
      # join to working data; should be many:one for data:substance
      output_df <- working_df %>%
        left_join(
          substance_final,
          by = c('PatientID', 'date.index'),
          relationship = 'many-to-one'
        )
      
      return(output_df)
      
    }
    
  ## add alcohol usage variables 
    
    add_alcohol_data <- function(substance_df, working_df) {
      
      # purpose: calculate + add alcohol use amount variables to data 
      # ----------------------------------------------------------- # 
      
      # filter to nonmissing OZ data
      nonmissing_alcohol <- substance_df %>% 
        filter(!is.na(AlcoholOZPerWeek))
      
      # join index dates 
      dates_alcohol <- nonmissing_alcohol %>% 
        left_join(
          working_df %>% select(PatientID, date.index), 
          by = 'PatientID', 
          relationship = 'many-to-many'
        ) %>% 
        mutate(datediff = abs(difftime(AdmitDate, date.index, units = 'days'))) %>% 
        group_by(PatientID, date.index) %>% 
        filter(datediff < 365.25) %>% 
        ungroup()
      
      # standardize alcohol OZ 
      standardized_alcohol <- dates_alcohol %>% 
        rowwise() %>% 
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
        ungroup()
      
      # average alcohol consumption for each patient/index combo 
      alcohol_final <- standardized_alcohol %>% 
        group_by(PatientID, date.index) %>% 
        summarize(alcohol.OZ.per.week = mean(FixedAlcoholOZPerWeek)) %>% 
        mutate(etoh.gm = alcohol.OZ.per.week * 23.33/7) %>% 
        select(PatientID, date.index, alcohol.OZ.per.week, etoh.gm)
        
      # join to working data; should be many:one for data:substance
      output_df <- working_df %>%
        left_join(
          alcohol_final,
          by = c('PatientID', 'date.index'),
          relationship = 'many-to-one'
        )
      
      return(output_df)
        
    } 
    
    
# labs -------------------------------------------------------------------------
    
  ## remove lab records if multiple within 48 hours 
    
    remove48 = function(temp) {
      
      setDT(temp)
      setorder(temp, PatientID, CollectionDate)
      
      temp_collapsed <- temp[, {
        
        collapsed_records = list()
        i = 1
        
        ## iterate over lab dates for each patient
        while (i <= .N) {
          
          start_date = CollectionDate[i]
          end_date   = start_date + 2      # 2 day timeframe for collapse 
          values     = list(VALUE[i])
          
          start_date_time = CollectionDateTime[i]
          
          ## save all lab records within 2 days to list
          j = i + 1
          while (j <= .N && CollectionDate[j] <= end_date) {
            values = append(values, VALUE[j])
            ## update end date to be relative to next lab in series 
            end_date = CollectionDate[j] + 2
            
            j = j + 1   
          }
          
          # if more than 1 values within 48 hour periods of one another, exclude all
          avg_value = -1
          ## take mean of list
          if (length(unlist(values)) == 1) {
            avg_value = unlist(values)
          }
          
          ## save as new collapsed record under collection date = start date 
          collapsed_records[[length(collapsed_records) + 1]] <- list(EncounterID[i], avg_value, start_date, start_date_time)
          
          ## i is now the j we left off on; skips over 
          i <- j
        }
        
        rbindlist(collapsed_records, use.names = FALSE)
        
      }, by = PatientID]
      
      setnames(temp_collapsed, c('PatientID', 'EncounterID', 'VALUE', 'CollectionDate', 'CollectionDateTime'))
      
      temp_collapsed = temp_collapsed %>% filter(VALUE != -1)
      
      return(temp_collapsed)
      
    }
    
  ## if lab over certain value, validate that patient consistently has high lab. if not, remove
    
    apply_consistency_filter = function(temp, elevated_value) {
      
      ## split data by elevated status 
      temp_normal   = temp %>% filter(VALUE < elevated_value) 
      temp_elevated = temp %>% filter(VALUE >= elevated_value)
      
      ## for elevated labs, check following conditions: 
      ## - at least five other labs with elevated status 
      ## - difference between start and max date is at least one year 
      include_patients = temp_elevated %>% 
        group_by(PatientID) %>% 
        summarize(
          min_date = min(CollectionDate), 
          max_date = max(CollectionDate), 
          n_values = n() 
        ) %>% 
        filter(
          difftime(max_date, min_date, units = 'days') > 365 &
            n_values > 5
        )
      
      temp_elevated_final = temp_elevated %>% 
        inner_join(
          include_patients %>% select(PatientID), 
          by = 'PatientID'
        )
      
      # ## merge back to normal labs 
      temp_final = rbind(temp_normal, temp_elevated_final)
      
      return(temp_final)
      
    }
    
    
# exclusion tracking -----------------------------------------------------------
    
  ## get distinct # of patients with measurement (lab, bmi) data within one year of index
    
    get_n_patients_within_index = function(measure_df, index_df) {
      
      measure_join_index = measure_df %>% 
        left_join(
          index_df %>% select(PatientID, date.index) %>% distinct(), 
          by = 'PatientID', 
          relationship = 'many-to-many'
        )
      
      measure_index_flag = measure_join_index %>%
        mutate(
          abs_date_diff = difftime(CollectionDate, date.index, units = 'days') %>% as.numeric() %>% abs(),
          one_year_flag = ifelse(abs_date_diff < 365.25, 1, 0)
        )
      
      patients_with_measure_within_index = measure_index_flag %>%
        filter(one_year_flag == 1) %>%
        distinct(PatientID)
      
      return(patients_with_measure_within_index %>% nrow())
      
    }
    
  ## get # of distinct measurement records (useful when working with data joined to index dates) 
    
    get_n_records = function(measure_df) {
      
      distinct_measure = measure_df %>% 
        select(PatientID, EncounterID, CollectionDateTime, VALUE) %>% 
        distinct() 
      
      return(distinct_measure %>% nrow())
      
    }
      
      