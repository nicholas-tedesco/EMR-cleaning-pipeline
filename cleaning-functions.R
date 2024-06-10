# README -----------------------------------------------------------------------
# 
#   file: cleaning-functions.R
#   goal: create centralized file for all cleaning functions used in project
#
# ------------------------------------------------------------------------------

# packages ---------------------------------------------------------------------


# functions for loading data ---------------------------------------------------

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
  
    
# functions for easy data cleaning ---------------------------------------------
    
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
      
      date_var_name   = paste0(dx_str, '.date')
      status_var_name = paste0(dx_str, '.status')
      time_var_name   = paste0(dx_str, '.time')
      
      dx_date <- dx_df %>% 
        group_by(PatientID) %>% 
        summarize(
          {{date_var_name}} := min(AdmitDate, na.rm = TRUE)
        ) %>% 
        mutate(
          {{status_var_name}} := 1
        )
      
      output_df <- working_df %>% 
        left_join(dx_date, by = 'PatientID') %>% 
        mutate(
          {{time_var_name}} := difftime(.[, date_var_name], date.index, units = 'days')
        )
      
      return(output_df)
      
    }
  
  
  