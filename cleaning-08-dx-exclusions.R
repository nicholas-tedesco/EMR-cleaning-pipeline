# README -----------------------------------------------------------------------
# 
#   name: cleaning-08-dx-exclusions.R
#   goal: apply diagnosis-based exclusions to data 
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)
  library(openxlsx)
  library(data.table)

  source('code/cleaning/cleaning-functions.R')

  data = read.csv('data/working-data/data-post-07.csv')
  enc_data = load_multiple_csv('data/data-direct-exports/encounter-data', verbose = TRUE)
  
  
# load diagnosis data ----------------------------------------------------------
  
  ## define ICD codes of interest
    
    cancer_icd = paste0('^', c(140:209, paste0('C', 0:96), 'C7A', 'C7B'))
    cancer_icd = cancer_icd[!(cancer_icd %in% c('^173', '^C44'))]          # 173, C44 allowed

    liver_tx_icd = paste0('^', c('V42.7', '996.82', 'Z94.4', 'T86.40', 'T86.41', 'T86.49'))
  
    cld_icd = paste0('^', c(
      '070.22', '070.23', '070.32', '070.33', 'V02.61', 'B18.0', 'B18.1', 'B19.1',    # chronic hepatitis B
      '070.44', '070.54', '070.70', '070.71', 'v02.62', 'B18.2', 'B19.20', 'B19.21',  # chronic hepatitis C 
      '070.59', '070.6', '070.9', 'B18.8', '18.9', 'B19.9',                           # other viral hepatitis
      '571.6', 'K74.3', 'K74.5',                                                      # primary biliary cholangitis
      '275.1', 'E83.01',                                                              # Wilson's disease 
      '273.4', 'E88.01',                                                              # alpha-1 antitrypsin deficiency
      '275.01', '275.03', 'E83.11',                                                   # hemochromatosis
      '571.42', 'K75.4',                                                              # autoimmune hepatitis
      'K83.01',                                                                       # primary sclerosing cholangitis
      'K71'                                                                           # drug induced liver injury
    ))
    
  ## read in outcomes, then divide into dx groups 
  
    dx_path = 'data/data-direct-exports/diagnosis-data/combined-dx/'
    exclusion_data = load_filtered_dx(
      dx_path, c('cancer', 'liver_tx', 'cld'), 
      list(cancer_icd, liver_tx_icd, cld_icd), verbose = TRUE
    )
    
    cancer    = exclusion_data %>% filter(cancer_status == 1) %>% select(-contains('status'))
    liver_tx  = exclusion_data %>% filter(liver_tx_status == 1) %>% select(-contains('status'))
    cld       = exclusion_data %>% filter(cld_status == 1) %>% select(-contains('status'))
  
  
# preprocessing ----------------------------------------------------------------
  
  ## clean date variables 
  
    enc_data$AdmitDate <- as.Date(enc_data$AdmitDate, format = "%m/%d/%Y %H:%M")
    data$date.index <- as.Date(data$date.index, format = '%Y-%m-%d')
  
  ## match encounter dates to diagnosis records  
  
    cancer   <- join_enc_date(cancer, enc_data)     
    liver_tx <- join_enc_date(liver_tx, enc_data)     
    
    
# exclusion 1: no cancers one year after index date or earlier -----------------
    
  ## for each patient, get earliest cancer dx date 
    
    cancer_dates <- cancer %>% 
      group_by(PatientID) %>% 
      filter(AdmitDate == min(AdmitDate)) %>% 
      select(PatientID, CancerDate = AdmitDate) %>% 
      distinct()
    
  ## merge index dates to cancer dates
    
    cancer_index_dates <- cancer_dates %>% 
      left_join(
        data %>% select(PatientID, date.index) %>% distinct(), 
        by = 'PatientID', 
        relationship = 'one-to-many'
      )
      ## will be 1:many relationship; 1 patient in cancer dates may be associated with multiple admissions
    
  ## calculate time difference + filter to patients in violation 
    
    cancer_exclusion <- cancer_index_dates %>% 
      mutate(
        datediff = difftime(CancerDate, date.index, units = 'days'), 
        exclusion_flag = ifelse(datediff < 365.25, 1, 0)
      ) %>% 
      filter(exclusion_flag == 1)
    
    cancer_exclusion_ids <- cancer_exclusion %>% 
      select(PatientID, date.index)
    
  ## apply cancer exclusion to working data 
    
    working_data <- data %>% 
      anti_join(cancer_exclusion_ids, by = c('PatientID', 'date.index'))
    
    
# exclusion 2: no liver transplant one year after index date or earlier --------
    
  ## for each patient, get liver transplant date 
    
    liver_tx_dates <- liver_tx %>% 
      group_by(PatientID) %>% 
      filter(AdmitDate == min(AdmitDate)) %>% 
      select(PatientID, LiverTXDate = AdmitDate) %>% 
      distinct()
    
  ## merge index dates to liver TX dates
    
    liver_tx_dates <- liver_tx_dates %>% 
      left_join(
        data %>% select(PatientID, date.index) %>% distinct(), 
        by = 'PatientID', 
        relationship = 'one-to-many'
      )
      ## will be 1:many relationship; 1 patient in liver dates may be associated with multiple admissions
    
  ## calculate time difference + filter to patients in violation 
    
    liver_tx_exclusion <- liver_tx_dates %>% 
      mutate(
        datediff = difftime(LiverTXDate, date.index, units = 'days'), 
        exclusion_flag = ifelse(datediff < 365.25, 1, 0)
      ) %>% 
      filter(exclusion_flag == 1)
    
    liver_tx_exclusion_ids <- liver_tx_exclusion %>% 
      select(PatientID, date.index)  
    
  ## apply liver transplant exclusion to working data 
    
    working_data <- working_data %>% 
      anti_join(liver_tx_exclusion_ids, by = c('PatientID', 'date.index'))

    
# exclusion 3: no CLD at any time ----------------------------------------------
    
  ## determine which patients have non-NAFLD-ALD-AUD CLD diagnoses 
    
    cld_patients <- unique(cld$PatientID)
    
  ## remove these patients from working data 
    
    working_data <- working_data %>% 
      filter(!(PatientID %in% cld_patients))
    
    
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-08.csv', row.names = FALSE)
    
    