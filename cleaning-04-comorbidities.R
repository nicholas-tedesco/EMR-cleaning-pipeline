# README -----------------------------------------------------------------------
# 
#   name: cleaning-04-comorbidities.R
#   goal: prepare + merge comorbidity data by... 
#         - merging comorbidity data to encounters 
#         - defining comorbidities
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)
  library(openxlsx)
  library(data.table)

  source('code/cleaning/cleaning-functions.R')
  
  data = read.csv('data/working-data/data-post-03.csv')
  enc_data = load_multiple_csv('data/data-direct-exports/encounter-data', verbose = TRUE)


# load diagnosis data ----------------------------------------------------------
    
  ## define ICD codes of interest
    
    hypertension_icd = paste0('^', c(401:405, paste0('I', seq(10, 16))))
    dyslipidemia_icd = paste0('^', c('272.0', '272.1', '272.2', '272.3', '272.4', 'E78.0', 'E78.1', 'E78.4', 'E78.5'))
    
    cad_icd = paste0('^', c(paste0(410:414), paste0('I', 20:25)))
    cvd_icd = paste0('^', c('433', '434', '435', '437.0', '437.1', '438', paste0('I', 63:66)))
    cm_icd  = paste0('^', c('425', '428',	'I42', 'I43', 'I50'))
    
    pad_icd = paste0('^', c('440', '441', '442', '444', '445', 'I70', 'I71', 'I72', 'I74', 'I75'))
    aud_icd = paste0('^', c(
      291, 303, '305.0', 357.5, 425.5, 535.3, '571.0', 571.1, 571.2, 571.3, 655.4, 760.71, '980.0', 
      980.1, 'E860.0', 'E860.1', 'E860.2', 'E860.9',	'F10', 'G62.1', 'G31.2', 'G72.1', 'I42.6', 'K29.2', 
      'K70.0', 'K70.1', 'K70.2', 'K70.3', 'K70.4', 'K70.9', 'K85.2', 'K86.0', 'Q86.0', 'P04.3', 'X45', 'Y15', 'X65' 
    ))
    ald_icd = paste0('^', c('571.0', 571.1, 571.2, 571.3, 'K70'))
    
  ## read in comorbidities, then divide into dx groups 
    
    dx_path = 'data/data-direct-exports/diagnosis-data/combined-dx/'
    comorbidity_data = load_filtered_dx(
      dx_path, c('hypertension', 'dyslipidemia', 'cad', 'cvd', 'cm', 'pad', 'aud', 'ald'), 
      list(hypertension_icd, dyslipidemia_icd, cad_icd, cvd_icd, cm_icd, pad_icd, aud_icd, ald_icd), verbose = TRUE
    )
    
    hypertension = comorbidity_data %>% filter(hypertension_status == 1) %>% select(-contains('status'))
    dyslipidemia = comorbidity_data %>% filter(dyslipidemia_status == 1) %>% select(-contains('status'))
    
    cad = comorbidity_data %>% filter(cad_status == 1) %>% select(-contains('status'))
    cvd = comorbidity_data %>% filter(cvd_status == 1) %>% select(-contains('status'))
    cm  = comorbidity_data %>% filter(cm_status == 1) %>% select(-contains('status'))
    
    pad = comorbidity_data %>% filter(pad_status == 1) %>% select(-contains('status'))
    aud = comorbidity_data %>% filter(aud_status == 1) %>% select(-contains('status'))
    ald = comorbidity_data %>% filter(ald_status == 1) %>% select(-contains('status'))
    
  
# preprocessing ----------------------------------------------------------------
  
  ## fix date variables 
  
    enc_data$AdmitDate <- as.Date(enc_data$AdmitDate, format = "%m/%d/%Y %H:%M")
    data$date.index <- as.Date(data$date.index, format = '%Y-%m-%d')
  
  ## match encounter dates to diagnoses 
    
    hypertension <- join_enc_date(hypertension, enc_data)
    dyslipidemia <- join_enc_date(dyslipidemia, enc_data)
    
    cad <- join_enc_date(cad, enc_data)
    cvd <- join_enc_date(cvd, enc_data)
    cm  <- join_enc_date(cm, enc_data)
    pad <- join_enc_date(pad, enc_data)
    aud <- join_enc_date(aud, enc_data)
    ald <- join_enc_date(ald, enc_data)
    
    
# define comorbidities ---------------------------------------------------------
    
  ## add diagnosis variables 
    
    working_data <- add_dx_data('hypertension', hypertension, data)
    working_data <- add_dx_data('dyslipidemia', dyslipidemia, working_data)
    working_data <- add_dx_data('cad', cad, working_data)
    working_data <- add_dx_data('cvd', cvd, working_data)
    working_data <- add_dx_data('cm', cm, working_data)
    working_data <- add_dx_data('pad', pad, working_data)
    working_data <- add_dx_data('aud', aud, working_data)
    working_data <- add_dx_data('ald', ald, working_data)
    

# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-04.csv', row.names = FALSE)
    
  