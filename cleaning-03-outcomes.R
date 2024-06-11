# README -----------------------------------------------------------------------
# 
#   name: cleaning-03-outcomes.R
#   goal: prepare + merge outcome data by... 
#         - matching diagnosis data to encounters
#         - defining conditions (both death and non-death related)  
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)
  library(openxlsx)

  source('code/cleaning/cleaning-functions.R')

  data = read.csv('data/working-data/data-post-02.csv')
  enc_data = load_multiple_csv('data/data-direct-exports/encounter-data', verbose = TRUE)
  
    
# load diagnosis data ----------------------------------------------------------
    
  ## define ICD codes of interest
    
    cirr_icd = c(
      '571.2', '571.5', '571.6',	'K70.30', 'K70.31', 'K70.40', 'K70.41', 'K71.7', 
      'K74.3', 'K74.4', 'K74.5', 'K74.60', 'K74.69', 'K70.31', '572.2', 'K72.11', 
      'K72.91',  'K76.82', '572.4',	'K76.7', '573.5', 'K76.81', '572.3', 'K76.6', 
      '567.23', 'K65.2', '456.0', '456.1', '456.20', '456.21', 'I85', 'I86.4' 
    )
    
    lre_icd = c(
      '789.59', 'R18.8', '572.2', 'K72.11', 'K72.90', 'K72.91', '572.4',	'K76.7', 
      '573.5',	'K76.81', '567.23',	'K65.2', '456.0', '456.20', 'I85.01', 'I85.11', 
      '155.0',	'C22.0'
    )
    
  ## read in outcomes, then divide into dx groups 
    
    dx_path = 'data/data-direct-exports/diagnosis-data/combined-dx/'
    outcome_data = load_filtered_dx(dx_path, c('cirr', 'LRE'), list(cirr_icd, lre_icd), verbose = TRUE)
    
    cirr = outcome_data %>% filter(cirr_status == 1) %>% select(-contains('status'))
    lre  = outcome_data %>% filter(LRE_status == 1) %>% select(-contains('status'))
  
  
# preprocessing ----------------------------------------------------------------
    
  ## fix related condition variables (extract ICD code after numbers)
    
    working_data <- data %>% 
      mutate(across(contains('RelatedCOD'), function(x) gsub('\\d\\d([A-Z].*)', '\\1', x)))
    
  ## fix date variables 
    
    enc_data$AdmitDate <- as.Date(enc_data$AdmitDate, format = "%m/%d/%Y %H:%M")
    working_data$date.index <- as.Date(working_data$date.index, format = '%Y-%m-%d')
    
  ## match encounter dates to diagnoses 
    
    cirr <- join_enc_date(cirr, enc_data)    
    lre  <- join_enc_date(lre, enc_data)     
    
  ## LRE subcategories 
    
    ascites_icd <- c('789.59', 'R18.8')
    ascites <- lre %>% filter(grepl(ascites_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    he_icd <- c('572.2', 'K72.11', 'K72.90', 'K72.91')
    he <- lre %>% filter(grepl(he_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    hrs_icd <- c('572.4', 'K76.7')
    hrs <- lre %>% filter(grepl(hrs_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    hps_icd <- c('573.5', 'K76.81')
    hps <- lre %>% filter(grepl(hps_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    sbp_icd <- c('567.23', 'K65.2')
    sbp <- lre %>% filter(grepl(sbp_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    evb_icd <- c('456.0', '456.20', 'I85.01', 'I85.11')
    evb <- lre %>% filter(grepl(evb_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    hcc_icd <- c('155.0', 'C22.0')
    hcc <- lre %>% filter(grepl(hcc_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    decomp_icd <- c(ascites_icd, he_icd, hrs_icd, hps_icd, sbp_icd, evb_icd)
    decomp <- lre %>% filter(grepl(decomp_icd %>% paste0(collapse = '|'), TermCodeMapped))
    
    
# define diagnoses -------------------------------------------------------------
    
  ## non-death outcomes 
  
    working_data <- add_dx_data('cirrhosis', cirr, working_data)
    working_data <- add_dx_data('lre', lre, working_data)
    working_data <- add_dx_data('ascites', ascites, working_data)
    working_data <- add_dx_data('he', he, working_data)
    working_data <- add_dx_data('hrs', hrs, working_data)
    working_data <- add_dx_data('hps', hps, working_data)
    working_data <- add_dx_data('sbp', sbp, working_data)
    working_data <- add_dx_data('evb', evb, working_data)
    working_data <- add_dx_data('hcc', hcc, working_data)
    working_data <- add_dx_data('decomp', decomp, working_data)
    
  ## death and related conditions 
    
    working_data <- working_data %>% 
      mutate(
        # nonspecific death variables 
        death.status = ifelse(is.na(date.death), 0, 1), 
        death.time = ifelse(death.status == 1, 
          difftime(date.death, date.index, units = 'days'),
          difftime(date.last.any, date.index, units = 'days')
        ), 
        # specific causes of death 
        death.liver.status = ifelse(if_any(select(., UnderlyingCOD_ICD10:RelatedCOD7_ICD10) %>% colnames(), function(x) grepl('^K7|C155|C155\\.0|C155\\.2', x)), 1, 0), 
        death.cancer.status = ifelse(if_any(select(., UnderlyingCOD_ICD10:RelatedCOD7_ICD10) %>% colnames(), function(x) grepl('^C', x)), 1, 0), 
        death.cv.status = ifelse(if_any(select(., UnderlyingCOD_ICD10:RelatedCOD7_ICD10) %>% colnames(), function(x) grepl('^I', x)), 1, 0)
      )

    
# export -----------------------------------------------------------------------
    
  write.csv(working_data, 'data/working-data/data-post-03.csv', row.names = FALSE)
  
  