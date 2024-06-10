# README -----------------------------------------------------------------------
# 
#   file: cleaning-00-cohort-info.R
#   goal: extract identifiers for steatosis cohort 
# 
# ------------------------------------------------------------------------------

# packages and data ------------------------------------------------------------

  library(tidyverse)

  data = read.csv('data/steato-all-bystudy-2024-05-13.csv')
  
  
# extract info -----------------------------------------------------------------
  
  all_MRNs <- data['MRN'] %>% unique()
  num_MRNs <- nrow(all_MRNs)
  
  first_half_MRNs  <- all_MRNs[1:(num_MRNs/2), , drop = FALSE]
  second_half_MRNs <- all_MRNs[(num_MRNs/2 + 1):num_MRNs, , drop = FALSE]
    # splitting into two because dx_all DD query keeps failing 
  

# export -----------------------------------------------------------------------
  
  openxlsx::write.xlsx(first_half_MRNs, 'data/MRN-upload-files/first-half-MRNs.xlsx', rowNames = FALSE)
  openxlsx::write.xlsx(second_half_MRNs, 'data/MRN-upload-files/second-half-MRNs.xlsx', rowNames = FALSE)
  
  
  
  