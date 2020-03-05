
# Meta --------------------------------------------------------------------
## Author:        Ian McCarthy
## Date Created:  1/13/2020
## Date Edited:   2/14/2020
## Notes:         R file to build dataset on physician price updates (phased in from 2010-2013)


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

## set file paths
source('code/paths.R')

# Subfiles ----------------------------------------------------------------
source('code/1_APC_Data.R')
source('code/2_Physician_Prices.R')
source('code/3_Betos.R')


# Organize final data -----------------------------------------------------
final.data <- final.pfs.data %>%
  left_join(full.apc.data %>%
              select(hcpcs, year, priceff_opps=payment_rate, si, apc),
            by=c("hcpcs","year")) %>%
  left_join(final.betos.data %>%
              select(hcpcs, betos_cat), 
            by=c("hcpcs"))


write_tsv(final.data,path='data/PFS_update_data.txt',append=FALSE,col_names=TRUE)
write_rds(final.data,'data/PFS_update_data.rds')



