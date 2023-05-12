source('scd_cog_prelim.R')
library(tidyverse)

#import dataset from scd_cog_prelim and select CCID, Age, Sex, Hand, Coil, MT_TR, and SCD columns
df_old_imaging <- read_csv('old_homeint_and_standard.csv') %>% 
  select('CCID', 'Age', 'Sex', 'Hand', 'Coil', 'MT_TR', 'SCD') %>%
  rename('participant_id'='CCID', 'gender_text'='Sex') %>% rename_with(~ tolower(.x))

#import dwi participant list and possible data issues, then join with df_old
dwi <- read_tsv('dwi_participants.tsv') %>% select('participant_id', 'gender_code', 'tiv_cubicmm')
dwi$participant_id <- gsub('sub-', '', dwi$participant_id)
dwi_data_issues <- read_csv('dwi_data_issues.csv')
dwi <- left_join(dwi, dwi_data_issues)
df_old_dwi <- inner_join(df_old_imaging, dwi)
sum(df_old_dwi$scd)

df_old_dwi_bad <- df_old_dwi %>% filter(!is.na(data_qual_notes))
sum(df_old_dwi_bad$scd)

#there are 325 participants over age 55 with DWI available. 127 are SCD. 8 of them have data quality issues
#with DWI, and 1 of the participants with DWI data qual issues is SCD.

#import mti participants list and join with df old. The join with df_old_dwi.
mti <- read_tsv('mti_participants.tsv') %>% select('participant_id')
mti_data_issues <- read_csv('mti_data_issues.csv')
mti_issues_merge <- left_join(mti, mti_data_issues) 
sum(!is.na(mti_issues_merge$mti_qual_notes)) #no data qual issues for old mti data
df_old_mti <- inner_join(df_old_imaging, mti)
sum(df_old_mti$scd)

#there are 307 participants over age 55 with MTI data available. 120 are SCD.

#count how many participants have mti and how many have both dwi and mti
df_old_dwi_mti <- inner_join(df_old_dwi, mti)
sum(df_old_dwi_mti$scd)

df_old_dwi_mti_bad <- df_old_dwi_mti %>% filter(!is.na(data_qual_notes))
sum(df_old_dwi_mti_bad$scd)

#there are 306 participants over age 55 with both MTI and DWI data available. 119 are SCD.
#7 of these participants have DWI data qual issues, and 1 of the participants with data qual
#issues is SCD. 

write.csv(df_old_dwi, file="old_dwi.csv", fileEncoding = 'UTF-8', row.names = FALSE)
