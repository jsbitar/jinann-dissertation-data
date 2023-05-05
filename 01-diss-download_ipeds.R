# Download downloadipeds.R & ipeds_file_list.txt from https://github.com/btskinner/downloadipeds
# Change out_dir on line 61 in downloadipeds.R and create that directory as needed
# Edit ipeds_file_list.txt to include desired files
# See IPEDS source here: https://nces.ed.gov/ipeds/datacenter/datafiles.aspx
# Run downloadipeds.R, which will download zipped data and dictionary
 

library(tidyverse)
library(readxl)

# ------------------
# Unzip IPEDS files
# ------------------

# HD2020
# IC2020_AY
# ADM2020
# SFA2021

ipeds <- readLines('./ipeds_file_list.txt')
ipeds <- ipeds[ipeds != '' & !grepl('^#', ipeds)]

ipeds_data_dir <- file.path('ipeds', 'data')
ipeds_dict_dir <- file.path('ipeds', 'dictionary')

for (i in ipeds) {
  unzip(zipfile = file.path(ipeds_data_dir, paste0(i, '.zip')), exdir = ipeds_data_dir)
  unzip(zipfile = file.path(ipeds_dict_dir, paste0(i, '_Dict.zip')), exdir = ipeds_dict_dir)
}


# -------------
# Read in data
# -------------

year <- 2020

#HD
hd2020 <- read_csv(
  file.path(ipeds_data_dir, str_c('hd', year, '.csv')),
  col_types = cols_only(
    UNITID = 'c', INSTNM = 'c', ADDR = 'c', CITY = 'c', STABBR = 'c', ZIP = 'c', FIPS = 'c',
    OPEID = 'c', OPEFLAG = 'c', WEBADDR = 'c', SECTOR = 'c', ICLEVEL = 'c', CONTROL = 'c', LOCALE = 'c',
    C15BASIC = 'c', INSTSIZE = 'c', CBSA = 'c', COUNTYCD = 'c', COUNTYNM = 'c', LONGITUD = 'n', LATITUDE = 'n'
  )
) %>% 
  rename_with(str_to_lower)

#IC
ic2020_ay <- read_csv(
  file.path(ipeds_data_dir, str_c('ic', year, '_ay.csv')),
  col_types = cols_only(
    UNITID = 'c', CHG2AT2 = 'n', CHG2AF2 = 'n', CHG3AT2 = 'n', CHG3AF2 = 'n',
    CHG4AY2 = 'n', CHG5AY2 = 'n', CHG6AY2 = 'n',
    CHG7AY2 = 'n', CHG8AY2 = 'n', CHG9AY2 = 'n'
  ),
  na = '.'
) %>% 
  rename_with(str_to_lower) %>% 
  mutate(
    instate_oncampus_coa = chg2at2 + chg4ay2 + chg5ay2 + chg6ay2,
    instate_offcampus_coa = chg2at2 + chg4ay2 + chg7ay2 + chg8ay2
  )

#ADM
adm2020 <- read_csv(
  file.path(ipeds_data_dir, str_c('adm', year, '.csv')),
  col_types = cols_only(
    UNITID = 'c', ADMCON1 = 'c', ADMCON2 = 'c', APPLCN = 'n', APPLCNM = 'n', APPLCNW = 'n', ADMSSN = 'n', ADMSSNM = 'n', ADMSSNW = 'n',
  )
) %>% 
  rename_with(str_to_lower)

#SFA
sfa2021 <- read_csv(
  file.path(ipeds_data_dir, str_c('sfa', str_sub(year, 3, 4), str_sub(year+1, 3, 4), '.csv')),
  col_types = cols_only(
    UNITID = 'c', SCFA1N = 'n',
    
    # Grant aid
    AGRNT_N = 'n', AGRNT_P = 'n', AGRNT_T = 'n', AGRNT_A = 'n',
    FGRNT_N = 'n', FGRNT_P = 'n', FGRNT_T = 'n', FGRNT_A = 'n',
    PGRNT_N = 'n', PGRNT_P = 'n', PGRNT_T = 'n', PGRNT_A = 'n',
    OFGRT_N = 'n', OFGRT_P = 'n', OFGRT_T = 'n', OFGRT_A = 'n',
    SGRNT_N = 'n', SGRNT_P = 'n', SGRNT_T = 'n', SGRNT_A = 'n',
    IGRNT_N = 'n', IGRNT_P = 'n', IGRNT_T = 'n', IGRNT_A = 'n',
    
    # Grant and scholarship aid by income band
    GIS4N2 = 'n', GIS4T2 = 'n', GIS4A2 = 'n',
    GIS4N12 = 'n', GIS4T12 = 'n', GIS4A12 = 'n',
    GIS4N22 = 'n', GIS4T22 = 'n', GIS4A22 = 'n',
    GIS4N32 = 'n', GIS4T32 = 'n', GIS4A32 = 'n',
    GIS4N42 = 'n', GIS4T42 = 'n', GIS4A42 = 'n',
    GIS4N52 = 'n', GIS4T52 = 'n', GIS4A52 = 'n',
    
    # Average net price by income band
    NPIST2 = 'n', NPIS412 = 'n', NPIS422 = 'n', NPIS432 = 'n', NPIS442 = 'n', NPIS452 = 'n'
  )
) %>% 
  rename_with(str_to_lower)

# ------------------
# Edit column names
# ------------------

# Function to rename columns
get_col_names <- function(filename, info = '') {
  dict <- read_excel(file.path(ipeds_dict_dir, str_c(filename, '.xlsx')), sheet = 'varlist')
  
  col_names <- names(get(filename))
  col_names <- plyr::mapvalues(col_names, dict$varname, dict$varTitle, warn_missing = F)
  col_names <- str_c(col_names, ' (', str_to_upper(filename), info, ')')
  
  c('UnitID', col_names[-1])
}


names(hd2020) <- get_col_names('hd2020')
names(ic2020_ay) <- get_col_names('ic2020_ay')
names(adm2020) <- get_col_names('adm2020')
names(sfa2021) <- get_col_names('sfa2021')

# --------------------
# Merge and save data
# --------------------

ipeds <- hd2020 %>% 
  left_join(ic2020_ay, by = 'UnitID') %>% 
  left_join(adm2020, by = 'UnitID') %>% 
  left_join(sfa2021, by = 'UnitID')

write_csv(ipeds, file = 'ipeds.csv')
