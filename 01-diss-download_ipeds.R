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

#HD2020
#IC2020_AY
#ADM2020
#SFA2021

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
hd <- read_csv(
  file.path(ipeds_data_dir, str_c('hd', year, '.csv')),
  col_types = cols_only(
    UNITID = 'c', INSTNM = 'c', ADDR = 'c', CITY = 'c', STABBR = 'c', ZIP = 'c', FIPS = 'c',
    OPEID = 'c', OPEFLAG = 'c', WEBADDR = 'c', SECTOR = 'c', ICLEVEL = 'c', CONTROL = 'c', LOCALE = 'c',
    C15BASIC = 'c', INSTSIZE = 'c', CBSA = 'c', COUNTYCD = 'c', COUNTYNM = 'c', LONGITUD = 'n', LATITUDE = 'n'
  )
) %>% 
  rename_with(str_to_lower)

#IC
ic_ay <- read_csv(
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
adm <- read_csv(
  file.path(ipeds_data_dir, str_c('adm', year, '.csv')),
  col_types = cols_only(
    UNITID = 'c', ADMCON1 = 'c', ADMCON2 = 'c', APPLCN = 'n', APPLCNM = 'n', APPLCNW = 'n', ADMSSN = 'n', ADMSSNM = 'n', ADMSSNW = 'n',
  )
) %>% 
  rename_with(str_to_lower)

#SFA
sfa <- read_csv(
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
# Renaming column (dummy) values
# ------------------

get_col_values <- function(objectname, filename, varnames) {
  dict <- read_excel(file.path(ipeds_dict_dir, str_c(filename, '.xlsx')), sheet = 'Frequencies')
  
    for (v in varnames) {
    d <- dict %>% filter(varname==str_to_upper(v))
    objectname[[v]] <- plyr::mapvalues(objectname[[v]], d$codevalue, d$valuelabel, warn_missing = F)      
    }
    objectname
}
hd <- get_col_values(hd, str_c('hd', year), c("sector", "locale", "c15basic", "iclevel", "instsize", "cbsa", "countycd","control","fips", "opeflag"))

# ------------------
# Edit column names
# ------------------

# Function to rename columns
get_col_names <- function(objectname, filename, info = '') {
  dict <- read_excel(file.path(ipeds_dict_dir, str_c(filename, '.xlsx')), sheet = 'varlist')
  
  col_names <- names(objectname)
  col_names <- plyr::mapvalues(col_names, dict$varname, dict$varTitle, warn_missing = F)
  
  c('UnitID', col_names[-1])
}


names(hd) <- get_col_names(hd, str_c('hd', year))
names(ic_ay) <- get_col_names(ic_ay, str_c('ic', year, '_ay'))
names(adm) <- get_col_names(adm, str_c('adm', year))
names(sfa) <- get_col_names(sfa, str_c('sfa', str_sub(year, 3, 4), str_sub(year+1, 3, 4)))

# --------------------
# Merge and save data
# --------------------

ipeds <- hd %>% 
  left_join(ic_ay, by = 'UnitID') %>% 
  left_join(adm, by = 'UnitID') %>% 
  left_join(sfa, by = 'UnitID') %>%
  rename (
  "Institution Name" = "instnm",
  "Institution Address" = "addr",
  "City of Institution" = "city", 
  "State Abbreviation" = "stabbr",
  "Web Address" = "webaddr", 
  "Sector of Institution" = "sector", 
  "Degree of Urbanization" = "locale",
  "Carnegie Classification 2015: Basic" = "c15basic",
  "Institution Size Category" = "instsize",
  "Longitude" = "longitud",
  "Latitude" = "latitude", 
  "Zip Code of Institution" = "zip",
  "FIPS State Code" = "fips",
  "Office of Postsecondary Education (OPE) ID Number" = "opeid",
  "OPE Title IV Eligibility Indicator Code" = "opeflag",
  "Level of Institution" = "iclevel",
  "Control of Institution" = "control",
  "Core Based Statistical Area (CBSA)" = "cbsa",
  "Fips County Code" = "countycd",
  "County Name" = "countynm",
  "In-State Cost of Attendance (On Campus)" = "instate_oncampus_coa", 
  "In-State Cost of Attendance (Off Campus)" = "instate_offcampus_coa", 
  "Published In-State Tuition" = "chg2at2",
  "Published In-State Fees" = "chg2af2",
  "Published Out-of-State Tuition" = "chg3at2",
  "Published Out-of-State Fees" = "chg3af2",
  "Books and Supplies" = "chg4ay2",
  "On Campus Room and Board" = "chg5ay2",
  "On Campus, Other Expenses" = "chg6ay2",
  "Off Campus (not with family) Room and Board" = "chg7ay2",
  "Off Campus (not with family) Other Expenses" = "chg8ay2",
  "Off campus (with family) Other Expenses" = "chg9ay2",
  "Secondary School GPA" = "admcon1",
  "Secondary School Rank" = "admcon2",
  "Applicants Total" = "applcn",
  "Applicants Men" = "applcnm",
  "Applicants Women" = "applcnw",
  "Admissions Total" = "admssn",
  "Admissions Men" = "admssnm",
  "Admissions Women" = "admssnw",
  "Number of Students in Fall Cohort" = "scfa1n",
  "Number of Students Awarded Grant Aid" = "agrnt_n",
  "Percent of Students Awarded Grant aid" = "agrnt_p",
  "Total Amount of Grant Aid Awarded to Students" = "agrnt_t",
  "Average Amount of Grant Aid Awarded to Students" = "agrnt_a",
  "Number of Students Awarded Federal Grant Aid" = "fgrnt_n",
  "Percent of Students Awarded Federal Grant Aid" = "fgrnt_p",
  "Total Amount of Federal Grant Aid Awarded to Students" = "fgrnt_t",
  "Average Amount of Federal Grant Aid Awarded to Students" = "fgrnt_a",
  "Number of Students Awarded Pell Grants" = "pgrnt_n",
  "Percent of Students Awarded Pell Grants" = "pgrnt_p",
  "Total Amount of Pell Grant Aid Awarded to Students" = "pgrnt_t",
  "Average amount of Pell Grant Aid Awarded to Students" = "pgrnt_a",
  "Number of Students Awarded Other Federal Grant Aid" = "ofgrt_n",
  "Percent of Students Awarded Other Federal Grant Aid" = "ofgrt_p",
  "Total Amount of Other Federal Grant Aid Awarded to full Students" = "ofgrt_t",
  "Average Amount of Other Federal Grant Aid Awarded to Students" = "ofgrt_a",
  "Number of Students Awarded State/Local Grant Aid" = "sgrnt_n",
  "Percent of Students Awarded State/Local Grant Aid" = "sgrnt_p",
  "Total amount of State/Local grant Aid Awarded to Students" = "sgrnt_t",
  "Average Amount of State/Local Grant Aid Awarded to Students" = "sgrnt_a",
  "Number of Students Awarded Institutional Grant Aid" = "igrnt_n",
  "Percent of Students Awarded Institutional Grant Aid" = "igrnt_p",
  "Total Amount of Institutional Grant Aid Awarded to Students" = "igrnt_t",
  "Average Amount of Institutional Grant Aid Awarded to Students" = "igrnt_a",
  "Total Number (of students) in All Income Levels" = "gis4n2",
  "Total Amount of Grant and Scholarship Aid Awarded, All Income Levels" = "gis4t2",
  "Average Amount of Grant and Scholarship Aid Awarded, All Income Levels" = "gis4a2",
  "Number (of students) in Income Level (0-30,000)" = "gis4n12",
  "Total Amount of Grant and Scholarship Aid Awarded, Income Level (0-30,000)" = "gis4t12",
  "Average Amount of Grant and Scholarship Aid Awarded, Income Level (0-30,000)" = "gis4a12",
  "Number (of students) in Income Level (30,001-48,000)" = "gis4n22",
  "Total Amount of Grant and Scholarship Aid Awarded, Income Level (30,001-48,000)" = "gis4t22",
  "Average Amount of Grant and Scholarship Aid Awarded, Income Level (30,001-48,000)" = "gis4a22",
  "Number (of students) in Income Level (48,001-75,000)" = "gis4n32",
  "Total Amount of Grant and Scholarship Aid Awarded, Income Level (48,001-75,000)" = "gis4t32",
  "Average Amount of Grant and Scholarship Aid Awarded, Income Level (48,001-75,000)" = "gis4a32",
  "Number (of students) in Income Level (75,001-110,000)" = "gis4n42",
  "Total Amount of Grant and Scholarship Aid Awarded, Income Level (75,001-110,000)" = "gis4t42",  
  "Average Amount of Grant and Scholarship Aid Awarded, Income Level (75,001-110,000)" = "gis4a42",
  "Number (of students) in Income Level (110,001 or more)" = "gis4n52",
  "Total Amount of Grant and Scholarship Aid Awarded, Income Level (110,001 or more)" = "gis4t52",
  "Average Amount of Grant and Scholarship Aid Awarded, Income Level (110,001 or more)" = "gis4a52",
  "Average Net Price for Students Awarded Grant or Scholarship Aid" = "npist2",
  "Average Net Price for Students Awarded Title IV Federal Financial Aid, Income Level (0-30,000)" = "npis412",
  "Average Net Price for Students Awarded Title IV Federal Financial Aid, Income Level (30,001-48,000)" = "npis422",
  "Average Net Price for Students Awarded Title IV Federal Financial Aid, Income Level (48,001-75,000)" = "npis432",
  "Average Net Price for Students Awarded Title IV Federal Financial Aid, Income Level (75,001-110,000)" = "npis442",
  "Average Net Price for Students Awarded Title IV Federal Financial Aid, Income Level (110,001 or more)" = "npis452" 
  )

write_csv(ipeds, file = 'ipeds.csv')
