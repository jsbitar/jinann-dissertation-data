# See College Scorecard source here: https://collegescorecard.ed.gov/data/
# R library to download data: https://github.com/btskinner/rscorecard
# Sign up for API key: https://api.data.gov/signup/


library(tidyverse)
library(rscorecard)


# -----------------------
# Search data dictionary
# -----------------------

# Search keyword
sc_dict('debt')

# Search by source
sc_dict('NSLDS', search_col = 'source')

# Save dictionary as dataframe
sc_dict('30,000')
lo_inc_vars <- sc_dict('30,000', limit = Inf, return_df = T)


# ----------------
# Fetch variables
# ----------------

# Store API key
sc_key('SkePia32HkA5G7rxnh721Awf0BwP562763QVr2tS')

sc_dict("debt")

sc <- sc_init() %>% 
  sc_select(
    unitid, mn_earn_wne_inc1_p10, mn_earn_wne_inc1_p6, md_earn_wne_inc1_p6, md_earn_wne_inc1_p8, md_earn_wne_inc1_p10, lo_inc_debt_mdn, lo_inc_debt_n
  ) %>%
  sc_year("latest") %>%
  sc_get()

# ------------------
# Edit column names
# ------------------

# Get column names from data dictionary
names(sc)

sc_desc <- plyr::mapvalues(names(sc), lo_inc_vars$varname, lo_inc_vars$description, warn_missing = F)
sc_source <- plyr::mapvalues(names(sc), lo_inc_vars$varname, lo_inc_vars$source, warn_missing = F)

sc_names <- str_c(sc_desc, ' (', sc_source, ')')
names(sc) <- c('UnitID', sc_names[-1])


# ----------
# Save data
# ----------

write_csv(sc, file = 'scorecard.csv')
