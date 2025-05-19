library(DBI)
library(dplyr)
conn_ir = DBI::dbConnect(
  odbc::odbc(),
  driver = "ODBC Driver 18 for SQL Server",
  TrustServerCertificate = "Yes",
  server = "PRTPRDSQL36.stats.govt.nz",
  database = "IDI_Clean_202403",
  Trusted_Connection = "Yes"
)


conn_adhocir = DBI::dbConnect(
  odbc::odbc(),
  driver = "ODBC Driver 18 for SQL Server",
  TrustServerCertificate = "Yes",
  server = "PRTPRDSQL36.stats.govt.nz",
  database = "IDI_Adhoc",
  Trusted_Connection = "Yes"
)


ONLY_TEST <- TRUE

# Load data from the database
if (ONLY_TEST) {
  ir3_clean <- dbGetQuery(conn_ir, "SELECT top 10000 * FROM ir_clean.ird_rtns_keypoints_ir3")
  pts <- dbGetQuery(conn_ir, "SELECT top 10000 * FROM ir_clean.ird_pts")
  #ir3_clean2 <- dbGetQuery(conn_ir, "SELECT top 10000 * FROM ir_clean.ird_rtns_keypoints_ir3 WHERE ir_ir3_return_period_date BETWEEN '2000-03-31' AND '2014-03-31'")
  #ir3_clean3 <- dbGetQuery(conn_ir, "SELECT top 10000 * FROM ir_clean.ird_rtns_keypoints_ir3 WHERE ir_ir3_return_period_date BETWEEN '2000-03-31' AND '2022-03-31'")
  ir3_2013_2020 <- dbGetQuery(conn_adhocir, "SELECT top 10000 * FROM clean_read_ir.ir_ir3_2013_to_2020")
  ir3_2000_2014 <- dbGetQuery(conn_adhocir, "SELECT top 10000 * FROM clean_read_ir.ir_ir3_2000_to_2014 WHERE ir_ir3_return_period_date IN ('2013-03-31', '2014-03-31')")
  ir3_2000_2012 <- dbGetQuery(conn_adhocir, "SELECT top 10000 * FROM clean_read_ir.ir_ir3_2000_to_2014 WHERE ir_ir3_return_period_date <= '2012-03-31'")
  ac <- dbGetQuery(conn_ir, "SELECT top 10000 snz_uid, snz_ird_uid, ir_ac_return_period_date, ir_ac_taxble_incm_amt, ir_ac_gross_sal_wages_amt, ir_ac_tot_gross_intrst_amt, ir_ac_tot_gross_dvdnd_amt, ir_ac_tot_gross_incmn_amt, ir_ac_processing_date FROM ir_clean.ird_autocalc_information WHERE ir_ac_return_period_date <= '2022-03-31'")
  tax_summary <- dbGetQuery(conn_ir, "SELECT top 10000 snz_ird_uid, inc_tax_yr_sum_year_nbr, inc_tax_yr_sum_all_srces_tot_amt FROM data.income_tax_yr_summary WHERE inc_tax_yr_sum_year_nbr BETWEEN 2000 AND 2022") %>%
    mutate(period = as.Date(paste0(inc_tax_yr_sum_year_nbr, "-03-31")))
  concordance <- dbGetQuery(conn_ir, "SELECT top 10000 snz_hes_uid, snz_ird_uid, snz_uid FROM security.concordance")
  personal_detail <- dbGetQuery(conn_ir, "SELECT top 10000 snz_spine_ind, snz_uid, snz_sex_gender_code, snz_birth_year_nbr, snz_birth_month_nbr, snz_birth_date_source_code, snz_ethnicity_grp1_nbr, snz_ethnicity_grp2_nbr, snz_ethnicity_grp3_nbr, snz_ethnicity_grp4_nbr, snz_ethnicity_grp5_nbr, snz_ethnicity_grp6_nbr, snz_ethnicity_source_code FROM data.personal_detail")
  
} else {
  ir3_clean <- dbGetQuery(conn_ir, "SELECT * FROM ir_clean.ird_rtns_keypoints_ir3")
  #ir3_clean2 <- dbGetQuery(conn_ir, "SELECT * FROM ir_clean.ird_rtns_keypoints_ir3 WHERE ir_ir3_return_period_date BETWEEN '2000-03-31' AND '2014-03-31'")
  #ir3_clean3 <- dbGetQuery(conn_ir, "SELECT * FROM ir_clean.ird_rtns_keypoints_ir3 WHERE ir_ir3_return_period_date BETWEEN '2000-03-31' AND '2022-03-31'")
  ir3_2013_2020 <- dbGetQuery(conn_adhocir, "SELECT * FROM clean_read_ir.ir_ir3_2013_to_2020")
  ir3_2000_2014 <- dbGetQuery(conn_adhocir, "SELECT * FROM clean_read_ir.ir_ir3_2000_to_2014 WHERE ir_ir3_return_period_date IN ('2013-03-31', '2014-03-31')")
  ir3_2000_2012 <- dbGetQuery(conn_adhocir, "SELECT * FROM clean_read_ir.ir_ir3_2000_to_2014 WHERE ir_ir3_return_period_date <= '2012-03-31'")
}

# Load data from the database
# ir3_2013_2020 <- sqlQuery(conn_adhocir, "SELECT * FROM clean_read_ir.ir_ir3_2013_to_2020 WHERE ir_ir3_return_period_date BETWEEN '2015-03-31' AND '2018-03-31'")

# <><><><><><><><><><><><><><><><>
# Step 1
# <><><><><><><><><><><><><><><><>
# Filter data for the period 2015-2022
ir3_clean <- ir3_clean %>%
  filter(ir_ir3_return_period_date >= as.Date('2015-03-31') & ir_ir3_return_period_date <= as.Date('2022-03-31')) %>%
  rename(period = ir_ir3_return_period_date)

# Sort data
ir3_clean <- ir3_clean %>%
  arrange(snz_ird_uid, period, ir_ir3_ird_timestamp_date)

# Remove duplicates
#temp <- ir3_clean %>%
#  group_by(snz_ird_uid, period) %>%
#  filter(!(first(period) & last(period)))


# Take the latest entry for each period
ir3_clean <- ir3_clean %>%
  group_by(snz_ird_uid, period) %>%
  filter(row_number() == n())


# Frequency table of periods
period_freq <- table(ir3_clean$period)

# Print frequency table
print(period_freq)

# Get data for 2019-2022
ir3_2019_2022_final <- ir3_clean %>%
  filter(period >= as.Date('2019-03-31') & period <= as.Date('2022-03-31')) %>%
  mutate(source = 'ir3_clean')

# Get data for 2015-2018 from adhocir
ir3_2013_2020 <- ir3_2013_2020 %>%
  filter(period >= as.Date('2015-03-31') & period <= as.Date('2018-03-31')) %>%
  arrange(snz_ird_uid, period, timestamp) %>%
  group_by(snz_ird_uid, period) %>%
  filter(row_number() == n())

# Get data for 2015-2018 from ir3_clean
ir3_clean_2015_2018 <- ir3_clean %>%
  filter(period <= as.Date('2018-03-31'))

# Frequency table for 2015-2018 data
period_freq_2015_2018 <- table(ir3_clean_2015_2018$period)


# <><><><><><><><><><><><><><><><>
# Step 2
# <><><><><><><><><><><><><><><><>
ir3_2015_2018_new <- anti_join(ir3_clean_2015_2018, ir3_2013_2020, by = c("snz_ird_uid", "period"))
period_freq_new <- table(ir3_2015_2018_new$period)

# Updated records for 2015-18
ir3_2015_2018_updated <- inner_join(ir3_clean_2015_2018, ir3_2013_2020, by = c("snz_ird_uid", "period")) %>%
  mutate(ir_ir3_taxable_income_amt = ifelse(is.na(ir_ir3_taxable_income_amt), 0, ir_ir3_taxable_income_amt)) %>%
  filter(ir_ir3_taxable_income_amt != taxable_income & ir_ir3_ird_timestamp_date >= as.Date('2020-09-21'))

# Frequency table of periods for updated records
period_freq_updated <- table(ir3_2015_2018_updated$period)
print(period_freq_updated)

# Drop the updated records from adhoc
ir3_2015_2018 <- anti_join(ir3_2013_2020, ir3_2015_2018_updated, by = c("snz_ird_uid", "period"))

# Frequency table of periods for remaining records
period_freq_remaining <- table(ir3_2015_2018$period)
print(period_freq_remaining)

# Rename common variables to be consistent
ir3_2015_2018 <- ir3_2015_2018 %>%
  rename(
    ir_ir3_location_nbr = location_number,
    ir_ir3_return_version_nbr = return_version,
    ir_ir3_tot_pship_income_amt = partnership_income,
    ir_ir3_tot_sholder_salary_amt = shareholder_salary,
    ir_ir3_net_profit_amt = selfemployed_income,
    ir_ir3_net_rents_826_amt = rents,
    ir_ir3_tot_wholding_paymnts_amt = withholding_payment,
    ir_ir3_tot_expenses_claimed_amt = total_expenses,
    ir_ir3_gross_earnings_407_amt = gross_salary,
    ir_ir3_gross_interest_amt = gross_interest,
    ir_ir3_gross_dividend_amt = gross_dividend,
    ir_ir3_estate_trust_income_amt = estate_income,
    ir_ir3_overseas_income_amt = overseas_income,
    ir_ir3_other_income_amt = other_income,
    ir_ir3_taxable_income_amt = taxable_income,
    ir_ir3_tax_on_taxable_income_amt = tax_on_taxable_income,
    ir_ir3_tot_rebate_amt = tax_credits,
    ir_ir3_sl_liable_income_amt = sl_liable_inc,
    ir_ir3_fam_sup_tax_crdt_entl_amt = ftc_entitlement_amt,
    ir_ir3_parent_tax_crdt_entl_amt = ptc_entitlement_amt,
    ir_ir3_child_tax_crdt_entl_amt = ctc_entitlement_amt,
    ir_ir3_family_tax_crdt_entl_amt = mftc_entitlement_amt,
    ir_ir3_in_work_payment_entl_amt = iwtc_entitlement_amt
  ) %>%
  mutate(ir_ir3_ird_timestamp_date = as.Date(paste0(substr(timestamp, 1, 4), "-", substr(timestamp, 5, 6), "-", substr(timestamp, 7, 8))))

# Combine all records
ir3_2015_2018_final <- bind_rows(
  ir3_2015_2018_new %>% mutate(source = 'ir3_clean new'),
  ir3_2015_2018_updated %>% mutate(source = 'ir3_clean updated'),
  ir3_2015_2018 %>% mutate(source = 'adhoc_2012_2020')
)

# Frequency table of periods and sources
period_source_freq <- table(ir3_2015_2018_final$period, ir3_2015_2018_final$source)
print(period_source_freq)

# <><><><><><><><><><><><><><><><>
# Step 3
# <><><><><><><><><><><><><><><><>
ir3_2013_2020 <- ir3_2013_2020 %>%
  mutate(adhoc_timestamp_date = as.Date(paste0(substr(timestamp, 1, 4), "-", substr(timestamp, 5, 6), "-", substr(timestamp, 7, 8))))

# Sort data
ir3_2013_2020 <- ir3_2013_2020 %>%
  arrange(snz_ird_uid, period, adhoc_timestamp_date)

ir3_2000_2014 <- ir3_2000_2014 %>%
  rename(period = ir_ir3_return_period_date) %>%
  arrange(snz_ird_uid, period, ir_ir3_ird_timestamp_date)

# Take the latest entry for each period
ir3_2013_2020 <- ir3_2013_2020 %>%
  group_by(snz_ird_uid, period) %>%
  filter(row_number() == n())

ir3_2000_2014 <- ir3_2000_2014 %>%
  group_by(snz_ird_uid, period) %>%
  filter(row_number() == n())

# New records for 2013-2014
ir3_2013_2014_new <- anti_join(ir3_2013_2020, ir3_2000_2014, by = c("snz_ird_uid", "period"))

# Updated records for 2013-2014
ir3_2013_2014_updated <- inner_join(
  ir3_2013_2020, 
  select(ir3_2000_2014, snz_ird_uid, period, ir_ir3_taxable_income_amt),
  by = c("snz_ird_uid", "period")) %>%
  mutate(ir_ir3_taxable_income_amt = ifelse(is.na(ir_ir3_taxable_income_amt), 0, ir_ir3_taxable_income_amt)) %>%
  filter(ir_ir3_taxable_income_amt != taxable_income) %>%
  select(-ir_ir3_taxable_income_amt)

# Delete the updated records from ir3_2000_2014 data
ir3_2013_2014 <- anti_join(ir3_2000_2014, ir3_2013_2014_updated, by = c("snz_ird_uid", "period")) %>%
  filter(period >= as.Date('2013-03-31'))

# Combine new and updated records
ir3_2013_2014_new_updated <- bind_rows(ir3_2013_2014_updated, ir3_2013_2014_new) %>%
  arrange(snz_ird_uid, period)

ir3_2013_2014_new_updated <- ir3_2013_2014_new_updated %>%
  rename(
    ir_ir3_location_nbr = location_number,
    ir_ir3_return_version_nbr = return_version,
    ir_ir3_tot_pship_income_amt = partnership_income,
    ir_ir3_tot_sholder_salary_amt = shareholder_salary,
    ir_ir3_net_profit_amt = selfemployed_income,
    ir_ir3_net_rents_826_amt = rents,
    ir_ir3_tot_wholding_paymnts_amt = withholding_payment,
    ir_ir3_tot_expenses_claimed_amt = total_expenses,
    ir_ir3_gross_earnings_407_amt = gross_salary,
    ir_ir3_gross_interest_amt = gross_interest,
    ir_ir3_gross_dividend_amt = gross_dividend,
    ir_ir3_estate_trust_income_amt = estate_income,
    ir_ir3_overseas_income_amt = overseas_income,
    ir_ir3_other_income_amt = other_income,
    ir_ir3_taxable_income_amt = taxable_income,
    ir_ir3_tax_on_taxable_income_amt = tax_on_taxable_income,
    ir_ir3_tot_rebate_amt = tax_credits,
    ir_ir3_sl_liable_income_amt = sl_liable_inc,
    ir_ir3_fam_sup_tax_crdt_entl_amt = ftc_entitlement_amt,
    ir_ir3_parent_tax_crdt_entl_amt = ptc_entitlement_amt,
    ir_ir3_child_tax_crdt_entl_amt = ctc_entitlement_amt,
    ir_ir3_family_tax_crdt_entl_amt = mftc_entitlement_amt,
    ir_ir3_in_work_payment_entl_amt = iwtc_entitlement_amt,
    ir_ir3_ird_timestamp_date = adhoc_timestamp_date
  )

# Select relevant columns
ir3_2013_2014_new_updated <- ir3_2013_2014_new_updated %>%
  select(
    snz_ird_uid,
    period,
    ir_ir3_location_nbr,
    ir_ir3_return_version_nbr,
    ir_ir3_tot_pship_income_amt,
    ir_ir3_tot_sholder_salary_amt,
    ir_ir3_net_profit_amt,
    ir_ir3_net_rents_826_amt,
    ir_ir3_tot_wholding_paymnts_amt,
    ir_ir3_tot_expenses_claimed_amt,
    ir_ir3_gross_earnings_407_amt,
    ir_ir3_gross_interest_amt,
    ir_ir3_gross_dividend_amt,
    ir_ir3_estate_trust_income_amt,
    ir_ir3_overseas_income_amt,
    ir_ir3_other_income_amt,
    ir_ir3_taxable_income_amt,
    ir_ir3_tax_on_taxable_income_amt,
    ir_ir3_tot_rebate_amt,
    ir_ir3_sl_liable_income_amt,
    ir_ir3_fam_sup_tax_crdt_entl_amt,
    ir_ir3_parent_tax_crdt_entl_amt,
    ir_ir3_child_tax_crdt_entl_amt,
    ir_ir3_family_tax_crdt_entl_amt,
    ir_ir3_in_work_payment_entl_amt,
    ir_ir3_ird_timestamp_date
  )

# <><><><><><><><><><><><><><><><>
# Step 4
# <><><><><><><><><><><><><><><><>
ir3_2013_2014 <- bind_rows(
  mutate(ir3_2013_2014_new_updated, source="adhoc_2012_2020"),
  mutate(ir3_2013_2014, source="adhoc_2000_2014")
) %>%
  select(-ir_ir3_snz_unique_nbr, -ir_ir3_income_imp_ind)


ir3_2013_2014 <- ir3_2013_2014 %>%
  arrange(snz_ird_uid, period)

# Frequency table of source and period
source_period_freq <- table(ir3_2013_2014$source, ir3_2013_2014$period)
print(source_period_freq)

# Data for 2000-2012
ir3_2000_2012 <- ir3_2000_2012 %>%
  rename(period = ir_ir3_return_period_date) %>%
  arrange(snz_ird_uid, period, ir_ir3_ird_timestamp_date) %>%
  group_by(snz_ird_uid, period) %>%
  filter(row_number() == n())

# Frequency table of periods for 2000-2012 data
period_freq_2000_2012 <- table(ir3_2000_2012$period)
print(period_freq_2000_2012)

# Combine adhoc data for 2000-2012 with 2012-2013
ir3_2000_2014 <- bind_rows(
  ir3_2000_2012 %>% mutate(source = 'adhoc_2000_2014'),
  ir3_2013_2014
)

# Frequency table of period and source for combined data
period_source_freq_combined <- table(ir3_2000_2014$period, ir3_2000_2014$source)
print(period_source_freq_combined)

# Sort combined data
ir3_2000_2014 <- ir3_2000_2014 %>%
  arrange(snz_ird_uid, period)

# Add records in ir3_clean for 2000 to 2014 that are not in the two adhoc datasets
#ir3_clean <- ir3_clean2 %>%
#  rename(period = ir_ir3_return_period_date) %>%
#  arrange(snz_ird_uid, period, ir_ir3_ird_timestamp_date)



# <><><><><><><><><><><><><><><><>
# Step 5
# <><><><><><><><><><><><><><><><>

ir3_clean <- ir3_clean %>%
  group_by(snz_ird_uid, period) %>%
  arrange(ir_ir3_ird_timestamp_date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Load data from the database
#ir3_clean <- ir3_clean %>%
#  rename(period = ir_ir3_return_period_date) %>%
#  arrange(snz_ird_uid, period, ir_ir3_ird_timestamp_date) %>%
#  group_by(snz_ird_uid, period) %>%
#  filter(row_number() == n())

ir3_2000_2014_new <- anti_join(
  ir3_clean, select(ir3_2000_2014, snz_ird_uid, period), by = c("snz_ird_uid", "period"))

# Frequency table of periods for new records
period_freq_new <- table(ir3_2000_2014_new$period)
print(period_freq_new)

# Updated records for 2000-2014
ir3_2000_2014_updated <- inner_join(
  ir3_clean, 
  select(ir3_2000_2014, snz_ird_uid, period, taxable_income = ir_ir3_taxable_income_amt, timestamp_date = ir_ir3_ird_timestamp_date), 
  by = c("snz_ird_uid", "period")) %>%
  mutate(taxable_income = ifelse(is.na(taxable_income), 0, taxable_income)) %>%
  filter(ir_ir3_taxable_income_amt != taxable_income & ir_ir3_ird_timestamp_date > timestamp_date) %>%
  select(-taxable_income, -timestamp_date)

# Remove updated records
ir3_2000_2014 <- anti_join(ir3_2000_2014, ir3_2000_2014_updated, by = c("snz_ird_uid", "period"))

# Combine all records
ir3_2000_2014_final <- bind_rows(
  ir3_2000_2014_updated %>% mutate(source = 'ir3_clean updated'),
  ir3_2000_2014_new %>% mutate(source = 'ir3_clean new'),
  ir3_2000_2014
)

# Frequency table of period and source for final data
period_source_freq_final <- table(ir3_2000_2014_final$period, ir3_2000_2014_final$source)
print(period_source_freq_final)

# Combine 3 datasets together to get a final dataset
ir3_2000_2022_final <- bind_rows(
  ir3_2000_2014_final,
  ir3_2015_2018_final,
  ir3_2019_2022_final
) %>%
  select(-snz_uid)

# Frequency table of period and source for combined final data
period_source_freq_combined_final <- table(ir3_2000_2022_final$period, ir3_2000_2022_final$source)
print(period_source_freq_combined_final)

# Sort combined final data
ir3_2000_2022_final <- ir3_2000_2022_final %>%
  arrange(snz_ird_uid, period)


# Display structure of final datasets
str(ir3_2000_2022_final)
str(ir3_2015_2018_final)
str(ir3_2000_2014_final)
str(ir3_2019_2022_final)

# data ir3_clean (rename=(ir_ir3_return_period_date = period)) ;
# set ir.ird_rtns_keypoints_ir3 (keep = snz_ird_uid ir_ir3_return_period_date ir_ir3_ird_timestamp_date) ;
# if ir_ir3_return_period_date >= '31MAR2000'D and ir_ir3_return_period_date <= '31MAR2022'D ;
# run ;
#
# proc sort data = ir3_clean ;
# by snz_ird_uid period ir_ir3_ird_timestamp_date ;
# run ;
#
# * take latest ;
# data ir3_clean ;
# set ir3_clean ;
# by snz_ird_uid period ;
# if last.period ;
# run ;
#
# proc freq data = ir3_clean ;
# tables period / missing ;
# run ;

ir3_clean <- dbGetQuery(conn_ir, "SELECT top 10000 * FROM ir_clean.ird_rtns_keypoints_ir3") %>%
  select(snz_ird_uid, period = ir_ir3_return_period_date, ir_ir3_ird_timestamp_date) %>%
  filter(period >= as.Date("2000-03-01") & period <= as.Date("2022-03-31"))

ir3_clean <-ir3_clean %>%
  group_by(snz_ird_uid, period) %>%
  slice_tail(n = 1) %>%
  ungroup()

period_freq_clean <- table(ir3_clean$period)
print(period_freq_clean)

# <><><><><><><><><><><><><><><><>
# Step 6
# <><><><><><><><><><><><><><><><>
#exclude <- ir3_clean %>%
#  left_join(select(ir3_2000_2022_final, snz_ird_uid, period),
#            by = c("snz_ird_uid", "period")) %>%
#  filter(!is.na(snz_ird_uid) & is.na(period.y))

#ir3_clean <- dbGetQuery(conn_ir, "SELECT top 10000 * FROM ir_clean.ird_rtns_keypoints_ir3 WHERE ir_ir3_return_period_date BETWEEN '2000-03-31' AND '2022-03-31'") %>%
#  rename(period = ir_ir3_return_period_date) %>%
#  arrange(snz_ird_uid, period, ir_ir3_ird_timestamp_date) %>%
#  group_by(snz_ird_uid, period) %>%
#  filter(row_number() == n())

# Excluded records
excluded <- anti_join(ir3_clean, ir3_2000_2022_final, by = c("snz_ird_uid", "period"))

# Update ir3_2000_2022_final with in_ir3_clean flag
ir3_2000_2022_final <- left_join(
  ir3_2000_2022_final, ir3_clean %>% select(snz_ird_uid, period), by = c("snz_ird_uid", "period")) %>%
  mutate(in_ir3_clean = ifelse(!is.na(period), 'Y', 'N'))

period_in_ir3_clean_freq <- table(ir3_2000_2022_final$period, ir3_2000_2022_final$in_ir3_clean)
print(period_in_ir3_clean_freq)

# Display structure of ir3_2000_2022_final
str(ir3_2000_2022_final)

# Adjust values for specific sources
ir3_2000_2022_final <- ir3_2000_2022_final %>%
  mutate(
    ir_ir3_tot_rebate_amt = ifelse(source == 'adhoc_2012_2020', ir_ir3_tot_rebate_amt * -1, ir_ir3_tot_rebate_amt),
    ir_ir3_tot_expenses_claimed_amt = ifelse(source == 'adhoc_2012_2020', ir_ir3_tot_expenses_claimed_amt * -1, ir_ir3_tot_expenses_claimed_amt),
    ir_ir3_tot_pship_income_amt = ifelse(is.na(ir_ir3_tot_pship_income_amt), 0, ir_ir3_tot_pship_income_amt),
    ir_ir3_tot_sholder_salary_amt = ifelse(is.na(ir_ir3_tot_sholder_salary_amt), 0, ir_ir3_tot_sholder_salary_amt),
    ir_ir3_net_profit_amt = ifelse(is.na(ir_ir3_net_profit_amt), 0, ir_ir3_net_profit_amt),
    ir_ir3_net_rents_826_amt = ifelse(is.na(ir_ir3_net_rents_826_amt), 0, ir_ir3_net_rents_826_amt),
    ir_ir3_tot_wholding_paymnts_amt = ifelse(is.na(ir_ir3_tot_wholding_paymnts_amt), 0, ir_ir3_tot_wholding_paymnts_amt),
    ir_ir3_tot_expenses_claimed_amt = ifelse(is.na(ir_ir3_tot_expenses_claimed_amt), 0, ir_ir3_tot_expenses_claimed_amt),
    ir_ir3_gross_earnings_407_amt = ifelse(is.na(ir_ir3_gross_earnings_407_amt), 0, ir_ir3_gross_earnings_407_amt),
    ir_ir3_gross_interest_amt = ifelse(is.na(ir_ir3_gross_interest_amt), 0, ir_ir3_gross_interest_amt),
    ir_ir3_gross_dividend_amt = ifelse(is.na(ir_ir3_gross_dividend_amt), 0, ir_ir3_gross_dividend_amt),
    ir_ir3_estate_trust_income_amt = ifelse(is.na(ir_ir3_estate_trust_income_amt), 0, ir_ir3_estate_trust_income_amt),
    ir_ir3_overseas_income_amt = ifelse(is.na(ir_ir3_overseas_income_amt), 0, ir_ir3_overseas_income_amt),
    ir_ir3_other_income_amt = ifelse(is.na(ir_ir3_other_income_amt), 0, ir_ir3_other_income_amt),
    ir_ir3_taxable_income_amt = ifelse(is.na(ir_ir3_taxable_income_amt), 0, ir_ir3_taxable_income_amt),
    ir_ir3_tax_on_taxable_income_amt = ifelse(is.na(ir_ir3_tax_on_taxable_income_amt), 0, ir_ir3_tax_on_taxable_income_amt),
    ir_ir3_tot_rebate_amt = ifelse(is.na(ir_ir3_tot_rebate_amt), 0, ir_ir3_tot_rebate_amt),
    ir_ir3_sl_liable_income_amt = ifelse(is.na(ir_ir3_sl_liable_income_amt), 0, ir_ir3_sl_liable_income_amt),
    ir_ir3_fam_sup_tax_crdt_entl_amt = ifelse(is.na(ir_ir3_fam_sup_tax_crdt_entl_amt), 0, ir_ir3_fam_sup_tax_crdt_entl_amt),
    ir_ir3_parent_tax_crdt_entl_amt = ifelse(is.na(ir_ir3_parent_tax_crdt_entl_amt), 0, ir_ir3_parent_tax_crdt_entl_amt),
    ir_ir3_child_tax_crdt_entl_amt = ifelse(is.na(ir_ir3_child_tax_crdt_entl_amt), 0, ir_ir3_child_tax_crdt_entl_amt),
    ir_ir3_family_tax_crdt_entl_amt = ifelse(is.na(ir_ir3_family_tax_crdt_entl_amt), 0, ir_ir3_family_tax_crdt_entl_amt),
    ir_ir3_in_work_payment_entl_amt = ifelse(is.na(ir_ir3_in_work_payment_entl_amt), 0, ir_ir3_in_work_payment_entl_amt),
    source = ifelse(source == 'adhoc_2012_2020', 'adhoc_2013_2020', source),
    ir_ir3_ird_timestamp_date = ifelse(source == 'adhoc_2013_2020', NA, ir_ir3_ird_timestamp_date)
  )

# Save final dataset
sarah_ir3_2000_2022_final_Mar2024 <- ir3_2000_2022_final
saveRDS(ir3_2000_2022_final, file = "sarah_ir3_2000_2022_final_Mar2024.rds")

# Sort and filter data
ac <- ac %>%
  arrange(ir_ac_return_period_date, snz_uid, ir_ac_processing_date) %>%
  group_by(ir_ac_return_period_date, snz_uid) %>%
  filter(row_number() == n())


# Filter records with specific criteria
temp <- ac %>%
  filter(ir_ac_taxble_incm_amt < 1 & ir_ac_tot_gross_incmn_amt >= 1)

# Create final dataset for autocalc tax years 2019-2022
#Autocalc_taxyr2019_22 <- ac %>%
#  filter(!(ir_ac_taxble_incm_amt < 1 & ir_ac_tot_gross_incmn_amt < 1))
Autocalc_taxyr2019_22 <- ac %>%
  mutate(drop = ifelse(ir_ac_taxble_incm_amt < 1 & ir_ac_tot_gross_incmn_amt < 1, "Y", NA))


# Save final dataset
saveRDS(Autocalc_taxyr2019_22, file = "Autocalc_taxyr2019_22.rds")

# <><><><><><><><><><><><><><><><>
# Step 7
# <><><><><><><><><><><><><><><><>
amt_format <- function(x) {
  ifelse(x < 0, '-ve   ',
         ifelse(x < 1, '  0   ', '   +ve'))
}

amtB_format <- function(x) {
  ifelse(x == 0, '           0',
         ifelse(x < 1, '          <1',
                ifelse(x < 10, '       1-<10',
                       ifelse(x < 100, '   10-<100  ',
                              ifelse(x < 1000, ' 100-<1000  ', '1000+       ')))))
}

# Apply custom formats and create frequency tables
#Autocalc_taxyr2019_22 <- Autocalc_taxyr2019_22 %>%
#  mutate(ir_ac_taxble_incm_amt_fmt = amtB_format(ir_ac_taxble_incm_amt))

# Sort and filter data
pts <- pts %>%
  arrange(ir_pts_return_period_date, snz_uid, ir_pts_timestamp_date) %>%
  group_by(ir_pts_return_period_date, snz_uid) %>%
  filter(row_number() == n())

# Frequency table of populated variables
freq_table3 <- table(pts$ir_pts_return_period_date, pts$ir_pts_tot_gross_earnings_amt, useNA = "ifany")
print(freq_table3)

# Create PTS dataset for tax years 2000-2018
PTS_taxyr2000_18 <- pts %>%
  filter(ir_pts_return_period_date <= as.Date('2018-03-31')) %>%
  mutate(ir_pts_taxable_inc_amt = ifelse(ir_pts_taxable_inc_amt < 0, 0, ir_pts_taxable_inc_amt),
         drop = ifelse(ir_pts_taxable_inc_amt < 1, 'Y', NA))


# Frequency table for PTS dataset
freq_table5 <- table(PTS_taxyr2000_18$ir_pts_return_period_date, PTS_taxyr2000_18$drop, useNA = "ifany")
print(freq_table5)

# Test dataset
test <- PTS_taxyr2000_18 %>%
  filter(drop == 'Y' & ir_pts_taxable_inc_amt != 0)

# Display structure of datasets
str(PTS_taxyr2000_18)
str(Autocalc_taxyr2019_22)


# <><><><><><><><><><><><><><><><>
# Step 8
# <><><><><><><><><><><><><><><><>
sarah_PTS_AC_2000_22_Mar2024 <- bind_rows(
  PTS_taxyr2000_18 %>%
    select(
      snz_uid, 
      snz_ird_uid,
      ir_pts_return_period_date,
      ir_pts_taxable_inc_amt,
      ir_pts_timestamp_date,
      ir_pts_tot_interest_amt,
      ir_pts_tot_dividend_amt,
      ir_pts_tot_gross_earnings_amt,
      drop) %>%
    rename(period = ir_pts_return_period_date, 
      ir_taxable_inc_amt = ir_pts_taxable_inc_amt,
      ir_tot_interest_amt = ir_pts_tot_interest_amt, 
      ir_tot_dividend_amt = ir_pts_tot_dividend_amt,
      ir_tot_gross_earnings_amt = ir_pts_tot_gross_earnings_amt, 
      ) %>%
    mutate(source = 'PTS'),
  Autocalc_taxyr2019_22 %>%
    select(
      snz_uid, 
      snz_ird_uid,
      ir_ac_return_period_date,
      ir_ac_taxble_incm_amt,
      ir_ac_tot_gross_intrst_amt,
      ir_ac_tot_gross_dvdnd_amt,
      ir_ac_gross_sal_wages_amt,
      ir_ac_processing_date,
      drop) %>%
    rename(
      period = ir_ac_return_period_date, 
      ir_taxable_inc_amt = ir_ac_taxble_incm_amt,
      ir_tot_interest_amt = ir_ac_tot_gross_intrst_amt, 
      ir_tot_dividend_amt = ir_ac_tot_gross_dvdnd_amt,
      ir_tot_gross_earnings_amt = ir_ac_gross_sal_wages_amt) %>%
    mutate(source = 'AC')
)

# Sort data
sarah_PTS_AC_2000_22_Mar2024 <- sarah_PTS_AC_2000_22_Mar2024 %>%
  arrange(snz_uid, period)

# Display structure of ir3_2000_2022_final_Mar2024
str(sarah_ir3_2000_2022_final_Mar2024)

# Frequency table of period and ir_ir3_taxable_income_amt
freq_table1 <- table(sarah_ir3_2000_2022_final_Mar2024$period, sarah_ir3_2000_2022_final_Mar2024$ir_ir3_taxable_income_amt)
print(freq_table1)

# Frequency table of period and source
freq_table2 <- table(sarah_ir3_2000_2022_final_Mar2024$period, sarah_ir3_2000_2022_final_Mar2024$source)
print(freq_table2)

# Define custom format
# zero_format <- function(x) {
#   ifelse(x < 0, '          -ve ',
#          ifelse(x == 0, '            0 ',
#                 ifelse(x < 1, '      0.01-<1 ', '     1+       ')))
# }
# 
# # Apply custom format and create frequency tables
# sarah_ir3_2000_2022_final_Mar2024 <- sarah_ir3_2000_2022_final_Mar2024 %>%
#   mutate(across(starts_with("ir_ir3_"), zero_format))
# 
# freq_table3 <- table(sarah_ir3_2000_2022_final_Mar2024$ir_ir3_tot_pship_income_amt, sarah_ir3_2000_2022_final_Mar2024$ir_ir3_net_profit_amt)
# print(freq_table3)

# Update drop flag
# sarah_ir3_2000_2022_final_Mar2024 <- sarah_ir3_2000_2022_final_Mar2024 %>%
#   mutate(drop = ifelse(
#     ir_ir3_taxable_income_amt >= 0 & ir_ir3_taxable_income_amt < 1 &
#       ir_ir3_tot_pship_income_amt == 0 & ir_ir3_tot_sholder_salary_amt == 0 &
#       ir_ir3_net_profit_amt == 0 & ir_ir3_net_rents_826_amt == 0 &
#       ir_ir3_tot_wholding_paymnts_amt == 0 & ir_ir3_tot_expenses_claimed_amt == 0 &
#       ir_ir3_gross_earnings_407_amt == 0 & ir_ir3_gross_interest_amt < 1 &
#       ir_ir3_gross_dividend_amt == 0 & ir_ir3_estate_trust_income_amt == 0 &
#       ir_ir3_overseas_income_amt == 0 & ir_ir3_other_income_amt == 0 &
#       ir_ir3_tot_rebate_amt == 0, 'Y', ' '))
# 
# # Frequency table of period and drop
# freq_table4 <- table(sarah_ir3_2000_2022_final_Mar2024$period, sarah_ir3_2000_2022_final_Mar2024$drop)
# print(freq_table4)

# Sort and remove duplicates
ir3_2000_2022_final <- sarah_ir3_2000_2022_final_Mar2024 %>%
  arrange(snz_ird_uid, period) %>%
  distinct()

PTS_AC_2000_22 <- sarah_PTS_AC_2000_22_Mar2024 %>%
  arrange(snz_ird_uid, period) %>%
  distinct()


# Identify records not in IR3
PTS_AC_too_add <- anti_join(PTS_AC_2000_22, ir3_2000_2022_final, by = c("snz_ird_uid", "period"))

# Combine datasets
combined <- bind_rows(
  ir3_2000_2022_final %>%
    mutate(
      processing_date = as.Date(processing_date, origin = "1970-01-01"),
      #ir_ir3_ird_timestamp_date = as.Date(ir_ir3_ird_timestamp_date, origin = "1970-01-01"),
      date = ifelse(source == 'adhoc_2013_2020', processing_date, ir_ir3_ird_timestamp_date)),
  PTS_AC_too_add %>%
    mutate(
      ir_ac_processing_date = as.Date(ir_ac_processing_date, origin = "1970-01-01"),
      ir_pts_timestamp_date = as.Date(ir_pts_timestamp_date, origin = "1970-01-01"),
      date = ifelse(source == 'AC', ir_ac_processing_date, ir_pts_timestamp_date))
) %>%
  mutate(date = as.Date(date, origin = "1970-01-01"))

# Sort combined dataset
combined2 <- combined %>%
  select(snz_ird_uid, period) %>%
  arrange(snz_ird_uid, period)

# <><><><><><><><><><><><><><><><>
# Step 9
# <><><><><><><><><><><><><><><><>
# Sort tax summary data
tax_summary <- tax_summary %>%
  arrange(snz_ird_uid, period)

# Identify records to be added
added <- anti_join(tax_summary, combined2, by = c("snz_ird_uid", "period")) %>%
  filter(snz_ird_uid != 0)

# Combine datasets
TaxInc_2000_2022 <- bind_rows(
  added %>%
    select(snz_ird_uid, period, inc_tax_yr_sum_all_srces_tot_amt) %>%
    mutate(source = 'Tax Summary'),
  combined %>%
    select(snz_ird_uid, period, source, drop, date, ir_tot_gross_earnings_amt, ir_tot_interest_amt, ir_tot_dividend_amt, ir_taxable_inc_amt,
           ir_ir3_tot_pship_income_amt, ir_ir3_tot_sholder_salary_amt, ir_ir3_net_profit_amt, ir_ir3_net_rents_826_amt, ir_ir3_tot_wholding_paymnts_amt,
           ir_ir3_tot_expenses_claimed_amt, ir_ir3_gross_earnings_407_amt, ir_ir3_gross_interest_amt, ir_ir3_gross_dividend_amt,
           ir_ir3_estate_trust_income_amt, ir_ir3_overseas_income_amt, ir_ir3_other_income_amt, ir_ir3_taxable_income_amt, ir_ir3_tot_rebate_amt)
) %>%
  mutate(taxable_income = case_when(
    source == 'Tax Summary' ~ inc_tax_yr_sum_all_srces_tot_amt,
    source %in% c('AC', 'PTS') ~ ir_taxable_inc_amt,
    TRUE ~ ir_ir3_taxable_income_amt
  ))


# <><><><><><><><><><><><><><><><>
# Step 10
# <><><><><><><><><><><><><><><><>
# Sort TaxInc_2000_2022 data
TaxInc_2000_2022 <- TaxInc_2000_2022 %>%
  arrange(snz_ird_uid, period)

# Create sarah.TaxInc_2000_2022_Mar2024 dataset
sarah_TaxInc_2000_2022_Mar2024 <- TaxInc_2000_2022 %>%
  filter(period >= as.Date('2000-03-31')) %>%
  mutate(drop = ifelse(
    ir_ir3_taxable_income_amt >= 0 & ir_ir3_taxable_income_amt < 1 &
      ir_ir3_tot_pship_income_amt == 0 & ir_ir3_tot_sholder_salary_amt == 0 &
      ir_ir3_net_profit_amt == 0 & ir_ir3_net_rents_826_amt == 0 &
      ir_ir3_tot_wholding_paymnts_amt == 0 & ir_ir3_tot_expenses_claimed_amt == 0 &
      ir_ir3_gross_earnings_407_amt == 0 & ir_ir3_gross_interest_amt < 1 &
      ir_ir3_gross_dividend_amt == 0 & ir_ir3_estate_trust_income_amt == 0 &
      ir_ir3_overseas_income_amt == 0 & ir_ir3_other_income_amt == 0 &
      ir_ir3_tot_rebate_amt == 0, 'Y', ' '))


# Sort concordance data
concordance <- concordance %>%
  arrange(snz_ird_uid)

# Sort sarah.TaxInc_2000_2022_Mar2024 data
temp <- sarah_TaxInc_2000_2022_Mar2024 %>%
  arrange(snz_ird_uid)


# Merge temp and concordance data
temp2 <- left_join(temp, concordance, by = "snz_ird_uid")

# Sort temp2 data
temp2 <- temp2 %>%
  arrange(snz_uid)


# Sort personal detail data
personal_detail <- personal_detail %>%
  arrange(snz_uid)

# Merge temp2 and personal detail data
sarah_TaxInc_2000_2022_Mar2024 <- left_join(temp2, personal_detail, by = "snz_uid") %>%
  mutate(details = ifelse(is.na(snz_spine_ind), 'N', 'Y'))
