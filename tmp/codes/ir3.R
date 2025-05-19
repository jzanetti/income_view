
get_ir3_data_2000_2022 <- function(
    ir3_data_2000_2014,
    ir3_data_2015_2018,
    ir3_clean_2019_2022,
    ir3_clean_2000_2022
    ) {
  
  # Combine 3 datasets together to get a final dataset
  ir3_2000_2022_final <- bind_rows(
    ir3_data_2000_2014,
    ir3_data_2015_2018,
    ir3_clean_2019_2022
  ) %>%
    select(-snz_uid)
  
  # Excluded records
  excluded <- anti_join(ir3_clean_2000_2022, ir3_2000_2022_final, by = c("snz_ird_uid", "period"))
  
  # Update ir3_2000_2022_final with in_ir3_clean flag
  ir3_2000_2022_final <- left_join(
    ir3_2000_2022_final, ir3_clean_2000_2022 %>% select(snz_ird_uid, period), by = c("snz_ird_uid", "period")) %>%
    mutate(in_ir3_clean = ifelse(!is.na(period), 'Y', 'N'))
  
  
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
  
  return (ir3_2000_2022_final)
}


get_ir3_data_2000_2014 <- function(
    raw_ir3_2000_2012_data,
    ir3_clean_2000_2014,
    ir3_2013_2014) {

  ir3_2000_2012 <- raw_ir3_2000_2012_data %>%
    arrange(snz_ird_uid, period, ir_ir3_ird_timestamp_date) %>%
    group_by(snz_ird_uid, period) %>%
    filter(row_number() == n())
  
  # Combine adhoc data for 2000-2012 with 2012-2013
  ir3_2000_2014 <- bind_rows(
    ir3_2000_2012 %>% mutate(source = 'adhoc_2000_2014'),
    ir3_2013_2014
  )
  
  #ir3_2000_2014_new <- anti_join(
  #  ir3_clean, 
  #  select(ir3_2000_2014, snz_ird_uid, period), 
  #  by = c("snz_ird_uid", "period"))
  
  # Updated records for 2000-2014
  ir3_2000_2014_updated <- inner_join(
    ir3_clean_2000_2014, 
    select(ir3_2000_2014, snz_ird_uid, period, taxable_income = ir_ir3_taxable_income_amt, timestamp_date = ir_ir3_ird_timestamp_date), 
    by = c("snz_ird_uid", "period")) %>%
    mutate(taxable_income = ifelse(is.na(taxable_income), 0, taxable_income)) %>%
    filter(ir_ir3_taxable_income_amt != taxable_income & ir_ir3_ird_timestamp_date > timestamp_date) %>%
    select(-taxable_income, -timestamp_date)
  
  if (nrow(ir3_2000_2014_updated) > 0) {
    ir3_2000_2014 <- anti_join(
      ir3_2000_2014, ir3_2000_2014_updated, by = c("snz_ird_uid", "period"))
  }
  
  ir3_2000_2014_final <- bind_rows(
    ir3_2000_2014_updated %>% mutate(source = 'ir3_clean updated'),
    ir3_2000_2014
  )
  
  return (ir3_2000_2014_final)
}


ir3_data_2013_2014 <- get_ir3_data_2013_2014(
  raw_data$ir3_2013_2014_new, 
  raw_data$ir3_2013_2014_old)


get_ir3_data_2013_2014 <- function(raw_ir3_2013_2024_new, raw_ir3_2013_2024_old) {
  
  # this will return all rows from raw_ir3_2013_2020_data that do not 
  # have matching (snz_ird_uid + period) in raw_ir3_2000_2014_data
  ir3_2013_2014_new <- anti_join(raw_ir3_2013_2024_new, raw_ir3_2013_2024_old, by = c("snz_ird_uid", "period"))
  
  ir3_2013_2014_updated <- inner_join(
    raw_ir3_2013_2024_new, 
    select(raw_ir3_2013_2024_old, snz_ird_uid, period, ir_ir3_taxable_income_amt),
    by = c("snz_ird_uid", "period")) %>%
    mutate(ir_ir3_taxable_income_amt = ifelse(is.na(ir_ir3_taxable_income_amt), 0, ir_ir3_taxable_income_amt)) %>%
    filter(ir_ir3_taxable_income_amt != taxable_income) %>%
    select(-ir_ir3_taxable_income_amt)
  
  # Delete the updated records from ir3_2000_2014 data
  ir3_2013_2014 <- anti_join(raw_ir3_2013_2024_old, ir3_2013_2014_updated, by = c("snz_ird_uid", "period")) %>%
    filter(period >= as.Date('2013-03-31'))
  
  # Combine new and updated records
  ir3_2013_2014_new_updated <- bind_rows(ir3_2013_2014_updated, ir3_2013_2020_new) %>%
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
  
  ir3_2013_2014 <- bind_rows(
    mutate(ir3_2013_2014_new_updated, source="adhoc_2012_2020"),
    mutate(ir3_2013_2014, source="adhoc_2000_2014")
  ) %>%
    select(-ir_ir3_snz_unique_nbr, -ir_ir3_income_imp_ind)
  
  return (ir3_2013_2014)
  
  
}


get_ir3_data_2015_2018 <- function(raw_ir3_2013_2020_data, ir3_clean_2015_2018){

  ir3_2015_2018 <- raw_ir3_2013_2020_data %>%
    filter(period >= as.Date('2015-03-31') & period <= as.Date('2018-03-31')) %>%
    arrange(snz_ird_uid, period, timestamp) %>%
    group_by(snz_ird_uid, period) %>%
    filter(row_number() == n()) # only keep the latest entry for each record
  
  ir3_2015_2018 <- ir3_2015_2018 %>%
    mutate(adhoc_timestamp_date = as.Date(
      paste0(substr(timestamp, 1, 4), "-", substr(timestamp, 5, 6), "-", substr(timestamp, 7, 8))))

  # what's ir_ir3_ird_timestamp_date, and why we only want to keep people after 2020-09-21
  ir3_2015_2018_updated <- inner_join(ir3_clean_2015_2018, ir3_2015_2018, by = c("snz_ird_uid", "period")) %>%
    mutate(ir_ir3_taxable_income_amt = ifelse(is.na(ir_ir3_taxable_income_amt), 0, ir_ir3_taxable_income_amt)) %>%
    filter(ir_ir3_taxable_income_amt != taxable_income & ir_ir3_ird_timestamp_date >= as.Date('2020-09-21'))
  
  ir3_2015_2018 <- anti_join(ir3_2015_2018, ir3_2015_2018_updated, by = c("snz_ird_uid", "period"))
  
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
    mutate(ir_ir3_ird_timestamp_date = as.Date(
      paste0(substr(timestamp, 1, 4), "-", substr(timestamp, 5, 6), "-", substr(timestamp, 7, 8))))
  
  return (ir3_2015_2018)
  
}