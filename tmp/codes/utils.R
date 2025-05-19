
combine_personal_info_with_tax_income <- function(concordance_raw_data, personal_detail_raw_data, tax_income_2000_2022) {
  
  # Merge temp and concordance data
  tax_income_2000_2022_with_concordance <- left_join(tax_income_2000_2022, concordance_raw_data, by = "snz_ird_uid")
  
  tax_income_2000_2022_with_personal_details <- left_join(tax_income_2000_2022_with_concordance, personal_detail_raw_data, by = "snz_uid") %>%
    mutate(details = ifelse(is.na(snz_spine_ind), 'N', 'Y'))
  
  return (tax_income_2000_2022_with_personal_details)
}

combined_pts_ac_ir3_2000_2022 <- function(pts_2000_2018_data, ac_2019_2022_data, ir3_data_2000_2022) {
  
  pts_ac_2000_2022 <- bind_rows(
    pts_2000_2018_data %>%
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
    ac_2019_2022_data %>%
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
  
  pts_ac_2000_2022 <- pts_ac_2000_2022 %>%
    arrange(snz_ird_uid, period) %>%
    distinct()
  
  
  # Identify records not in IR3
  pts_ac_to_add <- anti_join(pts_ac_2000_2022, ir3_data_2000_2022, by = c("snz_ird_uid", "period"))
  
  # Combine datasets
  pts_ac_ir3_2000_2022 <- bind_rows(
    ir3_data_2000_2022 %>%
      mutate(
        processing_date = as.Date(processing_date, origin = "1970-01-01"),
        #ir_ir3_ird_timestamp_date = as.Date(ir_ir3_ird_timestamp_date, origin = "1970-01-01"),
        date = ifelse(source == 'adhoc_2013_2020', processing_date, ir_ir3_ird_timestamp_date)),
    pts_ac_to_add %>%
      mutate(
        ir_ac_processing_date = as.Date(ir_ac_processing_date, origin = "1970-01-01"),
        ir_pts_timestamp_date = as.Date(ir_pts_timestamp_date, origin = "1970-01-01"),
        date = ifelse(source == 'AC', ir_ac_processing_date, ir_pts_timestamp_date))
  ) %>%
    mutate(date = as.Date(date, origin = "1970-01-01"))

  return (pts_ac_ir3_2000_2022)
}
