
get_final_tax_income <- function(pts_ac_ir3_data_2000_2022, tax_summary_2000_2022_raw_data) {
 
  pts_ac_ir3_data_2000_2022_tmp <- pts_ac_ir3_data_2000_2022 %>%
    select(snz_ird_uid, period) %>%
    arrange(snz_ird_uid, period)
  
  # Identify records to be added
  tax_summary_to_add <- anti_join(tax_summary_2000_2022_raw_data, pts_ac_ir3_data_2000_2022_tmp, by = c("snz_ird_uid", "period")) %>%
    filter(snz_ird_uid != 0)
  
  # Combine datasets
  tax_income_2000_2022 <- bind_rows(
    tax_summary_to_add %>%
      select(snz_ird_uid, period, inc_tax_yr_sum_all_srces_tot_amt) %>%
      mutate(source = 'Tax Summary'),
    pts_ac_ir3_data_2000_2022 %>%
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
  
  tax_income_2000_2022 <- tax_income_2000_2022 %>%
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
  
  return (tax_income_2000_2022)
  
}