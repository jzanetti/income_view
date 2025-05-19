

get_ac_data_2019_2022 <- function(raw_ac_2019_2022) {
  ac <- raw_ac_2019_2022 %>%
    arrange(ir_ac_return_period_date, snz_uid, ir_ac_processing_date) %>%
    group_by(ir_ac_return_period_date, snz_uid) %>%
    filter(row_number() == n())
  
  # Create final dataset for autocalc tax years 2019-2022
  #Autocalc_taxyr2019_22 <- ac %>%
  #  filter(!(ir_ac_taxble_incm_amt < 1 & ir_ac_tot_gross_incmn_amt < 1))
  Autocalc_taxyr2019_22 <- ac %>%
    mutate(drop = ifelse(ir_ac_taxble_incm_amt < 1 & ir_ac_tot_gross_incmn_amt < 1, "Y", NA))
  
  return (Autocalc_taxyr2019_22)
  
  
}