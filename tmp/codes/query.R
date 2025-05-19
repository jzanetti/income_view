

query_data <- function(load_local_data = FALSE) {
  
  if (load_local_data) {
    load("testdata.RData")
    return(output)
  }

  if (ONLY_TEST) {
    ir3_keypoints <- dbGetQuery(CONN_IR, paste0("SELECT top ", SAMPLE_SIZE, " * FROM ir_clean.ird_rtns_keypoints_ir3"))
    personal_tax_summary <- dbGetQuery(CONN_IR, paste0(
      "SELECT top ", SAMPLE_SIZE, " * FROM ir_clean.ird_pts"))
    ir3_2013_2020 <- dbGetQuery(CONN_ADHOCIR, paste0(
      "SELECT top ", SAMPLE_SIZE, " * FROM clean_read_ir.ir_ir3_2013_to_2020"))
    ir3_2000_2014 <- dbGetQuery(CONN_ADHOCIR, paste0(
      "SELECT top ", SAMPLE_SIZE, " * FROM clean_read_ir.ir_ir3_2000_to_2014"))
    ir3_2000_2012 <- dbGetQuery(CONN_ADHOCIR, paste0(
      "SELECT top ", SAMPLE_SIZE, " * FROM clean_read_ir.ir_ir3_2000_to_2014"))
    automatic_calculation <- dbGetQuery(CONN_IR, paste0("SELECT top ", SAMPLE_SIZE, " snz_uid, snz_ird_uid, ir_ac_return_period_date, ir_ac_taxble_incm_amt, ir_ac_gross_sal_wages_amt, ir_ac_tot_gross_intrst_amt, ir_ac_tot_gross_dvdnd_amt, ir_ac_tot_gross_incmn_amt, ir_ac_processing_date FROM ir_clean.ird_autocalc_information"))
    tax_summary_2000_2022 <- dbGetQuery(
      CONN_IR, 
      paste0("SELECT top ", SAMPLE_SIZE, " snz_ird_uid, inc_tax_yr_sum_year_nbr, inc_tax_yr_sum_all_srces_tot_amt FROM data.income_tax_yr_summary WHERE inc_tax_yr_sum_year_nbr BETWEEN 2000 AND 2022")) %>%
      mutate(period = as.Date(paste0(inc_tax_yr_sum_year_nbr, "-03-31")))
    concordance <- dbGetQuery(
      CONN_IR, paste0("SELECT top ", SAMPLE_SIZE, " snz_hes_uid, snz_ird_uid, snz_uid FROM security.concordance"))
    personal_detail <- dbGetQuery(
      CONN_IR, paste0(
        "SELECT top ", SAMPLE_SIZE, 
        " snz_spine_ind, snz_uid, snz_sex_gender_code, snz_birth_year_nbr, snz_birth_month_nbr, snz_birth_date_source_code, snz_ethnicity_grp1_nbr, snz_ethnicity_grp2_nbr, snz_ethnicity_grp3_nbr, snz_ethnicity_grp4_nbr, snz_ethnicity_grp5_nbr, snz_ethnicity_grp6_nbr, snz_ethnicity_source_code FROM data.personal_detail")
      )
    
  } else {
    ir3_keypoints <- dbGetQuery(CONN_IR, "SELECT * FROM ir_clean.ird_rtns_keypoints_ir3")
    personal_tax_summary <- dbGetQuery(CONN_IR, "SELECT * FROM ir_clean.ird_pts")
    ir3_2013_2020 <- dbGetQuery(CONN_ADHOCIR, "SELECT * FROM clean_read_ir.ir_ir3_2013_to_2020")
    ir3_2000_2014 <- dbGetQuery(CONN_ADHOCIR, "SELECT * FROM clean_read_ir.ir_ir3_2000_to_2014 WHERE ir_ir3_return_period_date IN ('2013-03-31', '2014-03-31')")
    ir3_2000_2012 <- dbGetQuery(CONN_ADHOCIR, "SELECT * FROM clean_read_ir.ir_ir3_2000_to_2014 WHERE ir_ir3_return_period_date <= '2012-03-31'")
    automatic_calculation <- dbGetQuery(CONN_IR, "SELECT snz_uid, snz_ird_uid, ir_ac_return_period_date, ir_ac_taxble_incm_amt, ir_ac_gross_sal_wages_amt, ir_ac_tot_gross_intrst_amt, ir_ac_tot_gross_dvdnd_amt, ir_ac_tot_gross_incmn_amt, ir_ac_processing_date FROM ir_clean.ird_autocalc_information")
    tax_summary <- dbGetQuery(CONN_IR, "SELECT snz_ird_uid, inc_tax_yr_sum_year_nbr, inc_tax_yr_sum_all_srces_tot_amt FROM data.income_tax_yr_summary WHERE inc_tax_yr_sum_year_nbr BETWEEN 2000 AND 2022") %>%
      mutate(period = as.Date(paste0(inc_tax_yr_sum_year_nbr, "-03-31")))
    concordance <- dbGetQuery(CONN_IR, "SELECT snz_hes_uid, snz_ird_uid, snz_uid FROM security.concordance")
    personal_detail <- dbGetQuery(CONN_IR, "SELECT snz_spine_ind, snz_uid, snz_sex_gender_code, snz_birth_year_nbr, snz_birth_month_nbr, snz_birth_date_source_code, snz_ethnicity_grp1_nbr, snz_ethnicity_grp2_nbr, snz_ethnicity_grp3_nbr, snz_ethnicity_grp4_nbr, snz_ethnicity_grp5_nbr, snz_ethnicity_grp6_nbr, snz_ethnicity_source_code FROM data.personal_detail")
  }
  

  ir3_2000_2012 <- ir3_2000_2012 %>%
    filter(ir_ir3_return_period_date <= "2012-03-31") %>%
    rename(period = ir_ir3_return_period_date)
  
  ir3_2013_2014_old <- ir3_2000_2014 %>%
    filter(ir_ir3_return_period_date == "2013-03-31" | ir_ir3_return_period_date == "2014-03-31") %>%
    rename(period = ir_ir3_return_period_date)
  
  ir3_2013_2014_new <- ir3_2013_2020 %>%
    filter(period == "2013-03-31" | period == "2014-03-31") %>%
    mutate(
      adhoc_timestamp_date = mdy(
        paste(substr(timestamp, 5, 6), 
              substr(timestamp, 7, 8), 
              substr(timestamp, 1, 4))
        )
      )
  
  ac_2019_2022 <- automatic_calculation %>%
    filter(ir_ac_return_period_date <= "2022-03-31")

  # ir_ir3_return_period_date
  output <- list(
    ir3_clean=ir3_keypoints,
    ir3_2000_2012=ir3_2000_2012,
    ir3_2013_2020=ir3_2013_2020,
    ir3_2013_2014_old=ir3_2013_2014_old,
    ir3_2013_2014_new=ir3_2013_2014_new,
    ac_2019_2022=ac_2019_2022,
    pts_2000_2018=personal_tax_summary,
    tax_summary_2000_2022=tax_summary_2000_2022,
    concordance=concordance,
    personal_detail=personal_detail
  )
  
  save(output, file = "testdata.RData")
    
  return (output) 
}