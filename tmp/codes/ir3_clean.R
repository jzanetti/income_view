

get_ir3_clean_data <- function(raw_ir3_clean_data) {
  
  ir3_clean_2000_2014 <- raw_ir3_clean_data %>%
    filter(ir_ir3_return_period_date >= as.Date('2000-03-31') & ir_ir3_return_period_date <= as.Date('2014-03-31')) %>%
    rename(period = ir_ir3_return_period_date)
  
  ir3_clean_2015_2022 <- raw_ir3_clean_data %>%
    filter(ir_ir3_return_period_date >= as.Date('2015-03-31') & ir_ir3_return_period_date <= as.Date('2022-03-31')) %>%
    rename(period = ir_ir3_return_period_date)
  
  #ir3_clean_2015_2022 <- ir3_clean_2015_2022 %>%
  #  group_by(snz_ird_uid, period)
  
  ir3_clean_2015_2022 <- ir3_clean_2015_2022 %>%
    group_by(snz_ird_uid, period) %>%
    arrange(ir_ir3_ird_timestamp_date) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Get data for 2015-2018 from ir3_clean
  ir3_clean_2015_2018 <- ir3_clean_2015_2022 %>%
    filter(period <= as.Date('2018-03-31'))
  
  # Get data for 2019-2022
  ir3_clean_2019_2022 <- ir3_clean_2015_2022 %>%
    filter(period >= as.Date('2019-03-31') & period <= as.Date('2022-03-31')) %>%
    mutate(source = 'ir3_clean')

  ir3_clean_2000_2022 <- raw_ir3_clean_data %>%
    filter(ir_ir3_return_period_date >= as.Date('2000-03-31') & ir_ir3_return_period_date <= as.Date('2022-03-31')) %>%
    rename(period = ir_ir3_return_period_date) %>%
    group_by(snz_ird_uid, period) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  return(list(
    ir3_clean_2000_2022=ir3_clean_2000_2022,
    ir3_clean_2000_2014=ir3_clean_2000_2014,
    ir3_clean_2015_2022=ir3_clean_2015_2022,
    ir3_clean_2015_2018=ir3_clean_2015_2018,
    ir3_clean_2019_2022=ir3_clean_2019_2022
  ))
}
