
get_pts_data_2000_2018 <- function(raw_pts_data_2000_2018) {
  pts <- raw_pts_data_2000_2018 %>%
    arrange(ir_pts_return_period_date, snz_uid, ir_pts_timestamp_date) %>%
    group_by(ir_pts_return_period_date, snz_uid) %>%
    filter(row_number() == n())
  
  PTS_taxyr2000_18 <- pts %>%
    filter(ir_pts_return_period_date <= as.Date('2018-03-31')) %>%
    mutate(ir_pts_taxable_inc_amt = ifelse(ir_pts_taxable_inc_amt < 0, 0, ir_pts_taxable_inc_amt),
           drop = ifelse(ir_pts_taxable_inc_amt < 1, 'Y', NA))
  
  return (PTS_taxyr2000_18)
}