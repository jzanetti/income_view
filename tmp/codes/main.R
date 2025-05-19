library(DBI)
library(dplyr)
library(lubridate)
source("constant.R")
source("query.R")
source("ir3_clean.R")
source("ir3.R")
source("ac.R")
source("pts.R")
source("utils.R")
source("tax_income.R")
raw_data <- query_data(load_local_data = FALSE)

# <><><><><><><><><><><><>
# Get IR3 data
# <><><><><><><><><><><><>
ir3_clean_data <- get_ir3_clean_data(raw_data$ir3_clean)

ir3_data_2015_2018 <- get_ir3_data_2015_2018(
  raw_data$ir3_2013_2020, 
  ir3_clean_data$ir3_clean_2015_2018)

ir3_data_2013_2014 <- get_ir3_data_2013_2014(
  raw_data$ir3_2013_2014_new, 
  raw_data$ir3_2013_2014_old)

ir3_data_2000_2014 <- get_ir3_data_2000_2014(
  raw_data$ir3_2000_2012,
  ir3_clean_data$ir3_clean_2000_2014,
  ir3_data_2013_2014)


ir3_2000_2022_final <- get_ir3_data_2000_2022(
    ir3_data_2000_2014,
    ir3_data_2015_2018,
    ir3_clean_data$ir3_clean_2019_2022,
    ir3_clean_data$ir3_clean_2000_2022
) 

# <><><><><><><><><><><><>
# Get AC data
# <><><><><><><><><><><><>
ac_2019_2022_final <- get_ac_data_2019_2022(raw_data$ac_2019_2022)

# <><><><><><><><><><><><>
# Get PTS data
# <><><><><><><><><><><><>
pts_2000_2018_final <- get_pts_data_2000_2018(raw_data$pts_2000_2018)

# <><><><><><><><><><><><>
# Combine AC and PTS data
# <><><><><><><><><><><><>
pts_ac_ir3_data_2000_2022 <- combined_pts_ac_ir3_2000_2022(
  pts_2000_2018_final, ac_2019_2022_final, ir3_2000_2022_final)

# <><><><><><><><><><><><>
# Get final tax income
# <><><><><><><><><><><><>
tax_income_2000_2022 <- get_final_tax_income(pts_ac_ir3_data_2000_2022, raw_data$tax_summary_2000_2022)


# <><><><><><><><><><><><>
# Add personal information
# <><><><><><><><><><><><>
tax_income_2000_2022_final <- combine_personal_info_with_tax_income(raw_data$concordance, raw_data$personal_detail, tax_income_2000_2022)

# <><><><><><><><><><><><>
# Save data
# <><><><><><><><><><><><>
write.csv(tax_income_2000_2022_final, "tax_income_2000_2022_final.csv", row.names = FALSE)





