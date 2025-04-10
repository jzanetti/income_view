library(readxl)
library(ggplot2)
library(dplyr)
data_num_records <- read_excel("etc/taxable_income.xlsx", sheet = "Data1", skip=1)
data_value_records <- read_excel("etc/taxable_income.xlsx", sheet = "Data2", skip=1)
ggplot(data_num_records, aes(x = age, y = year, fill = count_r6_rr3)) +
  geom_tile() +  # or geom_raster()
  scale_fill_gradient(low = "blue", high = "red", name = "number of records") +  # customize color gradient
  labs(title = "The number of records", x = "Age", y = "Year") +
  theme_minimal()

total_records <- data_num_records %>% 
  group_by(year) %>% 
  summarise(x = sum(count_r6_rr3))

col_name <- "ir_tot_gross_earnings_amt"
