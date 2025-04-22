

INCOME_DATA_PATH <- "etc/IR_data_to_check - without_raw_data - update - v5.0.xlsx"
CPI_DATA_PATH_RBNZ <- "etc/CPI.xlsx"
HYEF_DATA_PATH_TSY <- "etc/hyefu24-charts-data.xlsx"
POP_PROJECTION_PATH <- "etc/national-population-projections-2022base-2073.xlsx"
WORLD_BANK_DATA_PATH <- "etc/Worldbank_extracted_data1.xlsx"



NAME_MAPS <- list(
  # --------------------
  # Total taxable income
  # --------------------
  "taxable_income" = list(
    name = "Total taxable income",
    details = NULL,
    group = "Total taxable income",
    source = "Combined"),
  "inc_tax_yr_sum_all_srces_tot_amt" = list(
    name = "Total taxable income",
    details = NULL,
    group = "Total taxable income",
    source = "Income Tax Year Summary"),
  "ir_taxable_inc_amt" = list(
    name = "Total taxable income", 
    details = NULL,
    group = "Total taxable income",
    source = "IR Autocalc and IR PTS"),
  "ir_ir3_taxable_income_amt" = list(
    name = "Total taxable income", 
    details = NULL,
    group = "Total taxable income",
    source = "IR3_keypoints and IR3 adhoc"),

  # --------------------
  # Taxed at source income
  # --------------------
  "ir_tot_gross_earnings_amt" = list(
    name = "'Taxed at source' income", 
    details = "Wage and Salary, Benefits, Pension etc.",
    group = "Taxed at source income",
    source = "IR Autocalc and IR PTS"),
  "ir_ir3_gross_earnings_407_amt" = list(
    name = "'Taxed at source' income", 
    details = "Wage and Salary, Benefits, Pension etc.",
    group = "Taxed at source income",
    source = "IR3_keypoints and IR3 adhoc"),
  
  # --------------------
  # Investment
  # --------------------
  "ir_tot_interest_amt" = list(
    name = "Interest income", 
    details = NULL,
    group = "Investment",
    source = "IR Autocalc and IR PTS"),
  "ir_ir3_gross_interest_amt" = list(
    name = "Interest income", 
    details = NULL,
    group = "Investment",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_tot_dividend_amt" = list(
    name = "Divident income", 
    details = NULL,
    group = "Investment",
    source = "IR Autocalc and IR PTS"),
  "ir_ir3_gross_dividend_amt" = list(
    name = "Divident income", 
    details = NULL,
    group = "Investment",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_net_rents_826_amt" = list(
    name = "Rental income",
    details = NULL,
    group = "Investment",
    source = "IR3_keypoints and IR3 adhoc"),

  # --------------------
  # Self-employment
  # --------------------
  "ir_ir3_tot_pship_income_amt" = list(
    name = "Partnership Income",
    details = NULL,
    group = "Self-employment",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_tot_sholder_salary_amt" = list(
    name = "Shareholder Income",
    details = NULL,
    group = "Self-employment",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_net_profit_amt" = list(
    name = "Net profit income (as sole traders)",
    details = NULL,
    group = "Self-employment",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_tot_wholding_paymnts_amt" = list(
    name = "Withholding Payments",
    details = NULL,
    group = "Self-employment",
    source = "IR3_keypoints and IR3 adhoc"),

  # --------------------
  # Others
  # --------------------
  "ir_ir3_tot_expenses_claimed_amt" = list(
    name = "Expenses Claimed",
    details = NULL,
    group = "Others",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_estate_trust_income_amt" = list(
    name = "Estate Trust Income",
    details = NULL,
    group = "Others",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_overseas_income_amt" = list(
    name = "Overseas Income",
    details = NULL,
    group = "Others",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_other_income_amt" = list(
    name = "Other Income",
    details = NULL,
    group = "Others",
    source = "IR3_keypoints and IR3 adhoc"),
  "ir_ir3_tot_rebate_amt" = list(
    name = "Rebate Income",
    details = NULL,
    group = "Others",
    source = "IR3_keypoints and IR3 adhoc")
)


# Initialize an empty list to store the result
NAME_MAPS_REVERSED <- list()

# Iterate over each key in NAME_MAPS
for (key in names(NAME_MAPS)) {
  source <- NAME_MAPS[[key]]$source
  # If source doesn't exist in the list, create a new vector
  if (is.null(NAME_MAPS_REVERSED[[source]])) {
    NAME_MAPS_REVERSED[[source]] <- key
  } else {
    # Append the key to the existing vector
    NAME_MAPS_REVERSED[[source]] <- c(NAME_MAPS_REVERSED[[source]], key)
  }
}

# Initialize an empty list to store the grouped results
NAME_MAPS_GROUPS <- list()

# Iterate over each item in NAME_MAPS
for (key in names(NAME_MAPS)) {
  group <- NAME_MAPS[[key]]$group
  NAME_MAPS_GROUPS[[group]] <- c(NAME_MAPS_GROUPS[[group]], key)
}


HEFU_DATA_CFG <- list(
  "Data 1.1" = list(
    names = list(
      "...1"="date",
      "$billions (2009/10 prices)...2"="Real GDP (in billions)",
      "$thousands (2009/10 prices)...3"="Real GDP per capita (in thousands)"
    ),
    skip=6
  ),
  "Data 1.2" = list(
    names = list(
      "...1"="date",
      "Net % of firms"="QSBO (Net % of firms)"
    ),
    skip=4
  ),
  "Data 1.3"=list(
    names = list(
      "...2"="date",
      "Headline"="CPI"
    ),
    skip = 4
  ),
  "Data 1.4"=list(
    names = list(
      "...2"="date",
      "%...3"="90 days interest rate"
    ),
    skip = 4
  ),
  "Data 1.5"=list(
    names = list(
      "...2"="date",
      "% of labour force...3"="Unemployment rate"
    ),
    skip = 4
  ),
  "Data 1.6"=list(
    names = list(
      "...1"="date",
      "$billions (2009/10 prices)...2"="Household expenditure (Goods)",
      "$billions (2009/10 prices)...3"="Household expenditure (Service)"
    ),
    skip = 4
  ),
  "Data 1.7"=list(
    names = list(
      "...2"="date",
      "Half Year Update"="Private consumption"
    ),
    skip = 4
  ),
  "Data 1.8"=list(
    names = list(
      "...2"="date",
      "Index...3"="House prices"
    ),
    skip = 4
  ),
  "Data 1.9"=list(
    names = list(
      "...2"="date",
      "$billions (2009/10 prices)...3"="Government consumption"
    ),
    skip = 4
  ),
  "Data 1.10"=list(
    names = list(
      "...2"="date",
      "Index...3"="Labour productivity"
    ),
    skip = 4
  ),
  "Data 1.14"=list(
    names = list(
      "...1"="date",
      "Index"="Trade policy uncertainty"
    ),
    skip = 4
  )
)




