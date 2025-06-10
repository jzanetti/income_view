library(readxl)
library(dplyr) 
library(tidyr)
library(ggplot2)
data_path <- "shiny/data_to_check - without_raw_data - v3.0.xlsx"
data_path2 <- "etc/Companies and trust income statistics.xlsx"

df_all <- read_excel(data_path, sheet = "data", skip = 1)
df_count <- read_excel(data_path, sheet = "data2", skip = 1)
df_entity <- read_excel(data_path2, sheet = "Combined sheet", skip = 8)

df_entity <- df_entity[, c(
  "...1",
  "Trustee income...2",
  "All co.s credit minus debit divided by co. Rate...3",
  "Credit minus debit - close co.s divided by rate...4")]
  # "Credit minus debit - widely held divided by rate...5")]

df_entity <- df_entity %>%
  rename(
    "year" = "...1",
    "trustee" = "Trustee income...2",
    "company_all" = "All co.s credit minus debit divided by co. Rate...3",
    "company" = "Credit minus debit - close co.s divided by rate...4")
    #"Widely held company" = "Credit minus debit - widely held divided by rate...5")

scaler <- mean(df_entity$company / df_entity$company_all, na.rm = TRUE)

df_entity$company[is.na(df_entity$company)] <- df_entity$company_all[is.na(df_entity$company)] * scaler

df_entity <- df_entity[, c("year", "trustee", "company")]

million_flag <- 1000000
df_entity$trustee <- df_entity$trustee * million_flag
df_entity$company <- df_entity$company * million_flag
df_entity <- pivot_longer(
  df_entity,
  cols = c(trustee, company),
  names_to = "name",
  values_to = "all"
) %>%
  filter(year >= 2001)

df_all <- df_all %>%
  rename(
    "year" = "...1",
    "name" = "...2",
    "age" = "...3",
    "value1" = "To release...4",
    "value2" = "To release...5"
  )

df_count <- df_count %>%
  rename(
    "year" = "...1",
    "age" = "...2",
    "count" = "To release"
  )

df_all$value1 <- as.numeric(df_all$value1, na.rm=TRUE)
df_all$value2 <- as.numeric(df_all$value2, na.rm=TRUE)
df_all$value <- df_all$value1

# <><><><><><><><><><><><><><><><>
# Figure 1
# <><><><><><><><><><><><><><><><>
df_processed <- df_all %>%
  # Filter for 'total' name category
  filter(name == "total") %>%
  # Group by year to calculate total income for age > 55 and all ages
  group_by(year) %>%
  summarise(
    income_over_55 = sum(value[age %in% c("55-65", "65-70", ">=70")], na.rm = TRUE),
    income_all = sum(value, na.rm = TRUE)
  ) %>%
  # Calculate percentage
  mutate(percentage = (income_over_55 / income_all) * 100)


p <- ggplot(df_processed, aes(x = year, y = percentage)) +
  geom_line(linewidth = 1.2, color = "#4F81BD") +
  # geom_point(size = 3, color = "#4F81BD") +
  labs(
    title = "Share of Total Taxable Income for Age >=55 by Year",
    y = "Percentage of Total Taxable Income (%)",
    x = "Year"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5),
    panel.grid.minor = element_line(color = "#E8ECEF", linewidth = 0.25),
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),   # Increased title size
    axis.title = element_text(size = 16, face = "bold"),               # Increased axis title size
    axis.text = element_text(size = 14),                               # Increased axis text size
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("etc/paper/taxable_income_plot_share.png", plot = p, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure 2
# <><><><><><><><><><><><><><><><>
# Define the desired order of age groups
# age_order <- c("<15", "15-25", "25-35", "35-45", "45-55", "55-65", "65-70", ">=70")
age_order <- c("15-25", "25-35", "35-45", "45-55", "55-65", "65-70", ">=70")
# Process the data

df_processed <- df_all %>%
  # Filter for capital and total
  filter(name %in% c("capital", "labour"), year == 2023) %>%
  # Group by age and name, summing value1 across all years
  group_by(age, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  # Pivot to get capital and total in separate columns
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  # Calculate percentage
  mutate(percentage = (capital / (labour + capital)) * 100) %>%
  # Filter for relevant age groups (excluding "all" for bars)
  filter(age %in% age_order) %>%
  # Set age as factor with specified order
  mutate(age = factor(age, levels = age_order))

all_percentage <- df_all %>%
  filter(name %in% c("capital", "labour"), year == 2023, age == "all") %>%
  group_by(name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  mutate(percentage = (capital / (labour + capital)) * 100) %>%
  pull(percentage)

df_processed2 <- df_all %>%
  # Filter for labour and total
  filter(name %in% c("capital", "labour"), year == 2023) %>%
  # Group by age and name, summing value1 across all years
  group_by(age, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  # Pivot to get labour and total in separate columns
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  # Calculate percentage
  mutate(percentage = (labour / (labour + capital)) * 100) %>%
  # Filter for relevant age groups (excluding "all" for bars)
  filter(age %in% age_order) %>%
  # Set age as factor with specified order
  mutate(age = factor(age, levels = age_order))

# Extract the percentage for age == "all" for the horizontal line
all_percentage2 <- df_all %>%
  filter(name %in% c("capital", "labour"), year == 2023, age == "all") %>%
  group_by(name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  mutate(percentage = (labour / (labour + capital)) * 100) %>%
  pull(percentage)

# Create the bar chart
p1 <- ggplot(df_processed, aes(x = age, y = percentage)) +
  geom_bar(stat = "identity", fill = "#4F81BD") +
  geom_hline(yintercept = all_percentage, color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = Inf, y = all_percentage, label = sprintf("All ages: %.1f%%", all_percentage)),
            hjust = 1.1, vjust = -0.5, color = "red", size = 5) +
  labs(
    title = "Capital / (Capital + Labour) \nby Age Group (2023)",
    x = "Age Group",
    y = "Percentage of Capital Income (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5),
    panel.grid.minor = element_line(color = "#E8ECEF", linewidth = 0.25),
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),  # Left-aligned title
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave("etc/paper/capital_income_plot_share.png", plot = p1, width = 8, height = 6, dpi = 300)


p2 <- ggplot(df_processed2, aes(x = age, y = percentage)) +
  geom_bar(stat = "identity", fill = "#4F81BD") +
  geom_hline(yintercept = all_percentage2, color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = Inf, y = all_percentage2, label = sprintf("All ages: %.1f%%", all_percentage2)),
            hjust = 1.1, vjust = -0.5, color = "red", size = 5) +
  labs(
    title = "Labour / (Capital + Labour) \nby Age Group (2023)",
    x = "Age Group",
    y = "Percentage of Labour Income (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5),
    panel.grid.minor = element_line(color = "#E8ECEF", linewidth = 0.25),
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),  # Left-aligned title
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave("etc/paper/labour_income_plot_share.png", plot = p2, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure X (not used)
# <><><><><><><><><><><><><><><><>
df_processed <- df_all %>%
  filter(age == "all") %>%
  # Filter for labour and total
  filter(name %in% c("labour", "total")) %>%
  # Group by year and name, summing value1 across all age groups
  group_by(year, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  # Pivot to get labour and total in separate columns
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  # Calculate percentage
  mutate(percentage = (labour / total) * 100)

p <- ggplot(df_processed, aes(x = year, y = percentage)) +
  geom_line(linewidth = 1.2, color = "#4F81BD") +  # Bolder line with Excel-like blue
  # geom_point(size = 3, color = "#4F81BD") +       # Points to match line
  labs(
    title = "Percentage of Labour Income over Total Income by Year",
    x = "Year",
    y = "Percentage of Labour Income (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5),  # Light gray grid lines
    panel.grid.minor = element_line(color = "#E8ECEF", linewidth = 0.25), # Subtle minor grid
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),     # Left-aligned, larger title
    axis.title = element_text(size = 16, face = "bold"),                # Larger axis titles
    axis.text = element_text(size = 14),                                # Larger axis text
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("etc/paper/labour_income_plot_share_all_age.png", plot = p, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure 3
# <><><><><><><><><><><><><><><><>
# Define the desired order of age groups
age_order <- c("15-25", "25-35", "35-45", "45-55", "55-65", "65-70", ">=70")

# Process the data
df_processed <- df_all %>%
  # Filter for capital, labour, and specified years
  filter(name %in% c("capital", "labour"), year %in% c(2002, 2018)) %>%
  # Filter for specified age groups
  filter(age %in% age_order) %>%
  # Group by year, name, and age to sum value1
  group_by(year, name, age) %>%
  summarise(value2 = sum(value2, na.rm = TRUE)) %>%
  # Pivot to get 2002 and 2018 values in separate columns
  tidyr::pivot_wider(names_from = year, values_from = value2) %>%
  # Calculate percentage growth: ((2018 - 2002) / 2002) * 100
  mutate(growth = ((`2018` - `2002`) / `2002`) * 100) %>%
  # Set age as factor with specified order
  mutate(age = factor(age, levels = age_order)) %>%
  # Select relevant columns
  select(age, name, growth)


p <- ggplot(df_processed, aes(x = age, y = growth, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("capital" = "#4F81BD", "labour" = "#00B050"), 
                    labels = c("Capital", "Labour")) +
  labs(
    title = "Growth in Capital and Labour Average Income (2002 to 2018) by Age Group",
    x = "Age Group",
    y = "Growth (%)",
    fill = "Income Type"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5),  # Light gray grid lines
    panel.grid.minor = element_line(color = "#E8ECEF", linewidth = 0.25), # Subtle minor grid
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),     # Left-aligned, larger title
    axis.title = element_text(size = 16, face = "bold"),                # Larger axis titles
    axis.text = element_text(size = 14),                                # Larger axis text
    axis.text.x = element_text(angle = 45, hjust = 1),                  # Keep x-axis text angle
    legend.text = element_text(size = 12),                              # Larger legend text
    legend.title = element_text(size = 14, face = "bold"),              # Larger legend title
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave("etc/paper/growth.png", plot = p, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure 4
# <><><><><><><><><><><><><><><><>
base_year <- 2002
age_range <- c("<15", "15-25", "25-35", "35-45", "45-55", "55-65", ">=65")
# age_range <- c("55-65", "65-70", ">=70")
var <- "capital"

df_value1 <- df_all[, c("year", "name", "age", "value")]
df_value1 <- df_value1 %>%
  filter(
    name %in% c("labour", "capital"), 
    year >= base_year & year <= 2018,
    age %in% age_range)

df_value1_share <- df_value1 %>%
  filter(name %in% c("labour", "capital")) %>%
  group_by(year, name) %>%
  summarize(all = sum(value), .groups = "drop")

if (var == "capital") {
  df_value1_share <- df_value1_share %>%
    pivot_wider(names_from = name, values_from = all) %>%
    mutate(percentage = (capital / (labour + capital)) * 100) %>%
    select(year, percentage)
  legend_loc <- c(0.25, 0.25)
} else if (var == "labour") {
  df_value1_share <- df_value1_share %>%
    pivot_wider(names_from = name, values_from = all) %>%
    mutate(percentage = (labour / (labour + capital)) * 100) %>%
    select(year, percentage)
  legend_loc <- c(0.25, 0.92)
}

df_value2 <- df_all[, c("year", "name", "age", "value2")]

df_value_base <- df_value2 %>%
  filter(
    name %in% c("labour", "benefits", "capital", "total"), 
    year == base_year,
    age %in% age_range)

all_results <- list()
index <- 1
for (proc_year in base_year:2018){
  proc_df_count <- df_count %>%
    filter(
      year == proc_year,
      age %in% age_range)

  proc_df_value <- df_value_base %>%
    left_join(proc_df_count[, c("age", "count")], by = c("age")) %>%
    mutate(total = value2 * count)
  
  proc_result <- proc_df_value %>%
    filter(name %in% c("labour", "capital")) %>%
    group_by(year, name) %>%
    summarize(all = sum(total), .groups = "drop")
  
  if (var == "labour") {
    all_results[[index]] <- list(
      year = proc_year, 
      percentage_fixed = 100.0 * proc_result$all[2] / (proc_result$all[1] + proc_result$all[2]))
  } else if (var == "capital") {
    all_results[[index]] <- list(
      year = proc_year, 
      percentage_fixed = 100.0 * proc_result$all[1] / (proc_result$all[1] + proc_result$all[2]))
  }
  
  index <- index + 1
}

all_results <- bind_rows(all_results)

all_results <- all_results %>%
  left_join(df_value1_share, by = "year")

all_results <- all_results %>%
  pivot_longer(cols = c(percentage_fixed, percentage), 
               names_to = "type", 
               values_to = "percentage")

p <- ggplot(all_results, aes(x = year, y = percentage, color=type)) +
  geom_line(linewidth = 1.2) +  # Bolder line with Excel-like blue
  labs(
    title = paste0(paste0(var, " / (labour + capital income)")),
    x = "Year",
    y = paste0("Percentage of ", var, " Income (%)"),
    color = "Percentage Type"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5),  # Light gray grid lines
    panel.grid.minor = element_line(color = "#E8ECEF", linewidth = 0.25), # Subtle minor grid
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),     # Left-aligned, larger title
    axis.title = element_text(size = 16, face = "bold"),                # Larger axis titles
    axis.text = element_text(size = 14),                                # Larger axis text
    plot.margin = margin(10, 10, 10, 10),
    legend.position = legend_loc,  # Place legend inside plot (top-right)
    legend.justification = c(1, 1),   # Align legend to top-right corner
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),  # Optional: legend box
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
    legend.text = element_text(size = 10)  # Customize legend text
  )

ggsave(paste0("etc/paper/", var, "_income_plot_based_", base_year, ".png"), plot = p, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure 5
# <><><><><><><><><><><><><><><><>
df_value1 <- df_all[, c("year", "name", "age", "value")]
df_value1 <- df_value1 %>%
  filter(
    name %in% c("labour", "capital"), 
    year >= base_year & year <= 2023,
    age %in% age_range)

df_value1_all <- df_value1 %>%
  filter(name %in% c("labour", "capital")) %>%
  group_by(year, name) %>%
  summarize(all = sum(value), .groups = "drop")

df_value1_all <- bind_rows(df_value1_all, df_entity)
df_value1_all <- df_value1_all[order(df_value1_all$year, df_value1_all$name), ]
df_value1_all <- df_value1_all %>%
  filter(year >= 2001)

df_value1_share <- df_value1_all %>%
  pivot_wider(names_from = name, values_from = all) %>%
  mutate(
    percentage = (100 * labour / (labour + capital)),
    percentage2 = (100.0 * labour / (labour + capital + trustee + company)),
    ) %>%
  select(
    year, 
    percentage, 
    percentage2
    )

df_long <- pivot_longer(
  df_value1_share,
  cols = starts_with("percentage"),
  names_to = "type",
  values_to = "value"
)

legend_loc <- c(0.95, 0.25)
# Create the time series plot
p<-ggplot(data = df_long, aes(x = year, y = value, color = type)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Labour income share including estimated \nsheltered company and trustee income",
    x = "Year",
    y = "Percentage (%)",
    color = "Data Type"
  ) +
  scale_color_discrete(
    labels = c("percentage" = "Labour / (Labour + Capital)", 
               "percentage2" = "Labour / (Labour + Capital + Trustee + Company)"
               )  # Customize legend labels
  )+
  scale_y_continuous(limits = c(56, NA))+
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5),  # Light gray grid lines
    panel.grid.minor = element_line(color = "#E8ECEF", linewidth = 0.25), # Subtle minor grid
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),     # Left-aligned, larger title
    axis.title = element_text(size = 16, face = "bold"),                # Larger axis titles
    axis.text = element_text(size = 14),                                # Larger axis text
    plot.margin = margin(10, 10, 10, 10),
    legend.position = legend_loc,  # Place legend inside plot (top-right)
    legend.justification = c(1, 1),   # Align legend to top-right corner
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),  # Optional: legend box
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
    legend.text = element_text(size = 10)  # Customize legend text
  )

ggsave(paste0("etc/paper/entity_plot.png"), plot = p, width = 8, height = 6, dpi = 300)


