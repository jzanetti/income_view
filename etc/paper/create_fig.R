library(readxl)
library(dplyr)
library(ggplot2)

data_path <- "shiny/data_to_check - without_raw_data - v3.0.xlsx"

df_all <- read_excel(data_path, sheet = "data", skip = 1)

df_all <- df_all %>%
  rename(
    "year" = "...1",
    "name" = "...2",
    "age" = "...3",
    "value1" = "To release...4",
    "value2" = "To release...5"
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
  geom_line() +
  geom_point() +
  labs(
    title = "Share of Total Taxable Income for Age >=55 by Year",
    y = "Percentage of Total Taxable Income (%)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave("etc/paper/taxable_income_plot_share.png", plot = p, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure 2
# <><><><><><><><><><><><><><><><>
# Define the desired order of age groups
age_order <- c("<15", "15-25", "25-35", "35-45", "45-55", "55-65", "65-70", ">=70")

# Process the data
df_processed <- df_all %>%
  # Filter for capital and total
  filter(name %in% c("capital", "total"), year == 2023) %>%
  # Group by age and name, summing value1 across all years
  group_by(age, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  # Pivot to get capital and total in separate columns
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  # Calculate percentage
  mutate(percentage = (capital / total) * 100) %>%
  # Filter for relevant age groups (excluding "all" for bars)
  filter(age %in% age_order) %>%
  # Set age as factor with specified order
  mutate(age = factor(age, levels = age_order))

# Extract the percentage for age == "all" for the horizontal line
all_percentage <- df_all %>%
  filter(name %in% c("capital", "total")) %>%
  group_by(name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  mutate(percentage = (capital / total) * 100) %>%
  pull(percentage)

# Create the bar chart
p <- ggplot(df_processed, aes(x = age, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = all_percentage, color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = Inf, y = all_percentage, label = sprintf("All ages: %.1f%%", all_percentage)),
            hjust = 1.1, vjust = -0.5, color = "red", size = 4) +
  labs(
    title = "Percentage of Capital Income over Total Income by Age Group (2023)",
    x = "Age Group",
    y = "Percentage of Capital Income (%)"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("etc/paper/capital_income_plot_share.png", plot = p, width = 8, height = 6, dpi = 300)


# <><><><><><><><><><><><><><><><>
# Figure 3
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
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(
    title = "Percentage of Labour Income over Total Income by Year",
    x = "Year",
    y = "Percentage of Labour Income (%)"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave("etc/paper/labour_income_plot_share_all_age.png", plot = p, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure 4
# <><><><><><><><><><><><><><><><>
# Define the desired order of age groups
age_order <- c("15-25", "25-35", "35-45", "45-55", "55-65", "65-70", ">=70", "all")

# Process the data
df_processed <- df_all %>%
  # Filter for capital, labour, and specified years
  filter(name %in% c("capital", "labour"), year %in% c(2002, 2018)) %>%
  # Filter for specified age groups
  filter(age %in% age_order) %>%
  # Group by year, name, and age to sum value1
  group_by(year, name, age) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  # Pivot to get 2002 and 2018 values in separate columns
  tidyr::pivot_wider(names_from = year, values_from = value) %>%
  # Calculate percentage growth: ((2018 - 2002) / 2002) * 100
  mutate(growth = ((`2018` - `2002`) / `2002`) * 100) %>%
  # Set age as factor with specified order
  mutate(age = factor(age, levels = age_order)) %>%
  # Select relevant columns
  select(age, name, growth)


p <- ggplot(df_processed, aes(x = age, y = growth, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("capital" = "steelblue", "labour" = "darkgreen"), 
                    labels = c("Capital", "Labour")) +
  labs(
    title = "Growth in Capital and Labour Income (2002 to 2018) by Age Group",
    x = "Age Group",
    y = "Growth (%)",
    fill = "Income Type"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("etc/paper/growth.png", plot = p, width = 8, height = 6, dpi = 300)

# <><><><><><><><><><><><><><><><>
# Figure 5
# <><><><><><><><><><><><><><><><>

df_value <- df_all[, c("year", "name", "age", "value2")]
df_value <- df_value %>%
  filter(name == "capital")
df_count <- 
