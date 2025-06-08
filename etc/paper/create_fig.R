library(readxl)
library(dplyr) 
library(tidyr)
library(ggplot2)
library(ggthemes)
data_path <- "shiny/data_to_check - without_raw_data - v3.0.xlsx"

df_all <- read_excel(data_path, sheet = "data", skip = 1)
df_count <- read_excel(data_path, sheet = "data2", skip = 1)

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

all_percentage <- df_all %>%
  filter(name %in% c("capital", "total")) %>%
  group_by(name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  mutate(percentage = (capital / total) * 100) %>%
  pull(percentage)

df_processed2 <- df_all %>%
  # Filter for labour and total
  filter(name %in% c("labour", "total"), year == 2023) %>%
  # Group by age and name, summing value1 across all years
  group_by(age, name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  # Pivot to get labour and total in separate columns
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  # Calculate percentage
  mutate(percentage = (labour / total) * 100) %>%
  # Filter for relevant age groups (excluding "all" for bars)
  filter(age %in% age_order) %>%
  # Set age as factor with specified order
  mutate(age = factor(age, levels = age_order))

# Extract the percentage for age == "all" for the horizontal line
all_percentage2 <- df_all %>%
  filter(name %in% c("labour", "total")) %>%
  group_by(name) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  mutate(percentage = (labour / total) * 100) %>%
  pull(percentage)

# Create the bar chart
p1 <- ggplot(df_processed, aes(x = age, y = percentage)) +
  geom_bar(stat = "identity", fill = "#4F81BD") +
  geom_hline(yintercept = all_percentage, color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(aes(x = Inf, y = all_percentage, label = sprintf("All ages: %.1f%%", all_percentage)),
            hjust = 1.1, vjust = -0.5, color = "red", size = 5) +
  labs(
    title = "Percentage of Capital Income over Total Income \nby Age Group (2023)",
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
    title = "Percentage of Labour Income over Total Income \nby Age Group (2023)",
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
# Figure 4
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
  scale_fill_manual(values = c("capital" = "#4F81BD", "labour" = "#00B050"), 
                    labels = c("Capital", "Labour")) +
  labs(
    title = "Growth in Capital and Labour Income (2002 to 2018) by Age Group",
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
# Figure 5
# <><><><><><><><><><><><><><><><>
base_year <- 2002
# age_range <- c("<15", "15-25", "25-35", "35-45", "45-55", "55-65", ">=65")
age_range <- c("55-65", "65-70", ">=70")
var <- "labour"

df_value1 <- df_all[, c("year", "name", "age", "value")]
df_value1 <- df_value1 %>%
  filter(
    name %in% c(var, "total"), 
    year >= base_year & year <= 2018,
    age %in% age_range)

df_value1_share <- df_value1 %>%
  filter(name %in% c(var, "total")) %>%
  group_by(year, name) %>%
  summarize(all = sum(value), .groups = "drop")

if (var == "capital") {
  df_value1_share <- df_value1_share %>%
    pivot_wider(names_from = name, values_from = all) %>%
    mutate(percentage = (capital / total) * 100) %>%
    select(year, percentage)
  legend_loc <- c(0.25, 0.25)
} else if (var == "labour") {
  df_value1_share <- df_value1_share %>%
    pivot_wider(names_from = name, values_from = all) %>%
    mutate(percentage = (labour / total) * 100) %>%
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
    filter(name %in% c(var, "total")) %>%
    group_by(year, name) %>%
    summarize(all = sum(total), .groups = "drop")
  
  all_results[[index]] <- list(year = proc_year, percentage_fixed = 100.0 * proc_result$all[1] / proc_result$all[2])
  
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
    title = paste0(paste0("Percentage of ", var, " Income over Total Income",
                          "\n", "Age > 55")),
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


