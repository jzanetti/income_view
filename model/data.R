

get_proj_data <- function(year, scaler) {
  df_value_less_15 <- read_excel(DATA_PATH2, sheet = "<15", skip = 1)
  df_value_15_39 <- read_excel(DATA_PATH2, sheet = "15-39", skip = 1)
  df_value_40_64 <- read_excel(DATA_PATH2, sheet = "40-64", skip = 1)
  df_value_65_above <- read_excel(DATA_PATH2, sheet = "65+", skip = 1)

  df_value_less_15 <- df_value_less_15[, c("...1", "50th\r\n(Median)")] 
  df_value_15_39 <- df_value_15_39[, c("...1", "50th\r\n(Median)")] 
  df_value_40_64 <- df_value_40_64[, c("...1", "50th\r\n(Median)")]
  df_value_65_above <- df_value_65_above[, c("...1", "50th\r\n(Median)")]
  
  df0 <- df_value_less_15 %>% mutate(age = "<15")
  df1 <- df_value_15_39 %>% mutate(age = "15-39")
  df2 <- df_value_40_64 %>% mutate(age = "40-64")
  df3 <- df_value_65_above %>% mutate(age = ">65")
  
  df <- bind_rows(df0, df1, df2, df3)
  
  df <- df %>%
    rename(count = `50th\r\n(Median)`,
           year = "...1")
  
  df$count <- df$count * 1000.0
  
  df <- df[df$year == year,]

  print(" - Prepare training dataset (onehot shot decoding for age) ...")
  dummies <- dummyVars(~ age, data = df)
  age_encoded <- predict(dummies, df)

  # Define the new range: [0.5 * min, 2 * max]
  scaler_max <- scaler$max
  count_scaled <- df$count / scaler_max
  
  print(" - Prepare training dataset (x and y) ...")
  age_encoded <- as.data.frame(age_encoded)
  age_encoded$count <- count_scaled
  
  x <- age_encoded
  
  return (x)
}

get_input_data <- function(map_new_age = FALSE) {

  df_value <- read_excel(DATA_PATH, sheet = "data", skip = 1)
  df_count <- read_excel(DATA_PATH, sheet = "data2", skip = 1)
  df_value <- df_value %>%
    rename(
      "year" = "...1",
      "name" = "...2",
      "age" = "...3",
      "income" = "To release...4",
      # "value2" = "To release...5"
    )
  
  df_value <- df_value[, c("year", "name", "age", "income")] %>%
    filter(name == "total")
  
  df_value <- df_value[, c("year", "age", "income")]
  
  df_count <- df_count %>%
    rename(
      "year" = "...1",
      "age" = "...2",
      "count" = "To release"
    )
  
  df_all <- merge(df_value, df_count, by = c("year", "age"))
  
  train_df <- df_all[, c("year", "age", "income", "count")]

  if (map_new_age) {
    train_df <- train_df %>%
      filter(age %in% c("<15", "15-25", "25-35", "35-45", "45-55", "55-65", ">=65"))
    
    # Function to map original age categories to new groups
    map_age_groups <- function(age) {
      case_when(
        age %in% c("15-25", "25-35") ~ "15-39",
        age %in% c("35-45", "45-55", "55-65") ~ "40-64",
        age %in% c(">=65") ~ ">65",
        age %in% c("<15") ~ "<15",
        TRUE ~ NA_character_ # Handle any unexpected values
      )
    }

    train_df <- train_df %>%
      mutate(new_age = map_age_groups(age)) %>% # Create new_age column
      filter(!is.na(new_age)) %>% # Remove any rows with NA (if any)
      group_by(year, new_age) %>% # Group by new age groups
      summarise(
        income = mean(income, na.rm = TRUE), # Aggregate income by mean
        count = sum(count, na.rm = TRUE) # Aggregate count by sum
      ) %>%
      rename(age = new_age) # Rename new_age to age for final output
    train_df <- train_df[, c("age", "income", "count")]
  }
  
  print(" - Prepare training dataset (onehot shot decoding for age) ...")
  dummies <- dummyVars(~ age, data = train_df)
  age_encoded <- predict(dummies, train_df)
  
  
  print(" - Prepare training dataset (normalization) ...")
  min_count <- min(train_df$count, na.rm = TRUE)
  max_count <- max(train_df$count, na.rm = TRUE)
  
  # Define the new range: [0.5 * min, 2 * max]
  # scaler_min <- 0.5 * min_count
  scaler_max <- 2 * max_count
  # count_scaled <- (train_df$count - scaler_min) / (scaler_max - scaler_min)
  count_scaled <- train_df$count / scaler_max

  print(" - Prepare training dataset (x and y) ...")
  age_encoded <- as.data.frame(age_encoded)
  age_encoded$count <- count_scaled
  
  x <- age_encoded
  y <- train_df$income
  trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
  
  total_income <- df_all %>%
    filter(age == "all")
  return(list(
    x = x, 
    y = y, 
    trainIndex=trainIndex, 
    scaler=list(max = scaler_max),
    total_income=total_income))
}