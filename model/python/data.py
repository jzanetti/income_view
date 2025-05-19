from pandas import read_excel, merge, pivot
from numpy import nan as numpy_nan


DATA_PATH = "/Users/sijinzhang/Github/income_view/etc/data_to_check - without_raw_data - v3.0.xlsx"
DATA_PATH2 = "/Users/sijinzhang/Github/income_view/etc/national-population-projections-2022base-2073.xlsx"



def get_proj_data(year, scaler) {
  # Read Excel files from different sheets
    df_value_less_15 = pd.read_excel(DATA_PATH2, sheet_name="<15", skiprows=1)
    df_value_15_39 = pd.read_excel(DATA_PATH2, sheet_name="15-39", skiprows=1)
    df_value_40_64 = pd.read_excel(DATA_PATH2, sheet_name="40-64", skiprows=1)
    df_value_65_above = pd.read_excel(DATA_PATH2, sheet_name="65+", skiprows=1)

    # Select relevant columns
    df_value_less_15 = df_value_less_15.iloc[:, [0, df_value_less_15.columns.get_loc("50th\n(Median)")]]
    df_value_15_39 = df_value_15_39.iloc[:, [0, df_value_15_39.columns.get_loc("50th\n(Median)")]]
    df_value_40_64 = df_value_40_64.iloc[:, [0, df_value_40_64.columns.get_loc("50th\n(Median)")]]
    df_value_65_above = df_value_65_above.iloc[:, [0, df_value_65_above.columns.get_loc("50th\n(Median)")]]

    # Add age column to each DataFrame
    df0 = df_value_less_15.assign(age="<15")
    df1 = df_value_15_39.assign(age="15-39")
    df2 = df_value_40_64.assign(age="40-64")
    df3 = df_value_65_above.assign(age=">65")

    # Concatenate DataFrames
    df = pd.concat([df0, df1, df2, df3], ignore_index=True)

    # Rename columns
    df = df.rename(columns={
        "50th\n(Median)": "count",
        df.columns[0]: "year"
    })

    # Scale count by 1000
    df["count"] = df["count"] * 1000.0

    # Filter by specified year
    df = df[df["year"] == year]

    print(" - Prepare training dataset (onehot encoding for age) ...")
    # One-hot encode age
    encoder = OneHotEncoder(sparse_output=False, handle_unknown='ignore')
    age_encoded = encoder.fit_transform(df[["age"]])
    age_encoded = pd.DataFrame(age_encoded, columns=encoder.get_feature_names_out(['age']))

    # Normalize count
    scaler_max = scaler["max"]
    count_scaled = df["count"] / scaler_max

    print(" - Prepare training dataset (x and y) ...")
    # Add scaled count to encoded age DataFrame
    age_encoded["count"] = count_scaled.values

    x = age_encoded

    return x
}


def get_input_data(map_new_age: bool =True, income_type: str = "total"):
    # Read Excel files
    df_value = read_excel(DATA_PATH, sheet_name="data", skiprows=1)
    df_count = read_excel(DATA_PATH, sheet_name="data2", skiprows=1)

    # Rename columns in df_value
    df_value = df_value.rename(columns={
        df_value.columns[0]: "year",
        df_value.columns[1]: "name",
        df_value.columns[2]: "age",
        df_value.columns[3]: "income"
    })

    # Filter and select columns for df_value
    df_value = df_value[df_value["name"] == income_type][["year", "age", "income"]]

    # Rename columns in df_count
    df_count = df_count.rename(columns={
        df_count.columns[0]: "year",
        df_count.columns[1]: "age",
        df_count.columns[2]: "count"
    })

    # Merge dataframes
    df_all = merge(df_value, df_count, on=["year", "age"])

    # Select columns for training
    df_all = df_all[["year", "age", "income", "count"]]

    scaler_max = None

    if map_new_age:
        # Filter specific age categories
        valid_ages = ["<15", "15-25", "25-35", "35-45", "45-55", "55-65", ">=65"]
        df_all = df_all[df_all["age"].isin(valid_ages)]

        # Function to map age groups
        def map_age_groups(age):
            if age in ["15-25", "25-35", "35-45", "45-55", "55-65"]:
                return "15-64"
            elif age == ">=65":
                return ">65"
            elif age == "<15":
                return "<15"
            else:
                return numpy_nan

        # Apply age mapping and aggregate
        df_all["new_age"] = df_all["age"].apply(map_age_groups)
        df_all = df_all.dropna(subset=["new_age"])
        df_all = (df_all.groupby(["year", "new_age"])
                   .agg({
                       "income": "mean",
                       "count": "sum"
                   })
                   .reset_index()
                   .rename(columns={"new_age": "age"}))
        df_all = df_all[["year", "age", "income", "count"]]

        df_pivot = df_all.pivot(index='year', columns='age', values='count')[['<15', '15-64', '>65']]
        
        # Calculate the sum of income for each year
        df_income = df_all.groupby('year')['income'].sum().reset_index(name='all_income')
        
        # Merge the pivoted DataFrame with the income sums
        df_all = df_pivot.merge(df_income, on='year').reset_index()

        df_all = df_all[["year", "<15", "15-64", ">65", "all_income"]]

        scaler_max = 3.0 * max(df_all[["<15", "15-64", ">65"]].max())

        for age_group in ["<15", "15-64", ">65"]:
            df_all[age_group] = df_all[age_group] / scaler_max

    
    return {"data": df_all, "scaler_max": scaler_max}
