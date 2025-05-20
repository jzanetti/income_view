from pandas import read_excel, merge, concat
from numpy import nan as numpy_nan
from constant import DATA_PATH, DATA_PATH2, BILLION_CONVERTER



def get_proj_data(senario = "Median", base_year: int = 2023):
  # Read Excel files from different sheets
    if senario == "Median":
        colname = "50th\n(Median)"
    elif senario == "5th percentile":
        colname = "5th"
    elif senario == "25th percentile":
        colname = "25th"
    elif senario == "75th percentile":
        colname = "75th"
    elif senario == "95th percentile":
        colname = "95th"
    elif senario == "No immigration":
        colname = "No\nmigration\n(4)(5)"
    elif senario == "Cyclic immigration":
        colname = "Cyclic\nmigration\n(4)(6)"
    elif senario == "High immigration":
        colname = "Very high\nmigration\n(4)(7)"

    df_value = {}
    for proc_age in ["<15", "15-39", "40-64", "65+"]:
        df_value[proc_age] = read_excel(DATA_PATH2, sheet_name=proc_age, skiprows=1)
        df_value[proc_age] = df_value[proc_age].iloc[:, [0, df_value[proc_age].columns.get_loc(colname)]]
        df_value[proc_age] = df_value[proc_age].rename(columns = {
            "Unnamed: 0": "year",
            colname: "count"
        })

    df_value["15-64"] = merge(
        df_value["15-39"][["year", "count"]], 
        df_value["40-64"][["year", "count"]], on="year", how='inner', suffixes=('_df1', '_df2'))
    
    # Add X columns from df1 and df2
    df_value["15-64"]["count"] = df_value["15-64"]["count_df1"] + df_value["15-64"]["count_df2"]
    
    # Select only Y and the new X column
    df_value["15-64"] = df_value["15-64"][["year", "count"]]

    df_incre = {}
    for proc_age in ["<15", "15-64", "65+"]:
        base_count = df_value[proc_age].loc[df_value[proc_age]['year'] == base_year, 'count'].iloc[0]
        df_incre[proc_age] = 1.0 + (df_value[proc_age]["count"] - base_count) / base_count

    df_incre = concat(df_incre, axis=1)
    df_incre["year"] = df_value["15-64"]["year"]

    df_incre = df_incre.rename(columns={"65+": ">65"})

    return df_incre


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

    
    return df_all
