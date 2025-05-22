from pandas import read_excel, concat, to_numeric
from constant import DATA_PATH, DATA_PATH4

def obtain_pop_scaler(base_year: int = 2000):

    df_count = read_excel(DATA_PATH, sheet_name="data2", skiprows=1)
    df_count = df_count.rename(columns={
        df_count.columns[0]: "year",
        df_count.columns[1]: "age",
        df_count.columns[2]: "count"
    })

    scaled_df = []
    for proc_age in df_count["age"].unique():
        proc_df = df_count[df_count["age"] == proc_age]
        base_value = float(proc_df[proc_df["year"] == base_year]["count"].iloc[0])
        proc_df["count"] = proc_df["count"] / base_value
        scaled_df.append(proc_df)
    
    scaled_df = concat(scaled_df)

    scaled_df.to_csv("etc/scaled_pop.csv")

def obtain_cpi_scaler(base_year: int = 2000):

    cpi_records = read_excel(DATA_PATH4, sheet_name="Data", skiprows=4)

    # Select specific columns and rename them
    cpi_records = cpi_records[["Series Id", "CPI.Q.C.ia"]]
    cpi_records.columns = ["date", "CPI"]

    # Convert date to string, extract year, convert CPI to numeric
    cpi_records["year"] = cpi_records["date"].astype(str).str[:4]
    cpi_records["CPI"] = to_numeric(cpi_records["CPI"], errors='coerce')

    # Group by year and calculate mean CPI
    cpi_records = cpi_records.groupby("year", as_index=False)["CPI"].mean()
    cpi_records = cpi_records.rename(columns={"CPI": "mean_cpi"})

    # Calculate scaled CPI
    base_cpi_value = cpi_records.loc[cpi_records["year"] == str(base_year), "mean_cpi"].values[0]
    cpi_records["scaled_cpi"] = cpi_records["mean_cpi"] / base_cpi_value

    cpi_records.to_csv("etc/scaled_cpi.csv")
