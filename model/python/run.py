from data import get_input_data, get_proj_data, get_oecd_data
from model import run_training, run_predict, run_contribution
from pandas import DataFrame, concat
from analysis import obtain_pop_scaler, obtain_cpi_scaler
import warnings

# Suppress all warnings
warnings.filterwarnings('ignore')

run_projection = False
run_shap = False
run_analysis = True
if run_analysis:
    obtain_pop_scaler()
    obtain_cpi_scaler()

if run_shap:
    oced_data = get_oecd_data()
    income_data = get_input_data(income_type="benefits")
    run_contribution(
        oced_data, income_data,
        use_oecd = False,
        pre_selected_features = None, 
        run_feature_selection = False, 
        run_eval = True, 
        run_shap = True)


if run_projection:
    model_name = "linear_regression" # linear_regression, xgboost, stack_model
    base_year = 2023
    outputs = []
    for income_type in ["labour", "labour_sensitivity", "capital", "capital_sensitivity", "benefits", "total"]:

        income_data = get_input_data(income_type=income_type)
        model = run_training(income_data, model_name)

        for proc_senario in ["Median", "5th percentile", "25th percentile", "75th percentile", "95th percentile", "No immigration", "Cyclic immigration", "High immigration"]:
            pop_proj = get_proj_data(senario=proc_senario, base_year=base_year)
            years = list(pop_proj["year"].unique())
            all_prds = []
            base_data = income_data[income_data["year"] == base_year]
            for proc_year in years:
                proc_data = pop_proj[pop_proj["year"] == proc_year]
                for proc_var in ["<15", "15-64", ">65"]:
                    proc_data[proc_var] = proc_data[proc_var].iloc[0] * base_data[proc_var].iloc[0]
                
                all_prds.append(run_predict(proc_data[["<15", "15-64", ">65"]], model, model_name)[0])

            prd_data = DataFrame({"year": years, "all_income": all_prds})
            prd_data["status"] = "predicted"
            prd_data["scenario"] = proc_senario
            prd_data["income_type"] = income_type
            outputs.append(prd_data)

        income_data = income_data[["year", "all_income"]]
        income_data["status"] = "observed"
        income_data["scenario"] = None
        income_data["income_type"] = income_type
        outputs.append(income_data)

    combined_df = concat(outputs, axis=0, ignore_index=True)

    combined_df.to_csv("etc/income_proj.csv", index=False)