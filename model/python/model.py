


from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.neural_network import MLPRegressor
from sklearn.metrics import mean_squared_error
from xgboost import XGBRegressor
from sklearn.ensemble import StackingRegressor, RandomForestRegressor
from pandas import DataFrame
from constant import BILLION_CONVERTER, OCED_INDICATORS
from pandas import to_datetime as pandas_to_datetime
from copy import deepcopy
from darts.metrics import rmse
from pandas import merge as pandas_merge
from sklearn.feature_selection import RFECV
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import make_scorer, mean_squared_error
import shap
from matplotlib.pyplot import savefig, close

def run_contribution(
        data1: DataFrame, 
        data2: DataFrame,
        use_oecd: bool = False,
        pre_selected_features = OCED_INDICATORS, 
        run_feature_selection: bool = False, 
        run_eval: bool = True, 
        run_shap: bool = True):
    
    if use_oecd:
        all_data = pandas_merge(data1, data2, on = "year")
    else:
        all_data = data2
    all_data = all_data.dropna()

    if pre_selected_features is None:
        all_features = list(all_data.columns)
        all_features.remove("year")
        all_features.remove("all_income")
    else:
        all_features = OCED_INDICATORS
        all_features.extend(["<15", "15-64", ">65"])

    x = all_data[all_features]
    x.columns = [col.replace('[', '_').replace(']', '_').replace('<', '_') for col in x.columns]
    y = all_data['all_income']

    if run_feature_selection:
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(x)
        model = LinearRegression()
        scoring = make_scorer(mean_squared_error, greater_is_better=False)

        # Apply RFECV
        rfecv = RFECV(estimator=model, step=1, cv=5, scoring=scoring)
        _ = rfecv.fit_transform(X_scaled, y)

        all_features_update = []
        for proc_feature_index in rfecv.get_feature_names_out():
            proc_feature_index = int(proc_feature_index[1:])
            all_features_update.append(x.columns[proc_feature_index])
        all_features = all_features_update
        x = all_data[all_features]
        x.columns = [col.replace('[', '_').replace(']', '_').replace('<', '_') for col in x.columns]

    model = XGBRegressor(
        n_estimators=100,  # Number of trees
        learning_rate=0.1,  # Step size for updates
        max_depth=3,  # Maximum depth of each tree
        random_state=42  # For reproducibility
    )

    if run_eval:
        model_eval = deepcopy(model)
        x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=42)
        model_eval.fit(x_train, y_train)
        y_pred = model_eval.predict(x_test)
        mse = mean_squared_error(y_test / BILLION_CONVERTER, y_pred / BILLION_CONVERTER)
        print(f"Mean Squared Error of Model: {mse:.4f}")

    model.fit(x, y)

    if run_shap:
        # For tree-based models like XGBoost, shap.TreeExplainer is efficient
        explainer = shap.TreeExplainer(model)

        # Calculate SHAP values for the test set (or any dataset you want to explain)
        shap_values = explainer.shap_values(x)
        shap.summary_plot(shap_values, x, plot_type="bar", show=False)
        savefig("contribution.png")
        close()



def run_predict(data: DataFrame, model, model_name):
    data.columns = [col.replace('[', '_').replace(']', '_').replace('<', '_') for col in data.columns]
    y_pred = model.predict(data)

    return y_pred

def run_training(data: DataFrame, model_name: str, run_eval: bool = True):

    if model_name == "stack_model":
        base_models = [
            ('lr', LinearRegression()),
            ('rf', RandomForestRegressor(n_estimators=10)),
            ('nn', MLPRegressor(hidden_layer_sizes=(30,), max_iter=1000))
        ]
        meta_model = LinearRegression()
        model = StackingRegressor(
            estimators=base_models,
            final_estimator=meta_model,
            cv=5  # Cross-validation for generating meta-features
        )
    elif model_name == "xgboost":
        model = XGBRegressor(
            n_estimators=100,  # Number of trees
            learning_rate=0.1,  # Step size for updates
            max_depth=3,  # Maximum depth of each tree
            random_state=42  # For reproducibility
        )
    elif model_name == "linear_regression":
        model = LinearRegression()

    # Define features (X) and target (y)
    x = data[['<15', '15-64', '>65']]
    x.columns = [col.replace('[', '_').replace(']', '_').replace('<', '_') for col in x.columns]

    y = data['all_income']

    if run_eval:
        eval_model = deepcopy(model)
        x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=42)
        eval_model.fit(x_train, y_train)

        # Evaluate the model
        y_pred = eval_model.predict(x_test)
        mse = mean_squared_error(y_test / BILLION_CONVERTER, y_pred / BILLION_CONVERTER)
        print(f"Mean Squared Error of Model: {mse:.4f}")

        if model_name == "stack_model":
            # Optional: Evaluate individual base models
            for name, model in base_models:
                eval_model = deepcopy(model)
                eval_model.fit(x_train, y_train)  # Train individual model
                individual_pred = eval_model.predict(x_test)
                individual_mse = mean_squared_error(y_test / BILLION_CONVERTER, individual_pred / BILLION_CONVERTER)
                print(f"MSE for {name}: {individual_mse:.4f}")


    model.fit(x, y)

    return model