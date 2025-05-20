


from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.neural_network import MLPRegressor
from sklearn.metrics import mean_squared_error
from xgboost import XGBRegressor
from sklearn.ensemble import StackingRegressor, RandomForestRegressor
from pandas import DataFrame
from constant import BILLION_CONVERTER
from pandas import to_datetime as pandas_to_datetime
from copy import deepcopy
from darts.metrics import rmse

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