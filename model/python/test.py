import pandas as pd
import numpy as np
import torch
from darts import TimeSeries
from darts.models import TFTModel
from darts.metrics import rmse
from darts.dataprocessing.transformers import Scaler
import warnings
warnings.filterwarnings("ignore")

# Check if MPS is available, otherwise use CPU
device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
print(f"Using device: {device}")

# Sample historical DataFrame (yearly frequency, 2000–2023)
data = pd.DataFrame({
    'year': range(2000, 2024),
    '<15': [0.004196, 0.004709, 0.004587, 0.004391, 0.004341, 0.004442, 0.004442, 0.004067, 0.003927, 0.003279, 0.002860, 0.002670, 0.002681, 0.002560, 0.002957, 0.002724, 0.002526, 0.002503, 0.002400, 0.020413, 0.020094, 0.028291, 0.026825, 0.025776],
    '15-64': [0.223626, 0.226097, 0.229358, 0.234529, 0.240169, 0.245606, 0.250525, 0.251780, 0.255389, 0.256595, 0.256418, 0.257911, 0.258859, 0.258687, 0.261009, 0.265436, 0.270230, 0.276145, 0.281620, 0.301350, 0.308315, 0.321951, 0.322672, 0.333333],
    '>65': [0.042932, 0.043523, 0.044385, 0.045220, 0.046280, 0.047853, 0.048899, 0.050197, 0.051223, 0.052517, 0.053973, 0.055865, 0.058078, 0.060137, 0.062209, 0.064406, 0.066432, 0.068583, 0.070767, 0.074180, 0.076988, 0.080182, 0.083009, 0.086061],
    'all_income': [9.288513e+09, 9.732970e+09, 1.043808e+10, 1.119756e+10, 1.215951e+10, 1.323743e+10, 1.444163e+10, 1.545861e+10, 1.677422e+10, 1.796560e+10, 1.821810e+10, 1.907636e+10, 2.014151e+10, 2.124946e+10, 2.237427e+10, 2.377338e+10, 2.526969e+10, 2.648702e+10, 2.826905e+10, 3.015777e+10, 3.225660e+10, 3.350277e+10, 3.667994e+10, 4.016641e+10]
})

# Create a consistent year-end index
data['year'] = pd.date_range(start='2000-12-31', end='2023-12-31', freq='YE')
data.set_index('year', inplace=True)

# Convert data to float32 for MPS compatibility
data['all_income'] = data['all_income'].astype(np.float32)
data[['<15', '15-64', '>65']] = data[['<15', '15-64', '>65']].astype(np.float32)

# Create TimeSeries objects
target = TimeSeries.from_series(data['all_income'], freq='YE', fill_missing_dates=False)
past_covariates = TimeSeries.from_dataframe(data[['<15', '15-64', '>65']], freq='YE', fill_missing_dates=False)

# Scale the data for numerical stability
scaler_target = Scaler()
scaler_covariates = Scaler()
target_scaled = scaler_target.fit_transform(target)
past_covariates_scaled = scaler_covariates.fit_transform(past_covariates)

# Sample future covariates (monthly frequency, 2024–2026)
# Replace with your actual future covariate data
future_dates = pd.date_range(start='2025-01-31', end='2027-12-31', freq='ME')
future_covariates_df = pd.DataFrame({
    '<15': [0.025 + i * 0.0001 for i in range(len(future_dates))],
    '15-64': [0.335 + i * 0.0002 for i in range(len(future_dates))],
    '>65': [0.088 + i * 0.0003 for i in range(len(future_dates))]
}, index=future_dates)

# Resample future covariates to yearly frequency ('YE')
future_covariates_yearly = future_covariates_df.resample('YE').mean()
# Ensure the index starts exactly one year after 2023-12-31
future_covariates_yearly.index = pd.date_range(start='2024-12-31', end='2026-12-31', freq='YE')
future_covariates = TimeSeries.from_dataframe(future_covariates_yearly, freq='YE')
future_covariates = future_covariates.astype(np.float32)
future_covariates_scaled = scaler_covariates.transform(future_covariates)

# Verify time indices before appending
print("Past covariates end time:", past_covariates_scaled.end_time())
print("Future covariates start time:", future_covariates_scaled.start_time())

# Combine historical and future covariates
full_covariates_scaled = past_covariates_scaled.append(future_covariates_scaled)

# Initialize the TFTModel
model = TFTModel(
    input_chunk_length=12,
    output_chunk_length=3,
    hidden_size=32,
    lstm_layers=1,
    num_attention_heads=4,
    dropout=0.1,
    n_epochs=50,
    add_relative_index=True,
    random_state=42
)

# Train the model
model.fit(
    series=target_scaled,
    past_covariates=past_covariates_scaled,
    future_covariates=full_covariates_scaled,
    verbose=True
)

# Make predictions for 2024-2026
pred_scaled = model.predict(
    n=3,
    past_covariates=past_covariates_scaled,
    future_covariates=full_covariates_scaled
)
pred = scaler_target.inverse_transform(pred_scaled)

# Print the predictions
print("Predictions for all_income (2024-2026):")
print(pred.to_dataframe())

# Optional: Evaluate model performance
train_target, test_target = target_scaled.split_after(0.8)
train_covariates, test_covariates = past_covariates_scaled.split_after(0.8)
full_test_covariates = train_covariates.append(future_covariates_scaled)

# Train on training data
model.fit(
    series=train_target,
    past_covariates=train_covariates,
    future_covariates=full_test_covariates,
    verbose=True
)

# Predict on test period
pred_test_scaled = model.predict(
    n=len(test_target),
    past_covariates=past_covariates_scaled,
    future_covariates=full_test_covariates
)
pred_test = scaler_target.inverse_transform(pred_test_scaled)

# Calculate RMSE
error = rmse(scaler_target.inverse_transform(test_target), pred_test)
print(f"RMSE on test set: {error}")

# Optional: Visualize the results
import matplotlib.pyplot as plt
target.plot(label='Historical')
pred.plot(label='Forecast')
plt.legend()
plt.show()