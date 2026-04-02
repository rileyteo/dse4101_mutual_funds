# DSE4101 Group 1: Predicting Mutual Fund Performance in Science & Technology Sector

## Project Structure

```text
dse4101_mutual_funds/
├── data/                    # Processed datasets and benchmark returns
├── Data_processing/         # R scripts for cleaning and feature engineering
├── models/                  # Models, Tuning and SHAP analysis code
├── model_evaluation/        # Backtesting, Equity Curves, and DM-Tests
└── figures/                 # Visualisations of SHAP values and performance
```

## Model Evaluation Guide
To switch between the various machine learning models, follow these steps:

### 1. Open the model file
Open the specific model file in the models/ folder (e.g., RF.Rmd or RNN.Rmd). Load the required libraries at the top of each model's script.

### 2. Configure the model inside the portfolio evaluation script
Open model_evaluation/portfolio_evaluation_v2.2.Rmd. Locate the ```predict_alpha_...``` function and replace it with the respective model's function (documented in the table below). Then, locate the ```build_top_decile_history``` function call inside the code block below. Note that you must update the model_fn argument to match the architecture you wish to test.

#### Example: Switching to OLS
```r
results <- build_top_decile_history(
  model_fn = predict_alpha_ols,  # <--- CHANGE MODEL NAME HERE
  entire_df = df
)
```
| Model | Function Name | File Path |
| :--- | :--- | :--- |
| **OLS** | `predict_alpha_ols` | `models/OLS.Rmd` |
| **Elastic Net** | `predict_alpha_enet` | `models/Elastic Net.Rmd` |
| **Lasso** | `fit_lasso_model` | `models/lasso_model.Rmd` |
| **Random Forest** | `predict_alpha_rf` | `models/RF.Rmd` |
| **XGBoost** | `predict_alpha_xgb` | `models/XGBoost.Rmd` |
| **FNN (H2O)** | `predict_alpha_nn_h2o` | `models/FNN.Rmd` |
| **RNN (LSTM)** | `predict_alpha_rnn` | `models/RNN.Rmd` |

### 3. Run the script
Run the entire model_evaluation/portfolio_evaluation_v2.2.Rmd script. Subsequently, the equity plots and model performance metrics will be generated.
