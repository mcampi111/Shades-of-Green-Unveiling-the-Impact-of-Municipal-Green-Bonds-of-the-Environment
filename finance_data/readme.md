# finance_data Instruction

This folder contains the features extracted for the financial data. Inside the folder, you will find the following RData/rds results:

1. **kpca_opt_index_CA_TX_NY.RData**. kPCs extracted on the data (wategorical variables have been encoded first).
2. **new_kpcs_CA_TX_NY_100.rds**. New kpcs evaluated on a mesh of 100 new data points.
3. **new_kpcs_CA_TX_NY_1000.rds**. New kpcs evaluated on a mesh of 1000 new data points.
4. **new_pcs_gbs.RData**. New pcs evaluated on a mesh of new data points.
5. **opthyper_CA_TX_NY.RData**. Optimal hyperparameters (list) for the optimal kPCs evaluated through a grid search.
6. **param_df_final_CA_TX_NY.RData**. Optimal hyperparameters (dataframe) for the optimal kPCs evaluated through a grid search.
7. **pcs_gbs**. PCs extracted on the data (wategorical variables have been encoded first).

Note that, we have extracted this data for three states, i.e. California, Texas and New York. Hence for the analysis of this paper, a furthe filter step is required to select only the data for California. 
