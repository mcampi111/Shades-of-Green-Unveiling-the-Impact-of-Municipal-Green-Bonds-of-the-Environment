# pollution_data Instruction

This folder contains the raw_data and the features extracted for the pollution data. Inside the folders, you will find the following RData/rds results:

```diff
+ 1) features
```

1. **kpca_opt_index.RData**. kPCs extracted on the data.
2. **new_kpcs_100.rds**. New kpcs evaluated on a mesh of 100 new data points.
3. **new_kpcs_1000.rds**. New kpcs evaluated on a mesh of 1000 new data points.
4. **new_pcs_pollution.RData**. New pcs evaluated on a mesh of new data points.
5. **opthyper_pollution.RData**. Optimal hyperparameters (list) for the optimal kPCs evaluated through a grid search.
6. **param_df_final.RData**. Optimal hyperparameters (dataframe) for the optimal kPCs evaluated through a grid search.
7. **pcs_pollution**. PCs extracted on the data (categorical variables have been encoded first).


```diff
+ 1) raw_data
```
This folder contains 5 subfolders which have been downloaded by the Environmental Protection Agency website. We considers Air Quality, Carbon monoxide, Nitrogen dioxide,
particulate matter 2.5, Sulfur dioxide on a daily basis for 10 years. The data are given as follows:

1. **AQI_daily_2010_2020**. Daily data for 10 years.
2. **CO_daily_2010_2020**. Daily data for 10 years.
3. **NO2_daily_2010_2020**. Daily data for 10 years.
4. **PM25_daily_2010_2020**. Daily data for 10 years.
5. **SO2_daily_2010_2020**. Daily data for 10 years.

