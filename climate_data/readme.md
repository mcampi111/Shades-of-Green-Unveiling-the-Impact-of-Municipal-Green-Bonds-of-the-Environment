# climate_data Instruction

This folder contains the raw_data and the features extracted for the climate data. Inside the folders, you will find the following RData/rds results:

```diff
+ 1) feature
```

1. **kpca_opt_index.RData**. kPCs extracted on the data.
2. **new_kpcs_100.rds**. New kpcs evaluated on a mesh of 100 new data points.
3. **new_kpcs_1000.rds**. New kpcs evaluated on a mesh of 1000 new data points.
4. **new_pcs_climate.RData**. New pcs evaluated on a mesh of new data points.
5. **opthyper_climate.RData**. Optimal hyperparameters (list) for the optimal kPCs evaluated through a grid search.
6. **param_df_final.RData**. Optimal hyperparameters (dataframe) for the optimal kPCs evaluated through a grid search.
7. **pcs_climate**. PCs extracted on the data (categorical variables have been encoded first).


```diff
+ 1) raw_data
```

1. **meteo_var_GSOD_us_v2.RData**. We extracted through the R GSOD package climate variables for the us for 10 years, from 2010 to 2020. Hence, to get the California data, an extra filter step must be applied. Furthermore, several variables are contained in this data set. We filter them and consider precipitation, high temperature, low temperature and average temperature. 



