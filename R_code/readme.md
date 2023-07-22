# R_code Instruction

This folder contains the Rcode implemented for the data pre-processing, feature extraction, kPCA/PCA methods and the CCA solution for spatial-temporal analysis of municipal green bonds vs climate and municipal green bonds vs polltuion. Inside the folders, you will find the following rcodes:

```diff
+ 1) CCA
```

1. **cca_cali.R**. This code performs the extraction of the CCA for green financial kPCs vs pollution kPCs and green financial kPCs vs climate kPCs. Note that at the beginning, a matching procedure for selecting the 9 counties considered in the financial data case is performed. Several plots and statistical tests are provided to understand the obtained results and reported in the main body of the paper.
2. **cca_cali_pca.R**. This code performs the extraction of the CCA for green financial PCs vs pollution PCs and green financial PCs vs climate PCs. Note that at the beginning, a matching procedure for selecting the 9 counties considered in the financial data case is performed. Several plots and statistical tests are provided to understand the obtained results and reported in the main body of the paper.
3.  **cca_circular_barplot.R**. This code performs the circular bar plots used to analyse the canonical structured coefficients outputs. Note that these are presented in the case of the kPCA only since the results for the PCs have not been found significant.



```diff
+ 1) climate
```
1. **GSODR_extraction.R**. This code performs the extraction of 10 year climate time series from the GSODR website through its available R package.
2.  **ckta_res.R**. This code cpmputes the cKTAs of the .
3.  **kPCA_Cali.R**. .
4. **kPCA_by_state.R**. .
5.  **pca_extraction.R**. .
6.  **plot_seas_climate.R**. .
7.  ****. .


```diff
+ 1) financial
```
1. **ckta_cat_var.R**. .
2.  **ckta_kpca_vat.R**. .
3.  **ckta_res_gb.R**.  .
4.  **kPCs_extraction.R**. .
5. **pca_extraction.R**. .


```diff
+ 1) pollution
```
1. **AQI.R**. .
2.  **CO2.R**. .
3.  **NO2.R**. .
4. **PM2.5.R**. .
5.  **ckta_res.R**. .
6.  **kpca_by_state.R**. .
7.  **kpca_cali.R**. .
8.  **pca_cali.R**. .
9.  **plots_res.R**. .

