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
2.  **ckta_res.R**. This code computes the cKTAs of the different kPCs and PCs. These are provided in the results of the Index Analysis subsection.
3.  **kPCA_Cali.R**. This code performs the kPCs extraction for the data of California considered in this paper.
4. **kPCA_by_state.R**. This code performs extra kPCs extraction that can be used for future work since it considers all the US states.
5.  **pca_extraction.R**. This code performs the PCs extraction for California.
6.  **plot_seas_climate.R**. This code provides all the plots related to the kPCs and the PCs of the climate case.

```diff
+ 1) financial
```
1. **ckta_cat_var.R**. .
2.  **ckta_kpca_vat.R**. This code computes the cKTAs of the different kPCs. These are provided in the results of the Index Analysis subsection.
3.  **ckta_res_gb.R**.  This code computes the cKTAs of the different combinations of categorical variables to understand the relative contribution of each variable within each county. The results are provided in the Appendix.
4.  **kPCs_extraction.R**. This code performs the kPCs extraction for the data of California considered in this paper.
5. **pca_extraction.R**. This code performs the PCs extraction for California. Further, the cKTA for the PCs is also calculated.


```diff
+ 1) pollution
```
1. **AQI.R**. This code performs the data preparation for the AQI time series prior to the extraction of the PCs/kPCs.
2.  **CO2.R**. This code performs the data preparation for the CO time series prior to the extraction of the PCs/kPCs.
3.  **NO2.R**. This code performs the data preparation for the NO2 time series prior to the extraction of the PCs/kPCs.
4. **PM2.5.R**. This code performs the data preparation for the PM2.5 time series prior to the extraction of the PCs/kPCs.
5.  **ckta_res.R**. This code computes the cKTAs of the different kPCs and PCs. These are provided in the results of the Index Analysis subsection.
6.  **kpca_by_state.R**. This code performs extra kPCs extraction that can be used for future work since it considers all the US states.
7.  **kpca_cali.R**. This code performs the kPCs extraction for the data of California considered in this paper.
8.  **pca_cali.R**. This code performs the PCs extraction for the data of California considered in this paper.
9.  **plots_res.R**. This code provides all the plots related to the kPCs and the PCs of the pollution  case.

