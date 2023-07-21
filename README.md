# Shades of Green: Unveiling the Impact of Municipal Green Bonds on the Environment
This repository is linked to the methodology developed in the paper with title

**"Shades of Green: Unveiling the Impact of Municipal Green Bonds on the Environment"**. The pdf for the paper is available at this url https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3808550 or provided in the folder "paper", where it is also possibile to find the Supplementary Materials.

There is a **shiny app** available at https://mcjc-2020.shinyapps.io/final_results/

## **Abstract**

Green bonds allocate proceeds towards environmentally beneficial projects, distinguishing themselves from traditional bonds. However, investors often find it challenging to assess the carbon reduction potential of these bonds because of the lack of standardised environmental impact reporting. In response to this, our research constructs a unique set of indicators derived from financial and environmental datasets, using advanced statistical techniques. A novel method using kernel Principal Component Analysis (kPCA) and Canonical Correlation Analysis (CCA) is applied to detect cross-correlation in multivariate datasets. This approach innovatively handles variable comparability issues and the differential treatment of categorical and numerical variables. A significant finding of this study emerges when this methodology is applied to municipal financial data and pollution data from nine California counties. The results show a clear and interpretable correlation directly linked to the amount of green bond issuance, underscoring the tangible impact of these financial instruments on environmental outcomes. However, with the weak cross-correlation observed between climate and financial data sets, further research is recommended over a broader timescale. Such research will confirm the direct effect of green bonds on climate change reduction, enhancing market transparency, and bolstering confidence in green bonds as a crucial tool for the economic transition required by the Paris Agreement.


## Contributions of the paper
The paper has multiple contributions, categorised into conceptual, methodological, and data application contributions:
1. Our first conceptual contribution provides a quantitative definition of the environmental impact of a green bond, a highly debated topic in the green finance community. We also introduce reliable statistical indicators, specifically the kernel Principal Components (kPCs), that capture variability and highlight leading factors to measure this impact. This approach is a notable requirement in financial markets. Our research shows that these kPCs can detect diversified information based on the data set and the kPC number.
2. The second contribution of this work is the combination of the kPCA and the CCA in a novel way considering multiple multivariate pairs of data sets to account for the global green financial market effects as well as the global pollution in the US State of California. The goal is to detect the cross-correlation amongst multiple data sets in non-stationary settings. We show that this approach strongly empowers the desired findings with respect to traditional linear PCA combined with CCA. Therefore, it acts as a robust version of CCA for non-linear and non-stationary multi-multivariate data sets. 
3. Our approach to treating different data sources, including numerical and categorical variables, provides a contribution to data application. This issue often occurs in kernel methods, and we address it by using the Jaccard distance. We incorporate the contribution of the categorical data through the Jaccard kernel, allowing for multi-modality kPCA. This method is instrumental in the analysis of environmental and financial data. 
4. Another significant contribution to data applications is our development of specific variables and features that are needed to accurately identify the environmental impact over time and space. Given the variations in observational timestamps and spatial recording monitors in the data sets, such feature engineering is essential. It contains pertinent information that encapsulates variability and sheds light on the behaviour of the underlying data.
5. A key finding of our study is the substantial and interpretable correlation identified when applying our methodology to green bond municipal financial data and pollution data in nine counties in California. These results are directly related to the volume of green bond issuance, emphasising the real-world environmental impact of these financial instruments. Such investigations can confirm the direct influence of green bonds on climate change reduction, improving market transparency, and increasing faith in green bonds.

## Motivations For the Development of the Paper

Green bonds are distinctive financial instruments that direct funds towards environmentally advantageous initiatives, setting them apart from their conventional bond counterparts. However, assessing the potential for carbon reduction of these bonds presents a considerable challenge for investors due to the lack of standardised reporting on environmental impact. Our research initiative designed a unique set of indicators to address this gap, leveraging financial and environmental data sets and employing sophisticated statistical techniques.

The methodology and experimental design applied in this study facilitated an in-depth and multifaceted exploration of the influence of green bonds on environmental and climate-associated parameters in Californian counties. California, chosen for its abundant data availability and numerous environmental monitoring stations, offered an ideal backdrop for our investigation. Although the positioning of these stations introduced certain complexities, they did not detract from the overall viability of the study. The research focused on key cities in California, incorporating areas within a 50 km radius. This decision imbued the study with a layer of practical realism, as these zones frequently serve as the nucleus for dynamic economic and environmental operations, making them prime areas for the likely tangible effects of green bond issuance.

Integrating three different data sets, pollution, climate, and green bonds, into our research approach, we achieved a multifaceted view of the complex interplay between fiscal incentives and environmental improvement. This multidimensional perspective, together with our rigorous methodology, underscores the potential and importance of green bonds in instigating significant positive environmental change, providing a valuable reference for investors interested in environmentally responsible investment opportunities.


## Methodology of the Paper

Data spatial-temporal multivariate analysis is essential in financial end environmental modelling applications. Using non-stationary covariance decomposition methods combined with a linear multivariate technique allows us to develop a robust tool for real-data scenario settings. To efficiently capture global variations of the municipal green financial market and pollution (or climate) in unison, we project the original data of every county in California onto non-linear subspaces expressed through basis functions relying on the machine learning technique known as kernel Principal Component Analysis (kPCA). In such a way, we can detect underlying data variations that are intrinsically generated non-linearly and mostly drive the data in a more complex fashion. Financial data global shifts are hardly discernable through standard linear solutions such as panel regression or equivalent linear spatial-temporal methods. Hence, we extract kernel Principal Components (kPCs), expressing data non-linear variations, for each county and each data set considered, i.e. we will have kPC1, kPC2 and kPC3 for Alameda, Los Angeles, etc., for financial, pollution and climate data sets. At this stage, we consider kPC1 for all the counties of the financial data and kPC1 for all the counties of the pollution data. Afterwards, we quantify associations between these two sets of kPC1s through the Canonical Correlation Analysis (CCA) method. We will repeat the same exercise with kPC2 since, in practice, these two sets of evaluated functions capture different modes of data variations and, therefore, reveal several insights. The found association does not necessarily mean impact. Hence, we provide interpretation for the results that are indeed explainable. The following figure describes the implemented method. We refer to this implemented method as spatial-temporal kPCA-CCA, and we will compare it to its linear version PCA-CCA, referring to Principal Component Analysis-CCA, where we instead extract PCs.


<p align="center">
    <img src="https://i.postimg.cc/Y9Nt399V/diag-method-2.png" width="590" height="460" />
</p>


Firstly, the kPCA-CCA and its benchmark PCA-CCA are run to observe municipal green bonds' impact on pollution and climate. Therefore, the analysis will be performed by running kPCA-CCA (and PCA-CCA) on pollution vs financial kPCs (PCs) and climate vs financial kPCs (PCs). The Figure is set on the first case, but we perform an equivalent analysis for climate. Hence, we will describe the pair pollution/financial figure and consider similar steps for the case of climate/financial. Given the pollution and financial data sets, we split them by county and performed ad hoc cleaning and pre-processing procedures. Afterwards, a specific set of features was extracted depending on the data. In the case of pollution, this has foreseen engineering ad hoc features summarising information across many time series collected at various monitors. In the case of financial, numerical and categorical variables have been handled. After, both the linear PCA and the non-linear kPCA have been applied, and, for each method, the first two bases (PC1, PC2, kPC1, kPC2) have been retained. Once obtained, the next step consisted of comparing which bases better captured the variability of the given data set through centered kernel target alignment (cKTA), introduced in the main body of the paper. Finally, the PCA-CCA and the kPCA-CCA will be computed between pollution and financial PCs and kPCs, respectively, to measure the association of green bonds with pollution attributes within different counties in California and quantify a certain impact. 

The obtained results for the kPCA-CCA are significant and presented in the paper. We provide structured coefficients for interpretation, which will be given in the following figures. It is indeed possible to observe the results of the kPC1-CV1 (left) and kPC2-CV1 (right). The two kPCs capture different variation modes provided by the different number of issued green bonds per county. The direction of the correlations has a direct interpretation. More explanation is provided in the main body of the paper.

<p align="center">
    <img src="https://i.postimg.cc/MKBJ8kTz/screen-poll1.png" width="290" height="260" />
    <img src="https://i.postimg.cc/mrHmP4vr/screen-poll2.png" width="290" height="260" />
</p>



## Organization of the Repository
The repository is organized in the following folders:


```diff
+ 1) Python_code 
```
In this folder, there are the following .py codes, used for the evaluation of the kPCs on a new mesh. 

1.  **project_x.py**. This python file projects the existing kPCs on a new mesh, using an RBF kernel. Note that the eigenvalues/eigenvectors are required.
2. **project_x_2.py**. This python file projects the existing kPCs on a new mesh, using an RBF kernel on the set of the numerical variables (X1) and  a Jaccard kernel, on the set of the categorical variable (X2). Note that the eigenvalues/eigenvectors are required.

   
```diff
+ 2) R_code 
```
This folder contains the R code developed for the analysis of the paper. This is structured within the following folders:

1.  **CCA**. This folder contains the code for the extraction of the Canonical Correlation Analysis given in cca_cali.R and the code for producing the helioplots in the paper and in the above description (only some) in cca_circular_barplot.R.
2. **climate**. This folder contains the code for the extraction, pre-processing, feature engineering and PCA/kPCA computations of the climate data. 
3.  **financial**. This folder contains the code for.....
4.  **pollution**. This folder contains the code for the extraction, pre-processing, feature engineering and PCA/kPCA computations of the pollution data..

```diff
+ 3) climate_data 
```
This folder contains the climate data used for the analysis. Both the raw_data and the features are provided as follows:

1.  **raw_data**. This folders contains the raw data extracted for the climate variables. This data has been extracted through the R package for the GSOD website, hence data is in the RData format.
2. **features**.  This folders contains the engineered features for the climate variables, whose extraction procedure is presented in the main body of the paper. 


```diff
+ 4) finance_data 
```



```diff
+ 5) paper 
```
This folder contains the draft of the paper which is also available at https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3808550.

```diff
+ 6) paper_figures 
```
This folder contains all the paper figures of the paper. For each figure produced, there is an Rcode associated within the R_code folder.

```diff
+ 7) pollution_data
```

This folder contains the pollution data used for the analysis. Both the raw_data and the features are provided as follows:

1.  **raw_data**. This folders contains the raw data extracted for the pollution variables. This data has been extracted through the R package for the GSOD website, hence data is in the RData format.
2. **features**.  This folders contains the engineered features for the pollution variables, whose extraction procedure is presented in the main body of the paper. 

## Shiny App Instructions
The Shiny App provided at https://mcjc-2020.shinyapps.io/final_results/ can be used as follows:

The **top results** provide information about the different kPCA-CCA results where one can change the number of kPCs considered (i.e. 1 and 2), the canonical variate of interest (i.e. 1,2,3) and the multivariate relationship taken into account, i.e. Green Bonds vs Pollution or Green Bonds vs Climate, per county. These commands will change the underlying helio plots. Furthermore, the kPCs of the different data sets can be observed. While the ones extracted by the financial variables are always visible, it is possible to change the kCPs and observe the ones of the pollution data or the ones of the climate data. This can be done by selecting two facets, Quarters and County or by considering only one of the two facets individually. 

The **bottom results** provide information about the variables of the financial data set that have been used. The **right panel** allows to choose between numerical and categorical variables. Then, if categorical is selected, 


## Cite

If you use this code in your project, please cite:

@article{campi2021green,
  title={Shades of Green: Unveiling the Impact of Municipal Green Bonds on the Environment},
  author={Campi, Marta and Peters, Gareth W and Richards, Kylie-Anne},
  journal={Available at SSRN 3808550},
  year={2021}
}

