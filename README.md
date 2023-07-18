# Shades of Green: Unveiling the Impact of Municipal Green Bonds on the Environment
This repository is linked to the methodology developed in the paper with title

**"Shades of Green: Unveiling the Impact of Municipal Green Bonds on the Environment"**. The pdf for the paper is available at this url https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3808550 or provided in the folder "Paper", where it is also possibile to find the Supplementary Materials.

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

The combination of multiple multivariate data sets is of primary importance in financial end environmental modelling applications. 



## Methodology of the Paper


<p align="center">
    <img src="https://i.postimg.cc/Y9Nt399V/diag-method-2.png" width="590" height="460" />
</p>

## Organization of the Repository
The repository is organized in the following folders:



## Shiny App Instructions
The Shiny App can be used as follows:




## Cite

If you use this code in your project, please cite:

@article{campi2021green,
  title={Green Bond Performance and Risk Indicators},
  author={Campi, Marta and Peters, Gareth W and Richards, Kylie-Anne},
  journal={Available at SSRN 3808550},
  year={2021}
}

