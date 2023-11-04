# Initial data analysis in the context of regression modeling


## Repository for IDA for regression modeling 
The focus of this report is to provide examples on conducting initial data analysis in a reproducible manner in the context of intended regression analyses.

## Project Description

**Objective:**  Develop initial data analysis plan (IDAP) and provide sample reports for IDA before executing the statistical analysis plan (SAP) for regression modeling.

Six steps of the IDA framework [Ref 1] are

1. Meta data set-up
2. Data cleaning
3. Data screening
4. Initial data reporting
5. Updating/refining the statistical analysis plan
6. Reporting of IDA findings in research papers [Ref 2]

For our objective, we assume that meta data exist and data cleaning has already been done. We created hypothetical statistical analyses plans for each of the data sets. 
 
## Project Outcomes
 
1. Sample IDA plans and IDA reports to illustrate data screening and initial reporting (steps 3 and 4 of the IDA framework)
2. Recommendations for numerical and graphical summaries (step 3 of IDA framework) 
2. Explanation and elaboration of potential consequences to the SAP as a result of IDA findings (step 5 of IDA framework)
3. Recommendations for reporting of IDA for regression analyses (step 6 of IDA framework)
4. Manuscript with scope of regression model, generic IDA strategy, examples with IDA discoveries and consequences
 

## Structure  


* main - General report files
* data-raw - Repository for original data sets and their data dictionaries 
* data - Repository for analysis data sets
* R - R functions for data visualization and transformations used in the R markdown files
* docs - report in website
* report - report in MS word format 

## Data set specifications and tracker

https://docs.google.com/spreadsheets/d/1Ft5eyenvDnMBoLvJmcBaklfrYcwyW-rkt-ivIkaphdA/edit?usp=sharing


## References

### Initial data analysis
[1] Huebner M, le Cessie S, Schmidt CO, Vach W . A contemporary conceptual framework for initial data analysis. Observational Studies 2018; 4: 171-192. [Link](https://obsstudies.org/contemporary-conceptual-framework-initial-data-analysis/)

[2] Huebner M, Vach W, le Cessie S, Schmidt C, Lusa L. Hidden Analyses: a review of reporting practice and recommendations for more transparent reporting of initial data analyses. BMC Med Res Meth 2020; 20:61 [Link](https://bmcmedresmethodol.biomedcentral.com/track/pdf/10.1186/s12874-020-00942-y)

### Bacteremia data set
[3] Ratzinger F, Dedeyan M, Rammerstorfer M, Perkmann T, Burgmann H, et al. (2014) A Risk Prediction Model for Screening Bacteremic Patients: A Cross Sectional Study. PLoS ONE 9(9): e106765. doi:10.1371/journal.pone.0106765


## Abbreviations
SAP - statistical analysis plan </br>
IDA - initial data analysis </br>
IDAP - initial data analysis plan 


## Funding

None. </br>
Contributors are from the [STRATOS Initiative](https://stratos-initiative.org).

-TG2: Selection of variables and functional forms in multivariable analyses.</br>
-[TG3](https://www.stratosida.org): Initial data analysis

## Authors

Mark Baillie </br>
Novartis, </br>
Email: mark.baillie@novartis.com

Georg Heinze </br>
Medical University, Vienna, Austria</br>
Email: georg.heinze@meduniwien.ac.at

Marianne Huebner </br>
Department of Statistics and Probability, Michigan State University, East Lansing, MI, USA</br>
Email: huebner@msu.edu
