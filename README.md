# Ecosystem Climate Regulation Services

## Introduction

This repository contains the data and codes for replication of the results presented in the manuscript entitled *'Conserving the Cerrado and Amazon biomes of Brazil protects the soy economy from damaging warming'*, submitted to the journal World Development.

The scripts presented here process data on native vegetation, past climate, future climate projections, agriculture, and future projections of land use and land cover. This data is used to estimate the role of native vegetation in Brazil in providing regulation of extreme heat for neighboring soybean production. 

The materials in this repository allow users to reproduce the results and figures appearing in the main text of the manuscript.

To reproduce the figures appearing in the manuscript, it is necessary only to run the script Result_Analysis.R. To reproduce the methodology to generate the results presented in the paper, the script Master.R shows the order in which the several scripts for data analysis should be run, and the role of each of them.

The main scripts are written in R, with the exception of the scripts in the folder "Aux_codes/Compute_EDD/". These python scripts are auxiliary to calculate changes in Extreme Degree Days, and are not used independently, as they called from within other R scripts.


## Main structure

### Main scripts

* Result_analysis.R. 

This is the main R script in this repository that replicates the generation of tables and figures presented in the manuscript.

* Master.R. 

This is the master R script that runs all the auxiliary scripts in the Aux_codes folder, processing the input data and calculating the ecosystem extreme heat regulation services.  



### Folders

* Aux_codes/

This folder contains the R ad Python scripts used to estimate changes in temperature resulting from land use and land cover change; the changes in extreme heat exposure to soybean production, through estimation of changes extreme degree days; the value of loss revenue due to increased exposure to extreme heat in the historical period and future scenarios; and the ecosystem services provided by native vegetation in the future scenarios.

* Figures/

This folder contains the tables and figures that present the results from this analysis. The figure and table titles correspond to the order they are presented in the manuscript.

* Input_Data/

The input data used in the analysis of extreme heat regulation services in Brazilian Cerrado and Amazon ecosystems. Better detail on the input data is presented in the file Input_Data/READMEinput.md

* Output/

Results obtained in the analysis, used to produce the figures and tables presented in the Figures/ folder.

## Instructions

The repository with all codes and input data occupies around 6.9Gb. It can be downloaded in its entirety, or in parts.

In case the figures are to be replicated
* The script Result_Analysis.R, the folder Output/ and Figures/ are necessary to download
* The script Result_Analysis.R needs to be run in R. 

In case the results and figures are to be replicated: 
* The entire repository should be downloaded.
* The script Master.R shows the order in which the auxiliary scripts that process the data and calculate the indicators of relevance should be run. 



