
# This file includes all code that generate main results in the manuscript as following citation:

Xu Zhang, Jiinbao Li, Laibao Liu, Annually resolved atmospheric CO growth rate over the past nine centuries, Nature Communications, https://doi.org/10.1038/s41467-026-72220-2.

Here are the introduction:

## 1. System requirements
R (4.4.1) and RStudio (2024.09.1) are required to run the code.

## 2. Installation guide
Please open the file named "Project.Rproj" using RStudio. Then, execute the code in the order as introduced in Section 3. All code is saved in the "Code/" folder
The reconstruction normally takes 10 minutes on a computer with 8 cores.

## 3. Instruction for code
Here we introduce the scripts in the "Code/" folder. \
-- “functions/” folder\
This folder includes all functions that are used in following calculation. You can easily identify the source code of any functions by ctrl + left click.\
-- 0. library.r\
This script loads all packages that are used in this study. Meanwhile, the functions in "functions/" folder are sourced in this script.\
-- 11. read CGR.r\
This script reads CGR observations and applies high-pass filters.\
-- 12. read PAGS 2k.r\
This script reads PAGES 2k proxy records and applies high-pass filters.\
-- 13. Correlation between CGR and Temperature.r\
This script calculates the correlations between observed temperature and CGR at the gridded resolution.\
-- 14. Select Proxy Correlations.r\
This script selects proxy records as predictors using the criteria identified in the main text.\
-- 15. Read NBP.r\
This script reads NBP simulations in the TRENDY project.\
-- 21. Ensemble reconstruction.r\
This script coducts CGR reconstruction using PCR method. A total of 1000 ensemble members are reconstructed.\
-- 22. Compare with DVGMS and SOI.r\
This script compares the reconstructed CGR with NBP and SOI.\
-- 23. Sensitivity for VSSI.R\
This script examines the impacts of historical volcanic eruptions on reconstructed CGR variance.\
-- 31. Climate Data.r\
This script reads climate data, including Pre, Tmp, and scPDSI.\
-- 32. Largest CGR SD.r\
This script identifies the largest CGR variance during reconstruction period.\
-- 41. Cor with climatic variables.r\
This script calculates gridded correlations between CGR and climate variables.\
-- 42. land covers.r\
This script calculates the correlations between climate and CGR at individual land covers.\
-- 91. Plot CGR Reconstruction.r\
Plot Fig. 1 using ggplot2 package.\
-- 92. Plot CGR SD and climatic interpretation.r\
Plot Fig. 2 using ggplot2 package.\
-- 93. PDSI and CGR.r\
Plot Fig. 3 using ggplot2 package.\
-- Random Forest/11. Reconstruction RandomForest.R\
This script coducts CGR reconstruction using RF model. A total of 1000 ensemble members are reconstructed.\
-- Random Forest/12. Plot comparison with RF.R\
This script compares the results using PCR and RF.\
-- Alternative Reconstruction/11. Reconstruction_Cor predictors.R\
This script coducts CGR reconstruction using significantly correlated predictors.\
-- Alternative Reconstruction/12. Reconstruction_MLR predictors.R\
Alternative CGR reconstructio using mid-to-low latitude proxy.\
-- Alternative Reconstruction/13. Reconstruction_Low_predictors.R\
Alternative CGR reconstructio using low latitude proxy\
-- Alternative Reconstruction/14. Reconstruction Cor_S.R\
Alternative CGR reconstructio using proxy with correlations > 0.3 \
-- Alternative Reconstruction/21. Compare alternative CGR reconstruction.R\
Compare these alternative reconstructions.\
-- Alternative Reconstruction/31. Precipitation Observation.r\
This script read station-based precipitation observations and calculate their variance.\
-- Code for Model Simulations foler\
This folder includes all code that analyze PMIP simulations.\

## 4. Notes
4.1. All the figures will be saved to the folder "Figures/"\
4.2. Before running our code, please make sure that all packages specified in the "0. library.R" have been installed.\
4.3. Readers are required to download relevant data in the "Data Availability" section before running the code.\
4.4. The functions that authors defined are saved in the "functions/" folder. 


Shield: [![CC BY-NC 4.0][cc-by-nc-shield]][cc-by-nc]

This work is licensed under a
[Creative Commons Attribution-NonCommercial 4.0 International License][cc-by-nc].

[![CC BY-NC 4.0][cc-by-nc-image]][cc-by-nc]

[cc-by-nc]: https://creativecommons.org/licenses/by-nc/4.0/
[cc-by-nc-image]: https://licensebuttons.net/l/by-nc/4.0/88x31.png
[cc-by-nc-shield]: https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg
