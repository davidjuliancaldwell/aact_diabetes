### R code for analyzing AACT/clinicaltrials.gov database for diabetes related trials"

This is the R code to parse ClinicalTrials.gov data compiled by AACT. Running the file **"diabetesTrials_automated.RR"** will reproduce the figures and perform the statistics.

The source directory/working directory should be set to wherever this R file is saved.

Before running the **"diabetesTrials_automated.RR"** script, an individual must make an account with the
AACT website via the following link.  
*https://aact.ctti-clinicaltrials.org/users/sign_up*

Within the R script **"diabetesTrials_automated.RR"** are several variables that should be set before running the script.   
**savePlot** - this variable is boolean (TRUE/FALSE), and determines whether the plots generated will be saved in the working directory.    
**saveData** - this variable is boolean (TRUE/FALSE), and determines whether the data generated will be saved in the working directory.  
**userAACT** - this variable needs to be set as a string to whatever user name the user has setup with the above link. e.g. "user_name".  
**passwordAACT** - this variable needs to be set as a string to whatever password the user has setup with the above link. e.g. "user_name".


---
R Packages required for running this analysis.
tidyr  
RPostgreSQL  
dplyr  
plyr  
stringr  
lubridate  
ggplot2  
ggsci  
gridExtra  
cowplot  
here  
emmeans
sjPlot
coin
nnet
stargazer

Install with install.packges('packageName')

---

BSD-3 License  
David Caldwell, on behalf of the coauthors
