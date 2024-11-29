# ADD NECESSARY PACKAGES
#install.packages("renv") # if not already installed, install renv from CRAN
renv::activate() 
renv::restore() # this should prompt you to install the various packages required for the study

library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(dbplyr)
library(stringi)
library(here)
library(IncidencePrevalence)
library(readxl)
library(DescTools)
library(openxlsx)
library(CirceR) #remotes::install_github("ohdsi/CirceR")
library(testthat)
library(SqlRender)
library(lubridate)
library(PatientProfiles)
library(DrugUtilisation)
library(snakecase)
library(tidyr)
library(glmnet)
library(readr)
library(CodelistGenerator)
library(purrr)
library(MatchIt)
library(ggsurvfit)
library(tidycmprsk)
library(htmltools)
library(matrixStats)
library(hrbrthemes)
library(htmlwidgets) # to save the interactive plots as html file
library(plotly) # to create interactive plots
library(viridis) # for colour pellete
library(RColorBrewer) # for colour pellete
library(patchwork) # to combine plots together
  

  # load necessary functions
  source("/home/AD_NDORMS/qmao/functions.R")

  # cleaning fractures
  source("/home/AD_NDORMS/qmao/CleaningFractures.R")
  # generating cohort
