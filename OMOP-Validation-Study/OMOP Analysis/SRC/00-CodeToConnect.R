# ADD NECESSARY PACKAGES
#install.packages("renv") # if not already installed, install renv from CRAN
# renv::activate()
# renv::restore() # this should prompt you to install the various packages required for the study

library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(dbplyr)
library(stringi)
library(here)
library(RPostgres)
library(readxl)
library(DescTools)
library(openxlsx)
library(testthat)
library(lubridate)
library(PatientProfiles)
library(DrugUtilisation)
library(snakecase)
library(tidyr)
library(readr)
library(CodelistGenerator)
library(purrr)
library(MatchIt)
library(ggsurvfit)
library(htmltools)
library(matrixStats)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "aurum_full"

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here(paste0("Results_", db_name))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Database connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html 
# for more details.
# you may need to install another package for this 
# eg for postgres 
db <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_SERVER_cdm_aurum_women_full_dbi"),
  port = Sys.getenv("DB_PORT"),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public"

# The name of the schema where results tables will be created 
results_database_schema <- "results"

# Name of stem outcome table in the result schema where the outcome cohorts will
# be stored. 
# Notes: 
# - if there is an existing table in your results schema with the same names it
#   will be overwritten
# - more than one cohort will be created
# - name must be lower case
stem_table <- "gfa30"

# minimum counts that can be displayed according to data governance
minimum_counts <- 5

# study start date
study_start_date <- as.Date("2010-04-01")

# study end date
study_end_date <- as.Date("2018-03-31")

# prior obs
prior_observation <- 730

prefix <- stri_rand_strings(n =1, length = 6, '[a-z]')

# Country setting 
# Has to be one of the following: UK, Germany, France, Italy, Spain and Netherlands
country_setting <- "UK"

# create cdm reference ----

cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  writePrefix = prefix,
  .softValidation = TRUE
)
# check database connection
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

