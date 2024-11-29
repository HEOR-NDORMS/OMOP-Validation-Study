#install.packages("DBI")
# install.packages("RPostgres")
# install.packages("dbplyr")
# install.packages("dplyr")
# install.packages("devtools")
# install.packages("usethis")
# install.packages("remotes")
# install.packages("CDMConnector")
# install.packages("dm")
# install.packages("usethis")
# install.packages("duckdb")

#load packages from library
library(DBI)
library(RPostgres)
library(dbplyr)
library(dplyr)
library(devtools)
library(usethis)
library(duckdb)
library(CDMConnector)
library(dm)

# Connect to the database
db <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_SERVER_cdm_aurum_women_full_dbi"),
  port = Sys.getenv("DB_PORT"),
  host = Sys.getenv("DB_HOST"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Connect to the tables in profile
my_dm <- dm::dm_from_con(con = db,
                         schema = "validation")

#Collect the table called “_validation”

all_patients <- my_dm$`_validation` %>%  collect()


# Cleaning (following the same logic as CleaningFractures.R) ----

## Checking if there are cancer records before the birth year

cancer_yob<-all_patients %>% 
  dplyr::filter(cancer_fa_index_dt <= yob) %>% tally()

## Checking if there are bone disease records before the birth year

bone_yob <-all_patients %>% 
  dplyr::filter(paget_lpon_index_dt <= yob) %>% tally()

## Checking if there are fractures before the birth year

fx_yob <-all_patients %>% 
  dplyr::filter(index_date <= yob) %>% tally()

## Checking if there are fractures that happen on the same day as a trauma 

fx_trauma <- all_patients %>% 
  dplyr::filter(trauma_faon_index_dt == index_date) %>% tally()


## Checking if there are multiple fractures that happen on the same day

multiple_fx <-all_patients %>%
  dplyr::filter(index_hip_fx==1 & index_nhnv_fx==1 & index_vert_fx==1) %>% tally()


## Removing the necessary

all_patients_clean <- all_patients %>%
  filter(is.na(trauma_faon_index_dt) | index_date != trauma_faon_index_dt) %>%
  filter(!(index_hip_fx == 1 & index_nhnv_fx == 1 & index_vert_fx == 1))


# Cleaning and creating cohort (following the same logic as CohortRQ1&2) ----






