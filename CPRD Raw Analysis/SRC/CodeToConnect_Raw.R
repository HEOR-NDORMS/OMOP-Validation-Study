# Set up the R environ
# renv::activate()
# usethis::edit_r_environ()

# install.packages("DBI")
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

# load packages from library
library(DBI)
library(RPostgres)
library(dbplyr)
library(dplyr)
library(devtools)
library(usethis)
# library(duckdb)
library(CDMConnector)
library(dm)

# Connect to the database

server_dbi <- Sys.getenv("DB_SERVER_cdm_aurum_women_full_dbi") 
user <- Sys.getenv("DB_USER") 
password <- Sys.getenv("DB_PASSWORD") 
port <- Sys.getenv("DB_PORT") 
host <- Sys.getenv("DB_HOST")

db <- dbConnect(RPostgres::Postgres(), 
                dbname = server_dbi, 
                port = port,
                host = host, 
                user = user, 
                password = password)

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

# Collect the table called “_validation”

all_patients <- my_dm$`_validation` %>%  collect()
