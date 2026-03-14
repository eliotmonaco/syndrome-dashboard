# Create dashboard data

library(Rnssp)
library(tidyverse)
library(setmeup)
library(rsatscan)
library(sf)

# Load Essence profile object, needed for `get_api_data()`
load("data/myProfile.rda")

# Import geographic data
geo <- readRDS("data/geographic_data.rds")

# Assign the end of the date range for Essence data download
end_date <- Sys.Date()

# Create a directory in `data/` for storing output
dir_data <- paste0("data/an-", end_date, "/")
dir.create(dir_data)

source("scripts/fn.R")
source("scripts/syndromes.R")
# syn <- syn[1:2]
source("scripts/get-ess.R")
source("scripts/satscan.R")

# for (i in 5:1) {
#   end_date <- Sys.Date() - i
#
#   dir_data <- paste0("data/an-", end_date, "/")
#
#   dir.create(dir_data)
#
#   source("scripts/get-ess.R")
#   source("scripts/satscan.R")
# }

