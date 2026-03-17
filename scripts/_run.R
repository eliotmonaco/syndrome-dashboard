# Create dashboard data

library(Rnssp)
library(tidyverse)
library(setmeup)
library(rsatscan)
library(sf)

# Load Essence profile object, needed for `get_api_data()`
load("data/myProfile.rda")

# Import data
geo <- readRDS("data/geographic_data.rds")
ansi <- readRDS("data/ansi_state_codes.rds")

# Assign the end of the date range for Essence data download
end_date <- Sys.Date()

# Create a directory in `data/` for storing output
dir_data <- paste0("data/an-", end_date, "/")
dir.create(dir_data)

source("scripts/fn.R")
source("scripts/syndromes.R")
source("scripts/get-ess.R")
source("scripts/satscan.R")

# Combine data
dirs <- list.dirs("data/", full.names = TRUE, recursive = FALSE)
dirs <- dirs[grepl("^data/an-", dirs)]

dbdata <- combine_all_data(dirs)

# Save
saveRDS(dbdata, "data/dashboard_data.rds")


# for (i in 6:0) {
#   end_date <- Sys.Date() - i
#
#   dir_data <- paste0("data/an-", end_date, "/")
#   dir.create(dir_data)
#
#   source("scripts/get-ess.R")
#   source("scripts/satscan.R")
# }

