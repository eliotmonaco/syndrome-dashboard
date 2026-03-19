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
unlink(dir_data, recursive = TRUE, force = TRUE)
dir.create(dir_data)

source("scripts/fn.R")
source("scripts/syndromes.R")
source("scripts/get-ess.R")
source("scripts/satscan.R")

# Tally datasets and add to log
tally <- c(
  sapply(c(dd, ts), \(ls) {
    paste0(sum(sapply(ls, is.data.frame)), "/", length(syn))
  }),
  sapply(ssresults, \(ls) {
    paste0(sum(sapply(ls, \(ls2) !is.null(ls2))), "/", length(syn))
  })
)

tally <- paste(
  c(rep("Data details,", 2), rep("Time series,", 2), rep("Satscan output,", 2)),
  paste0(names(tally), ": ", tally)
)

log <- readLines(paste0(dir_data, "log.txt"))

log <- c(log, "", "---------- DATASETS ----------\n", tally, "")

writeLines(log, paste0(dir_data, "log.txt"))

# Combine all data and save
dirs <- list.dirs("data/", full.names = TRUE, recursive = FALSE)
dirs <- dirs[grepl("^data/an-", dirs)]

dbdata <- combine_all_data(dirs)

saveRDS(dbdata, "data/dashboard_data.rds")

