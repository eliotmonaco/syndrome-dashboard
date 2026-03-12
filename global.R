library(shiny)
library(bslib)
library(tidyverse)
library(setmeup)
library(sf)
library(highcharter)
library(reactable)
library(leaflet)

source("scripts/fn.R")
source("scripts/syndromes.R")

# Analysis dates
dirs <- list.dirs("data/", full.names = TRUE, recursive = FALSE)
dirs <- dirs[grepl("^data/an-", dirs)]
dt <- as.Date(sub("^data/an-", "", dirs))

# Essence data (configured)
# dd <- lapply(dirs, \(x) readRDS(paste0(x, "/essence_data_details.rds")))
dd <- readRDS(paste0("data/an-", max(dt), "/essence_data_details.rds"))
# ts <- lapply(dirs, \(x) readRDS(paste0(x, "/essence_time_series.rds")))
ts <- readRDS(paste0("data/an-", max(dt), "/essence_time_series.rds"))

# Satscan output
ssfull <- lapply(dirs, \(x) {
  readRDS(paste0(x, "/satscan-output/satscan_results.rds"))
})
names(ssfull) <- gsub("data/|-", "", dirs)
# ssresults <- readRDS("data/satscan-output/satscan_results.rds")

# Date sequence for time series date input
date_seq <- readRDS(paste0("data/an-", max(dt), "/date_range.rds"))
# date_seq <- readRDS("data/date_seq.rds")

# Spatial data
geo <- readRDS("data/geographic_data.rds")

# Radio button date options
date_buttons <- list(
  "Two weeks" = as.character(max(date_seq) - 14),
  "30 days" = as.character(max(date_seq) - 30),
  "90 days" = as.character(max(date_seq) - 90),
  "180 days" = as.character(max(date_seq) - 180),
  "One year" = as.character(max(date_seq) - 365)
)

# Syndrome list for select input
syn_names <- as.list(names(syn))

names(syn_names) <- sapply(syn, \(ls) {
  x <- ls$name

  paste0(
    toupper(substr(x, 1, 1)),
    substr(x, 2, nchar(x))
  )
})

# UI text
clustcount_tbl_text <- paste(
  "Spatiotemporal clusters are detected using SaTScan software. This table",
  "shows the number of clusters where p < 0.05 for each syndrome."
)

loc_val <- paste(
  "Select a cluster on the map or the cluster table to see location details"
)

clust_val <- "No clusters detected"

ts_pat_text <- paste(
  "This dataset consists of ER visit records for patients residing in Clay,",
  "Jackson, or Platte County."
  # "This dataset consists of records in which the patient's ZIP code is at",
  # "least partly within the Kansas City boundary."
)

ts_hosp_text <- paste(
  "This dataset consists of ER visit records from hospitals in Kansas City."
)
