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

# Date sequence for time series date input
date_seq <- readRDS(paste0("data/an-", max(dt), "/date_range.rds"))

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
uitext <- list(
  cctbl = paste(
    "Spatiotemporal clusters are detected using SaTScan software. This table",
    "shows the number of clusters where p < 0.05 for each syndrome."
  ),
  val_loc = paste(
    "Select a cluster on the map or the cluster table to see location details"
  ),
  val_clust = "No clusters detected",
  tspat = paste(
    "This dataset consists of ER visit records for patients residing in Cass,",
    "Clay, Jackson, and Platte County ZIP codes."
  ),
  tshosp = paste(
    "This dataset consists of ER visit records from hospitals in Cass, Clay,",
    "Jackson, and Platte Counties."
  )
)

# Graphical parameters for cluster map shapes and markers
gp_pat <- list(
  study = list(
    name = "Study area (ZCTA)",
    clr = "#aaa",
    fill = "#aaa",
    wt = 2,
    opac1 = 1,
    opac2 = .1,
    shp = "square"
  ),
  kc = list(
    name = "KC boundary",
    clr = "#024cbf",
    fill = "#024cbf",
    wt = 2,
    opac1 = 1,
    opac2 = .1,
    shp = "square"
  ),
  clust = list(
    name = "Syndrome cluster",
    clr = "red",
    fill = "red",
    wt = 2,
    opac1 = .5,
    opac2 = .2,
    shp = "square"
  )
)

gp_hosp <- list(
  study = list(
    name = "Study area (county)",
    clr = "#aaa",
    fill = "#aaa",
    wt = 2,
    opac1 = 1,
    opac2 = .1,
    shp = "square"
  ),
  kc = list(
    name = "KC boundary",
    clr = "#024cbf",
    fill = "#024cbf",
    wt = 2,
    opac1 = 1,
    opac2 = .1,
    shp = "square"
  ),
  hosp = list(
    name = "Hospital",
    class = "plus-legend"
  ),
  clust = list(
    name = "Syndrome cluster",
    clr = "red",
    fill = "red",
    wt = 2,
    opac1 = .5,
    opac2 = .2,
    shp = "circle"
  )
)

