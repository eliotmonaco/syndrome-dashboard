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

# Import dashboard data
dbdata <- readRDS("data/dashboard_data.rds")

# Data directories
dirs <- list.dirs("data/", full.names = TRUE, recursive = FALSE)
dirs <- dirs[grepl("^data/an-", dirs)]

# Analysis date input list
dt <- as.Date(sub("^data/an-", "", dirs))

# Initial syndrome select input list
synselect1 <- dbdata |>
  get_list_data(max(dt), "syn") |>
  syn_select_list()

# Initial date range input list
daterng1 <- dbdata |>
  get_list_data(max(dt), "daterng") |>
  daterange_select_list()

# Spatial data
geo <- readRDS("data/geographic_data.rds")

# UI text
uitext <- list(
  sigp = HTML("Show clusters where p&nbsp;<&nbsp;0.05 only"),
  cctbl = HTML(paste(
    "Spatiotemporal clusters are detected using SaTScan software. This table",
    "shows the number of clusters where p&nbsp;<&nbsp;0.05 for each syndrome."
  )),
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

