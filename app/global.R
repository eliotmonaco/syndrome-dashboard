library(shiny)
library(bslib)
library(tidyverse)
library(setmeup)
library(sf)
library(highcharter)
library(reactable)
library(leaflet)

source("../scripts/fn.R")
source("../scripts/syndromes.R")

# Essence data (configured)
dd <- readRDS("../data/essence_data_details.rds")
ts <- readRDS("../data/essence_time_series.rds")

# Satscan output
ssresults <- readRDS("../data/satscan-output/satscan_results.rds")

# KC ZCTA maps
kczcta <- st_transform(kcData::sf_zcta_2024, crs = "WGS84")

kczctafull <- readRDS("../data/kc_zctas_full.rds")

kczctafull <- st_transform(kczctafull, crs = "WGS84")

# Hospital locations
hosploc <- readRDS("../data/hospital_locations.rds")

hosploc <- hosploc |>
  select(hospital_name, long, lat)

hosploc <- hosploc |>
  st_as_sf(coords = c("long", "lat"), crs = "WGS84") |>
  st_filter(kczcta)

# Date list
date_range <- readRDS("../data/date_range.rds")

# Radio button date options
date_buttons <- list(
  "Two weeks" = as.character(max(date_range) - 14),
  "30 days" = as.character(max(date_range) - 30),
  "90 days" = as.character(max(date_range) - 90),
  "180 days" = as.character(max(date_range) - 180),
  "One year" = as.character(max(date_range) - 365)
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

# Validation text
loc_val <- "Select a cluster on the map or the cluster table to see location details"

clust_val <- "No clusters detected"

