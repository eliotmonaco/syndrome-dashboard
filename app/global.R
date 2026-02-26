library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(highcharter)
library(DT)

source("../scripts/fn.R")
source("../scripts/syndromes.R")

# Essence data (configured)
dd <- readRDS("../data/essence_data_details.rds")
ts <- readRDS("../data/essence_time_series.rds")

# Satscan output
ssresults <- readRDS("../data/satscan-output/satscan_results.rds")

# KC ZCTA map
kcmap <- st_transform(kcData::sf_zcta_2024, crs = "WGS84")

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

# Syndrome list
syn_names <- as.list(names(syn))

names(syn_names) <- sapply(syn, \(ls) {
  x <- ls$name

  paste0(
    toupper(substr(x, 1, 1)),
    substr(x, 2, nchar(x))
  )
})

max(date_range) - 14
