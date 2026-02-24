library(shiny)
library(bslib)
library(tidyverse)
library(highcharter)

source("../scripts/fn.R")
source("../scripts/syndromes.R")

# Import configured Essence data
dd0 <- readRDS("../data/essence_data_details.rds")
ts0 <- readRDS("../data/essence_time_series.rds")

syn_names <- as.list(names(syn))

names(syn_names) <- sapply(syn, \(ls) {
  x <- ls$name

  paste0(
    toupper(substr(x, 1, 1)),
    substr(x, 2, nchar(x))
  )
})

