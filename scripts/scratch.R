# Create dashboard data (TEST)

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
dir_data <- "data/test/"
dir.create(dir_data)

source("scripts/fn.R")
source("scripts/syndromes.R")
syn <- syn[1:2]
source("scripts/get-ess.R")
source("scripts/satscan.R")








inputsyn <- "resp"

inputdtrng <- daterng1$`One year`

# Data details
dd <- dbdata |>
  get_list_data(max(dt), "dd") |>
  get_dd_data(inputsyn, inputdtrng)



ss <- get_list_data(dbdata, max(dt), "ss")




# Filter cluster data
clustdata <- config_syndrome_data(ss, inputsyn, TRUE, TRUE, geo)

# Filter cluster locations
clustloc <- list(
  patient = filter_location_geometries(
    clustdata$patient,
    geo = geo$zctas,
    var = "GEOID20"
  ),
  hospital = filter_location_geometries(
    clustdata$hospital,
    geo = clustdata$hospital$shapeclust,
    var = "loc_id"
  )
)

# Cluster map (by patient)
cluster_map(
  cluster_locations = clustloc$patient,
  location_boundaries = geo$zctas,
  kc_boundary = geo$city,
  gp = gp_pat
)

# Cluster map (by hospital)
cluster_map(
  cluster_locations = clustloc$hospital,
  cluster_points = clustdata$hospital$shapegis,
  location_boundaries = geo$counties,
  kc_boundary = geo$city,
  hospital_locations = geo$hosp,
  gp = gp_hosp
)

# Cluster count table
ssresults |>
  significant_clusters_by_syndrome(syndrome = syn) |>
  clustcount_table()

# Cluster data table (by patient)
cluster_table(clustdata$patient$shapeclust)

# Location data table (by patient)
location_table(clustdata$patient$gis, id = 1, type = "patient")
location_table(clustdata$hospital$gis, id = 1, type = "hospital")







ddhosp <- lapply(dd, \(ls) {list_rbind(ls$hospital)}) |>
  list_rbind()

ddpat <- lapply(dd, \(ls) {list_rbind(ls$patient)}) |>
  list_rbind()

apply(ddpat, 2, \(x) setmeup::pct(sum(grepl("^none$", x)), nrow(ddpat), 1))

apply(ddhosp, 2, \(x) setmeup::pct(sum(grepl("^none$", x)), nrow(ddhosp), 1))

ddpat |>
  count(patient_state)

ddpat |>
  count(patient_country)

ddpat |>
  count(hospital_name)

ddpat |>
  count(hospital_state)

ddhosp |>
  count(patient_state)

ddhosp |>
  count(patient_country)

ddhosp |>
  count(hospital_name)

ddhosp |>
  count(hospital_state)









