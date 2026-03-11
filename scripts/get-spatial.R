# Get spatial data

library(kcData)
library(tidyverse)
library(sf)

source("scripts/fn.R")

options(tigris_use_cache = TRUE)

# Download ZCTAs
ls <- list(
  geo = "zcta",
  year = 2024,
  intersect = list("city", "city", "metro", "metro"),
  geometry = list("clipped", "full", "clipped", "full")
)

zctas <- pmap(ls, get_kc_sf)

names(zctas) <- paste(ls$intersect, ls$geometry, sep = "_")

# Transform CRS to WGS84
zctas <- lapply(zctas, st_transform, crs = "WGS84")

# Get centroids
centroids <- lapply(zctas[grepl("full", names(zctas))], get_centroids)

# Visualize
ggplot() +
  geom_sf(
    data = zctas$city_full,
    color = "darkgreen",
    fill = "lightgreen",
    alpha = .5
  ) +
  geom_sf(
    data = zctas$city_clipped,
    color = "blue",
    fill = "lightblue",
    alpha = .5
  ) +
  geom_sf(
    data = centroids$city_full,
    color = "red"
  )

ggplot() +
  geom_sf(
    data = zctas$metro_full,
    color = "darkgreen",
    fill = "lightgreen",
    alpha = .5
  ) +
  geom_sf(
    data = zctas$metro_clipped,
    color = "blue",
    fill = "lightblue",
    alpha = .5
  ) +
  geom_sf(
    data = centroids$metro_full,
    color = "red"
  )

saveRDS(zctas, "data/zctas.rds")
saveRDS(centroids, "data/centroids.rds")

