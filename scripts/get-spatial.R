# Get spatial data

library(kcData)
library(tigris)
library(tidyverse)
library(sf)

source("scripts/fn.R")

hosp <- readRDS("data/hospital_locations.rds")

options(tigris_use_cache = TRUE)

# Get all MO ZCTAs
zctamap <- zctas(starts_with = c(63:65), year = 2024)

# Get all MO counties
comap <- counties(state = 29, year = 2024)

# Get KC boundary
kcmap <- get_kc_sf("place", 2024)

# Filter counties to the 4 KC counties
comap <- comap |>
  filter(GEOID %in% geoid$county)

# Get union of county geometries
counion <- st_union(comap)

# Filter ZCTAs by counties
sf1 <- st_filter(zctamap, counion, .predicate = st_intersects)
sf2 <- st_filter(zctamap, counion, .predicate = st_touches)
sf3 <- st_filter(zctamap, counion, .predicate = st_within)

idrm <- sf2$GEOID20[!sf2$GEOID20 %in% sf3$GEOID20]

zctaco <- sf1[!sf1$GEOID20 %in% idrm, ]

# Visualize
ggplot() +
  geom_sf(
    data = zctaco,
    color = "darkgreen",
    fill = "lightgreen",
    alpha = .5
  ) +
  geom_sf(
    data = counion,
    color = "black",
    linewidth = 1,
    fill = NA
  ) +
  geom_sf(
    data = kcmap,
    color = "blue",
    linewidth = 1,
    fill = NA
  )

# Transform CRS to WGS84
zctaco <- st_transform(zctaco, crs = "WGS84")
kcmap <- st_transform(kcmap, crs = "WGS84")

# Get centroids
zcta_pts <- get_centroids(zctaco)

# Geocoded hospitals to points
hosp <- hosp |>
  # Location name without spaces for Satscan coordinates file
  mutate(hospital_name_geo = gsub("\\s", "_", hospital_name)) |>
  st_as_sf(coords = c("long", "lat"), crs = "WGS84", remove = FALSE)

# Visualize
ggplot() +
  geom_sf(
    data = zctaco,
    color = "darkgreen",
    fill = "lightgreen",
    alpha = .5
  ) +
  geom_sf(
    data = counion,
    color = "black",
    linewidth = 1,
    fill = NA
  ) +
  geom_sf(
    data = kcmap,
    color = "blue",
    linewidth = 1,
    fill = NA
  ) +
  geom_sf(
    data = zcta_pts,
    color = "red"
  ) +
  geom_sf(
    data = hosp,
    color = "yellow"
  )

# Combine
geo <- list(
  zctas = zctaco,
  city = kcmap,
  counties = comap,
  zcta_pts = zcta_pts,
  hosp = hosp
)

# Save
saveRDS(geo, "data/geographic_data.rds")

