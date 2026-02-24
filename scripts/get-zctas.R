# Get centroid coordinates and simple features for KC ZCTAs

library(kcData)
library(tidyverse)
library(sf)

kc_zctas <- kcData:::get_raw_sf2("zcta", 2024)

kc_zctas <- kc_zctas |>
  filter(ZCTA5CE20 %in% unique(unlist(geoids$zcta)))

centroids <- kc_zctas |>
  st_centroid() |>
  bind_cols(
    kc_zctas |>
      st_centroid() |>
      st_coordinates()
  ) |>
  select(zcta = ZCTA5CE20, long = X, lat = Y)

ggplot() +
  geom_sf(data = kc_zctas) +
  geom_sf(
    data = sf_city_2024,
    color = "blue",
    fill = "lightblue",
    alpha = .5
  ) +
  geom_sf(
    data = centroids,
    color = "red"
  )

saveRDS(kc_zctas, "data/kc_zctas.rds")
saveRDS(centroids, "data/kc_zcta_centroids.rds")

