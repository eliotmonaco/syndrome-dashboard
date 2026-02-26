

url <- build_ess_url(
  syndrome = syn_api$resp,
  start = Sys.Date() - 5,
  data_source = "patient",
  output = "dd",
  dd_fields = NULL
)

df <- get_ess_dd(url)





library(sf)

clusters <- st_read("data/satscan-output/resp-patient.kml")
locations <- st_read("data/satscan-output/resp-patient.gis.shp")

kc <- kcData::sf_zcta_2024

kc <- st_transform(kc, crs = st_crs(clusters))

ggplot() +
  geom_sf(
    data = kc,
    linewidth = 1,
    fill = "black",
    alpha = .2
  ) +
  geom_sf(
    data = locations,
    color = "red"
  ) +
  geom_sf(
    data = clusters,
    fill = "red",
    color = NA,
    alpha = .2
  )



ssresults <- readRDS("data/satscan-output/satscan_results.rds")

kc <- kcData::sf_zcta_2024

kc <- st_transform(kc, crs = "WGS84")

clst <- ssresults$patient.resp$shapeclust

pts <- st_as_sf(
  ssresults$patient.resp$gis,
  coords = c("LOC_LONG", "LOC_LAT"),
  crs = st_crs(clst)
)

ggplot() +
  geom_sf(
    data = kc,
    linewidth = 1,
    fill = "black",
    alpha = .2
  ) +
  geom_sf(
    data = pts,
    color = "red"
  ) +
  geom_sf(
    data = clst,
    fill = "red",
    color = NA,
    alpha = .2
  )

library(DT)

clst |>
  st_drop_geometry() |>
  select(
    CLUSTER, START_DATE, END_DATE, NUMBER_LOC, TEST_STAT, P_VALUE,
    RECURR_INT, OBSERVED, EXPECTED, ODE
  ) |>
  datatable()










