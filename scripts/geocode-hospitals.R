# Geocode hospital locations

library(tidygeocoder)

hosp <- read.csv("data/hospitals.csv")

hosp <- hosp |>
  tidyr::separate_wider_delim(
    cols = address,
    delim = ", ",
    names = c("address", "city", "state"),
    cols_remove = TRUE
  ) |>
  tidyr::separate_wider_delim(
    state,
    delim = " ",
    names = c("state", "zip"),
    cols_remove = TRUE
  )

hosp_gc <- geocode(
  hosp,
  street = "address",
  city = "city",
  state = "state",
  postalcode = "zip",
  method = "arcgis",
  full_results = TRUE
)

saveRDS(hosp_gc, "data/hospital_locations.rds")

