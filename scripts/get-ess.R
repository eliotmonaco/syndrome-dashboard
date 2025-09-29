# Get time series and/or data details from Essence via API

library(Rnssp)

# load Essence profile object, needed for `get_api_data()`
load("C:/Users/emonaco01/OneDrive - City of Kansas City/Documents/r/essence/myProfile.rda")

source("scripts/syndromes.R")
source("scripts/fn.R")

ess <- list()

# get time series
urls <- build_url(syn, start = "2024-01-01", type = "ts")

system.time(
  ess$ts <- lapply(urls, \(x) {
    ls <- get_api_data(
      x,
      col_types = readr::cols(.default = "c")
    )

    ls$timeSeriesData
  })
)

names(ess$ts) <- names(syn)

# get data details
urls <- build_url(syn, start = "2024-01-01", type = "dd")

system.time(
  ess$dd <- lapply(urls, \(x) {
    get_api_data(
      x,
      fromCSV = TRUE,
      col_types = readr::cols(.default = "c")
    )
  })
)

names(ess$dd) <- names(syn)

saveRDS(ess, "data/ess_raw_data.rds")
