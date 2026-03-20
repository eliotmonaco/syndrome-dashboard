# Get Essence data via API

# library(Rnssp)
# library(tidyverse)
# library(setmeup)
#
# # Load Essence profile object, needed for `get_api_data()`
# load("data/myProfile.rda")
#
# source("scripts/fn.R")
# source("scripts/syndromes.R")
#
# geo <- readRDS("data/geographic_data.rds")

# Build URLs and pull data ------------------------------------------------

t0 <- Sys.time()

# Start date = 1 year and 1 day before current date
start_date <- get_start_date(end_date)

date_range <- seq.Date(start_date, end_date, "day")

# Syndrome API strings
syn_api <- lapply(syn, \(ls) ls$apistring)

# Data details fields
flds <- c(
  "Date", "Time", "Age", "AgeGroup", "Sex", "DateOfBirth",
  "ZipCode", "Patient_City", "Patient_State", "Patient_Country", "Travel",
  "HospitalName", "HospitalState", "VisitNumber", "Patient_ID", "HasBeenE"
)

# Build URLs for data details and time series outputs by both patient and
# hospital location (4 total)
urldd <- list(
  patient = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "patient",
    output = "dd",
    dd_fields = flds,
    zipcodes = geo$zctas$GEOID20
  ),
  hospital = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "hospital",
    output = "dd",
    dd_fields = flds
  )
)

urlts <- list(
  patient = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "patient",
    output = "ts",
    zipcodes = geo$zctas$GEOID20
  ),
  hospital = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "hospital",
    output = "ts"
  )
)

# Get data
t1 <- Sys.time()

ddraw <- lapply(urldd, \(x) {
  lapply(x, \(y) {
    tryCatch(
      capture_message(get_ess_dd(y)),
      error = function (e) e
    )
  })
})

tsraw <- lapply(urlts, \(x) {
  lapply(x, \(y) {
    tryCatch(
      capture_message(get_ess_ts(y)),
      error = function (e) e
    )
  })
})

t2 <- Sys.time()

names(ddraw$patient) <- names(syn)
names(ddraw$hospital) <- names(syn)
names(tsraw$patient) <- names(syn)
names(tsraw$hospital) <- names(syn)

# Create log entry --------------------------------------------------------

# Pull message text
msgdd <- lapply(ddraw, \(ls1) {
  sapply(ls1, \(ls2) {
    sub("\\n$", "", cli::ansi_strip(ls2$message))
  })
})

msgts <- lapply(tsraw, \(ls1) {
  sapply(ls1, \(ls2) {
    sub("\\n$", "", cli::ansi_strip(ls2$message))
  })
})

df <- data.frame(
  SYNDROME_QUERY = sapply(syn, \(ls) ls$queryname),
  DD_BY_PATIENT = msgdd$patient,
  DD_BY_HOSPITAL = msgdd$hospital,
  TS_BY_PATIENT = msgts$patient,
  TS_BY_HOSPITAL = msgts$hospital
)

# Make a table easy to read in a text file
df <- readable_table(df, 30)

tf <- tempfile(fileext = ".txt")

write.table(
  df,
  file = tf,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

log <- readLines(tf)

dur <- t2 - t1

log <- c(
  paste0(Sys.Date(), "\n"),
  "---------- ESSENCE DATA DOWNLOAD ----------\n",
  paste("Started at", format(t0, "%I:%M %p")),
  paste(
    "Download time:",
    round_ties_away(as.numeric(dur), 2),
    units(dur), "\n"
  ),
  log,
  ""
)

# Configure data ----------------------------------------------------------

# Separate data from API messages
dd <- lapply(ddraw, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

ts <- lapply(tsraw, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

# Configure
dd <- lapply(dd, \(ls) {
  var = c("zip_code", "hospital_name")

  map2(ls, var, \(df, x) {
    tryCatch(
      config_dd(df, geo_var = x),
      error = function(e) e
    )
  })
})

ts <- lapply(ts, \(ls) {
  lapply(ls, \(df) {
    tryCatch(
      config_ts(df),
      error = function(e) e
    )
  })
})

# Separate data from error tables in data details
dderror <- lapply(dd, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$error_rate
  })
})

dd <- lapply(dd, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

# Combine raw data lists
ess_raw <- list(
  data_details = ddraw,
  time_series = tsraw
)

# Save --------------------------------------------------------------------

writeLines(log, paste0(dir_data, "log.txt"))
saveRDS(ess_raw, paste0(dir_data, "essence_raw.rds"))
saveRDS(dd, paste0(dir_data, "essence_data_details.rds"))
saveRDS(ts, paste0(dir_data, "essence_time_series.rds"))
saveRDS(dderror, paste0(dir_data, "data_details_deduplication_error.rds"))
saveRDS(date_range, paste0(dir_data, "date_range.rds"))
saveRDS(syn, paste0(dir_data, "syndrome_list.rds"))

