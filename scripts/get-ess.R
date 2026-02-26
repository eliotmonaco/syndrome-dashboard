# Get Essence data via API

library(Rnssp)
library(tidyverse)
library(setmeup)

# Load Essence profile object, needed for `get_api_data()`
load("C:/Users/emonaco01/OneDrive - City of Kansas City/Documents/r/essence/myProfile.rda")

source("scripts/fn.R")
source("scripts/syndromes.R")

# Build URLs and pull data ------------------------------------------------

t0 <- Sys.time()

# Start date = 1 year and 1 day before current date
start_date <- Sys.Date() - lubridate::years(1) - lubridate::days(1)

# Syndrome API strings
syn_api <- lapply(syn, \(ls) ls$apistring)

# Data details fields
flds <- c(
  "Date", "Time", "Age", "Sex", "DateOfBirth", "Travel",
  "ZipCode", "Patient_City", "Patient_State", "Patient_Country",
  "HospitalName", "HospitalState", "Facility_State",
  "VisitNumber", "Patient_ID", "MedicalRecordNumber"
)

# Pull data by both patient location and hospital location
datasrc <- c("patient", "hospital")

# Build URLs
urlsdd <- lapply(datasrc, \(x) { # data details
  build_ess_url(
    syndrome = syn_api,
    start = start_date,
    data_source = x,
    output = "dd",
    dd_fields = flds
  )
})
names(urlsdd) <- datasrc

urlsts <- lapply(datasrc, \(x) { # time series
  build_ess_url(
    syndrome = syn_api,
    start = start_date,
    data_source = x,
    output = "ts"
  )
})
names(urlsts) <- datasrc

# Get data
t1 <- Sys.time()

ddraw <- lapply(urlsdd, \(x) {
  lapply(x, \(y) capture_message(get_ess_dd(y)))
})

tsraw <- lapply(urlsts, \(x) {
  lapply(x, \(y) capture_message(get_ess_ts(y)))
})

t2 <- Sys.time()

names(ddraw$patient) <- names(syn)
names(ddraw$hospital) <- names(syn)
names(tsraw$patient) <- names(syn)
names(tsraw$hospital) <- names(syn)

# Create log entry --------------------------------------------------------

# Pull all query names and pad for text output
qnm <- sapply(syn, \(ls) ls$queryname)

qnm <- str_pad(
  qnm,
  width = max(nchar(qnm)),
  side = "right"
)

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

logtbl <- data.frame(
  query = qnm,
  message1 = msgdd$patient,
  message2 = msgdd$hospital,
  message3 = msgts$patient,
  message4 = msgts$hospital
)

# Create new var names and pad for text output
colnames(logtbl) <- c(
  str_pad("SYNDROME QUERY", width = max(nchar(qnm)), side = "right"),
  str_pad("DD BY PATIENT", width = max(nchar(msgdd$patient)), side = "right"),
  str_pad("DD BY HOSPITAL", width = max(nchar(msgdd$hospital)), side = "right"),
  str_pad("TS BY PATIENT", width = max(nchar(msgts$patient)), side = "right"),
  "TS BY HOSPITAL"
)

tf <- tempfile(fileext = ".txt")

write.table(
  logtbl,
  file = tf,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE
)

log <- readLines(tf)

dur <- t2 - t1

log <- c(
  paste0(Sys.Date(), "\n"),
  paste(
    "Essence download started at",
    format(t0, "%I:%M %p"), "\n"
  ),
  paste(
    "Download time:",
    round_ties_away(as.numeric(dur), 2),
    units(dur), "\n"
  ),
  log
)

# Configure data ----------------------------------------------------------

dd <- lapply(ddraw, \(ls1) {
  lapply(ls1, \(ls2) {
    tryCatch(
      config_dd(ls2$data),
      error = function(e) e
    )
  })
})

ts <- lapply(tsraw, \(ls1) {
  lapply(ls1, \(ls2) {
    tryCatch(
      config_ts(ls2$data),
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

# Separate data from API messages
ddraw <- lapply(ddraw, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

tsraw <- lapply(tsraw, \(ls1) {
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

writeLines(log, "data/log.txt")
saveRDS(ess_raw, "data/essence_raw.rds")
saveRDS(dd, "data/essence_data_details.rds")
saveRDS(ts, "data/essence_time_series.rds")
saveRDS(dderror, "data/data_details_deduplication_error.rds")
saveRDS(
  seq.Date(start_date, Sys.Date(), "day"),
  "data/date_range.rds"
)

