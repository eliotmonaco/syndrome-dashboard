# Get Essence data via API

library(Rnssp)
library(tidyverse)
library(setmeup)

# Load Essence profile object, needed for `get_api_data()`
load("C:/Users/emonaco01/OneDrive - City of Kansas City/Documents/r/essence/myProfile.rda")

source("scripts/fn.R")
source("scripts/syndromes.R")

t0 <- format(Sys.time(), "%Y-%m-%d %I:%M %p")

# Start date = 1 year and 1 day before current date
start_date <- Sys.Date() - lubridate::years(1) - lubridate::days(1)

# Syndrome API strings
syn <- syn[1:2]
syn_api <- lapply(syn, \(ls) ls$apistring)

# Data details fields
flds <- c(
  "Date", "Time", "Age", "Sex", "DateOfBirth",
  "ZipCode", "Travel", "HospitalName", "VisitNumber",
  "Patient_ID", "MedicalRecordNumber"
)

# Build URLs
urls <- list()

urls$ddpat <- build_ess_url( # data details, patient location
  syndrome = syn_api,
  start = start_date,
  data_source = "patient",
  output = "dd",
  dd_fields = flds
)

urls$ddhosp <- build_ess_url( # data details, hospital location
  syndrome = syn_api,
  start = start_date,
  data_source = "hospital",
  output = "dd",
  dd_fields = flds
)

urls$tspat <- build_ess_url( # time series, patient location
  syndrome = syn_api,
  start = start_date,
  data_source = "patient",
  output = "ts"
)

urls$tshosp <- build_ess_url( # time series, hospital location
  syndrome = syn_api,
  start = start_date,
  data_source = "hospital",
  output = "ts"
)

# Get data
t1 <- Sys.time()

ess_raw <- list()

ess_raw$ddpat <- lapply(urls$ddpat, \(x) {
  capture_message(get_ess_dd(x))
})

ess_raw$ddhosp <- lapply(urls$ddhosp, \(x) {
  capture_message(get_ess_dd(x))
})

ess_raw$tspat <- lapply(urls$tspat, \(x) {
  capture_message(get_ess_ts(x))
})

ess_raw$tshosp <- lapply(urls$tshosp, \(x) {
  capture_message(get_ess_ts(x))
})

t2 <- Sys.time()

names(ess_raw$ddpat) <- names(syn)
names(ess_raw$ddhosp) <- names(syn)
names(ess_raw$tspat) <- names(syn)
names(ess_raw$tshosp) <- names(syn)

# Create log entry
qnm <- sapply(syn, \(ls) ls$queryname)

qnm <- str_pad(
  qnm,
  width = max(nchar(qnm)),
  side = "right"
)

# Pull message text
msgs <- lapply(ess_raw, \(ls1) {
  sapply(ls1, \(ls2) {
    sub("\\n$", "", cli::ansi_strip(ls2$message))
  })
})

logtbl <- data.frame(
  query = qnm,
  message1 = msgs$ddpat,
  message2 = msgs$ddhosp,
  message3 = msgs$tspat,
  message4 = msgs$tshosp
)

# Create new var names and pad for text output
colnames(logtbl) <- c(
  str_pad("SYNDROME QUERY", width = max(nchar(qnm)), side = "right"),
  str_pad("DD BY PATIENT", width = max(nchar(msgs$ddpat)), side = "right"),
  str_pad("DD BY HOSPITAL", width = max(nchar(msgs$ddhosp)), side = "right"),
  str_pad("TS BY PATIENT", width = max(nchar(msgs$tspat)), side = "right"),
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

log <- c(
  paste0("Dashboard updated started ", t0, "\n"),
  paste(
    "Download time:",
    round_ties_away(as.numeric(t2 - t1), 2),
    "minutes\n"
  ),
  log
)

# Configure data
ess_config <- list()

ess_config$ddpat <- lapply(ess_raw$ddpat, \(ls) {
  tryCatch(
    config_dd(ls$data),
    error = function(e) e
  )
})

ess_config$ddhosp <- lapply(ess_raw$ddhosp, \(ls) {
  tryCatch(
    config_dd(ls$data),
    error = function(e) e
  )
})

ess_config$tspat <- map2(ess_raw$tspat, names(syn), \(ls, x) {
  tryCatch(
    config_ts(ls$data, x),
    error = function(e) e
  )
})

ess_config$tshosp <- map2(ess_raw$tshosp, names(syn), \(ls, x) {
  tryCatch(
    config_ts(ls$data, x),
    error = function(e) e
  )
})

# Combine time series tables
ts <- ess_config$tspat |>
  list_rbind() |>
  mutate(data_source = "patient") |>
  bind_rows(
    ess_config$tshosp |>
      list_rbind() |>
      mutate(data_source = "hospital")
  )

data_details <- ess_config[which(grepl("^dd", names(ess_config)))]

names(data_details) <- sub("^dd", "", names(data_details))

# Save
writeLines(log, "data/log-essence.txt")
saveRDS(ess_raw, "data/essence_raw.rds")
saveRDS(data_details, "data/essence_data_details.rds")
saveRDS(ts, "data/essence_time_series.rds")

