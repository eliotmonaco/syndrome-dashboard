# Get Essence data via API

library(Rnssp)
library(tidyverse)
library(setmeup)

# Load Essence profile object, needed for `get_api_data()`
load("C:/Users/emonaco01/OneDrive - City of Kansas City/Documents/r/essence/myProfile.rda")

source("scripts/fn.R")
source("scripts/syndromes.R")

# Start date = 1 year and 1 day before current date
start_date <- Sys.Date() - lubridate::years(1) - lubridate::days(1)

# Data details fields
flds <- c(
  "Date", "Time", "Age", "Sex", "DateOfBirth",
  "ZipCode", "Travel", "HospitalName", "VisitNumber",
  "Patient_ID", "MedicalRecordNumber"
)

# Build URLs for data by patient location
url_pat <- build_ess_url(
  syndrome = lapply(syn, `[[`, 2),
  start = start_date,
  data_source = "patient",
  output = "dd",
  dd_fields = flds
)

# Build URLs for data by hospital location
url_hosp <- build_ess_url(
  syndrome = lapply(syn, `[[`, 2),
  start = start_date,
  data_source = "hospital",
  output = "dd",
  dd_fields = flds
)

# i <- 1:5
#
# url_pat <- url_pat[i]
# url_hosp <- url_hosp[i]
#
# url_pat[[2]] <- sub("startDate=19Feb2025&", "", url_pat[[2]])

# Get data
t1 <- Sys.time()

ess_pat <- lapply(url_pat, \(x) {
  capture_message(get_ess_dd(x))
})

names(ess_pat) <- names(syn)

ess_hosp <- lapply(url_hosp, \(x) {
  capture_message(get_ess_dd(x))
})

names(ess_hosp) <- names(syn)

t2 <- Sys.time()

# Create log entry
qnm <- sapply(syn, `[[`, 1)

qnm <- str_pad(
  qnm,
  width = max(nchar(qnm)),
  side = "right"
)

msg1 <- sapply(ess_pat, \(ls) {
  sub("\\n$", "", cli::ansi_strip(ls$message))
})

msg2 <- sapply(ess_pat, \(ls) {
  sub("\\n$", "", cli::ansi_strip(ls$message))
})

logtbl <- data.frame(
  query = qnm,
  message1 = msg1,
  message2 = msg2
)

colnames(logtbl) <- c(
  str_pad("SYNDROME QUERY", width = max(nchar(qnm)), side = "right"),
  str_pad("DATA BY PATIENT", width = max(nchar(msg1)), side = "right"),
  "DATA BY HOSPITAL"
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
  paste(
    "Dashboard updated on",
    format(Sys.time(), "%Y-%m-%d %I:%M %p"),
    "\n"
  ),
  paste(
    "Download time:",
    round_ties_away(as.numeric(t2 - t1), 2),
    "minutes\n"
  ),
  log
)

writeLines(log, "data/log.txt")

# Configure data
ess_pat_config <- lapply(ess_pat, \(ls) {
  tryCatch(
    config_ess(ls$data),
    error = function(e) e
  )
})

ess_hosp_config <- lapply(ess_hosp, \(ls) {
  tryCatch(
    config_ess(ls$data),
    error = function(e) e
  )
})

# Save
saveRDS(ess_pat_config, "data/data_by_patient.rds")
saveRDS(ess_hosp_config, "data/data_by_hospital.rds")

