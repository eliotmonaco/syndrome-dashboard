# Run Satscan analyses

# library(rsatscan)
# library(tidyverse)
# library(sf)
# library(setmeup)
#
# source("scripts/fn.R")
#
# dd <- readRDS(paste0(dir_data, "essence_data_details.rds"))
# geo <- readRDS("data/geographic_data.rds")

dd <- unlist(dd, recursive = FALSE)

t0 <- Sys.time()

dir_in <- paste0(dir_data, "satscan-input/")
dir_out <- paste0(dir_data, "satscan-output/")

dir.create(dir_in)
dir.create(dir_out)

# Satscan analysis --------------------------------------------------------

# Case file: <location ID> <# cases> <date/time>
case_files <- imap(dd, \(df, i) {
  tryCatch(
    expr = {
      nm <- unlist(strsplit(i, "\\."))

      nm <- paste0(nm[2], "-", nm[1])

      if (grepl("^patient", i)) {
        # Summarize by date and ZIP code
        df <- config_casefile(df, var = "zip_code")
      } else if (grepl("^hospital", i)) {
        # Summarize by date and hospital name
        df <- config_casefile(df, var = "hospital_name")
      }

      write.cas(df, dir_in, nm)

      list(name = nm, data = df)
    },
    error = function(e) e
  )
})

# Coordinates file: <location ID> <latitude> <longitude>
geo_file_pat <- geo$zcta_pts |>
  st_drop_geometry() |>
  select(zcta, lat, long)

geo_file_hosp <- geo$hosp |>
  st_drop_geometry() |>
  select(hospital_name_geo, lat, long)

write.geo(geo_file_pat, dir_in, "zctas")
write.geo(geo_file_hosp, dir_in, "hospitals")

# Parameter file
prm_files <- lapply(case_files, \(ls) {
  # Set Satscan options to defaults
  invisible(ss.options(reset = TRUE, version = "10.3"))

  # Set Satscan options
  set_ss_opts(
    casefile = paste0(dir_in, ls$name, ".cas"),
    coordfile = if (grepl("-patient$", ls$name)) {
      paste0(dir_in, "zctas.geo")
    } else if (grepl("-hospital$", ls$name)) {
      paste0(dir_in, "hospitals.geo")
    },
    start = format(min(ls$data$date), "%Y/%m/%d"),
    end = format(max(ls$data$date), "%Y/%m/%d")
  )

  write.ss.prm(dir_out, ls$name)

  ls$name
})

# Run Satscan
ssresults <- lapply(prm_files, \(x) {
  run_satscan(
    dir = dir_out,
    file = x,
    satscan_exe = "C:/Program Files/SaTScan/SaTScanBatch64"
  )

  # rsatscan::satscan(
  #   prmlocation = dir_out,
  #   prmfilename = x,
  #   sslocation = "C:/Program Files/SaTScan",
  #   ssbatchfilename = "SaTScanBatch64",
  #   verbose = TRUE
  # )
})

t1 <- Sys.time()

# Create log entry --------------------------------------------------------

# Import log
log <- readLines(paste0(dir_data, "log.txt"))

dur <- t1 - t0

# Find warnings or error messages in `cmd_output`
msg <- imap(ssresults, \(ls, i) {
  if (any(grepl("^Warning|^Error", ls$cmd_output))) {
    c(i, ls$cmd_output)
  }
})

log <- c(
  log,
  paste(
    "\nSatscan analysis started at",
    format(t0, "%I:%M %p"), "\n"
  ),
  paste(
    "Computation time:",
    round_ties_away(as.numeric(dur), 2),
    units(dur), "\n"
  ),
  "CMD warning/error output:\n",
  unlist(msg)
)

# Save --------------------------------------------------------------------

# Var names to lowercase
ssresults <- lapply(ssresults, \(ls) {
  lapply(ls, \(x) {
    if (is.data.frame(x)) {
      colnames(x) <- tolower(colnames(x))
    }

    x
  })
})

ssresults <- list(
  patient = ssresults[grepl("^patient", names(ssresults))],
  hospital = ssresults[grepl("^hospital", names(ssresults))]
)

names(ssresults$patient) <- sub("^patient\\.", "", names(ssresults$patient))
names(ssresults$hospital) <- sub("^hospital\\.", "", names(ssresults$hospital))

writeLines(log, paste0(dir_data, "log.txt"))
saveRDS(ssresults, paste0(dir_out, "satscan_results.rds"))

