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

t0 <- Sys.time()

dir_in <- paste0(dir_data, "satscan-input/")
dir_out <- paste0(dir_data, "satscan-output/")

dir.create(dir_in)
dir.create(dir_out)

# Satscan analysis --------------------------------------------------------

# Case file: <location ID> <# cases> <date/time>
imap(dd$patient, \(df, i) {
  tryCatch(
    expr = {
      df <- config_casefile(df, var = "zip_code")
      write.cas(df, dir_in, paste0(i, "-patient"))
    },
    error = function(e) e
  )
})

imap(dd$hospital, \(df, i) {
  tryCatch(
    expr = {
      df <- config_casefile(df, var = "hospital_name_geo")
      write.cas(df, dir_in, paste0(i, "-hospital"))
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
imap(dd$patient, \(df, i) {
  # Set Satscan options to defaults
  invisible(ss.options(reset = TRUE, version = "10.3"))

  # Configure Satscan options
  set_ss_opts(
    casefile = paste0(dir_in, paste0(i, "-patient"), ".cas"),
    coordfile = paste0(dir_in, "zctas.geo"),
    start = format(min(df$date), "%Y/%m/%d"),
    end = format(max(df$date), "%Y/%m/%d")
  )

  write.ss.prm(dir_out, paste0(i, "-patient"))
})

imap(dd$hospital, \(df, i) {
  # Set Satscan options to defaults
  invisible(ss.options(reset = TRUE, version = "10.3"))

  # Configure Satscan options
  set_ss_opts(
    casefile = paste0(dir_in, paste0(i, "-hospital"), ".cas"),
    coordfile = paste0(dir_in, "hospitals.geo"),
    start = format(min(df$date), "%Y/%m/%d"),
    end = format(max(df$date), "%Y/%m/%d")
  )

  write.ss.prm(dir_out, paste0(i, "-hospital"))
})

# Run Satscan
ssresults <- imap(dd, \(ls, i) {
  imap(ls, \(x, j) {
    run_satscan(
      dir = dir_out,
      file = paste0(j, "-", i),
      satscan_exe = "C:/Program Files/SaTScan/SaTScanBatch64"
    )
  })
})

t1 <- Sys.time()

# Create log entry --------------------------------------------------------

# Import log
log <- readLines(paste0(dir_data, "log.txt"))

dur <- t1 - t0

# Find warnings or error messages in `ssresults$cmd_output`
msg <- imap(unlist(ssresults, recursive = FALSE), \(ls, i) {
  if (any(grepl("^Warning|^Error", ls$cmd_output))) {
    m <- c(
      paste("-", i),
      paste("   ", gsub("\n", "\n    ", str_wrap(ls$cmd_output, 80)))
    )

    m[!grepl("^\\s*$", m)]
  }
})

if (length(compact(msg)) == 0) {
  logmsg <- "CMD warning/error output: None"
} else {
  logmsg <- c("CMD warning/error output:\n", unlist(msg))
}

log <- c(
  log,
  "---------- SATSCAN ANALYSIS ----------\n",
  paste("Started at", format(t0, "%I:%M %p")),
  paste(
    "Computation time:",
    round_ties_away(as.numeric(dur), 2),
    units(dur), "\n"
  ),
  logmsg
)

# Save --------------------------------------------------------------------

# Var names to lowercase
ssresults <- lapply(ssresults, \(ls) {
  lapply(ls, \(ls2) {
    lapply(ls2, \(x) {
      if (is.data.frame(x)) {
        colnames(x) <- tolower(colnames(x))
      }

      x
    })
  })
})

writeLines(log, paste0(dir_data, "log.txt"))
saveRDS(ssresults, paste0(dir_out, "satscan_results.rds"))

