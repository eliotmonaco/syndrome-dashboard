library(rsatscan)
library(tidyverse)
library(sf)

source("scripts/fn.R")

indir <- "data/satscan-input/"
outdir <- "data/satscan-output/"

# Import data files
dd <- readRDS("data/essence_data_details.rds")
centroids <- readRDS("data/kc_zcta_centroids.rds")

dd$pat <- lapply(dd$pat, \(ls) ls$data)
dd$hosp <- lapply(dd$hosp, \(ls) ls$data)

dd <- unlist(dd, recursive = FALSE)

# Case file: <location ID> <# cases> <date/time>
case_files <- imap(dd, \(df, i) {
  tryCatch(
    expr = {
      df <- config_casefile(df)

      nm <- unlist(strsplit(i, "\\."))

      nm <- paste0(nm[2], "-", nm[1])

      write.cas(df, indir, nm)

      list(name = nm, data = df)
    },
    error = function(e) e
  )
})

# Coordinates file: <location ID> <latitude> <longitude>
geo_file <- centroids |>
  st_drop_geometry() |>
  select(zcta, lat, long)

write.geo(geo_file, indir, "kc-zctas")

# Parameter file
nm <- unlist(nm)

prm_files <- lapply(case_files, \(ls) {
  # Set Satscan options to defaults
  invisible(ss.options(reset = TRUE, version = "10.3"))

  date_min <- format(min(ls$data$date), "%Y/%m/%d")
  date_max <- format(max(ls$data$date), "%Y/%m/%d")

  # Set options for the analysis
  ss.options(list(
    # Input
    CaseFile = paste0(indir, ls$name, ".cas"),
    PrecisionCaseTimes = 3, # day
    StartDate = date_min,
    EndDate = date_max,
    CoordinatesFile = paste0(indir, "kc-zctas.geo"),
    CoordinatesType = 1, # lat/long

    # Analysis
    AnalysisType = 4, # prospective spacetime
    ModelType = 2, # spacetime permutation
    ScanAreas = 1, # high rates
    TimeAggregationUnits = 3, # day

    # Output
    OutputGoogleEarthKML = "y",
    OutputShapefiles = "n",
    OutputCartesianGraph = "y",
    # MostLikelyClusterEachCentroidDBase = "n",
    # MostLikelyClusterCaseInfoEachCentroidDBase = "n",
    # CensusAreasReportedClustersDBase = "n",
    # IncludeRelativeRisksCensusAreasDBase = "n",

    # Data Checking
    StudyPeriodCheckType = 1, # relaxed bounds
    GeographicalCoordinatesCheckType = 1, # relaxed coordinates

    # # Locations network
    # LocationsNetworkFilename = "", # NETWORK FILE USED IN NYC STUDY
    # UseLocationsNetworkFile = "y",

    # Spatial Window
    MaxSpatialSizeInPopulationAtRisk = 50,

    # Temporal window
    MinimumTemporalClusterSize = 2, # 2 days
    MaxTemporalSizeInterpretation = 1, # interpret as time
    MaxTemporalSize = 30, # 30 days

    # Space and Time Adjustments
    AdjustForWeeklyTrends = "y",

    # Inference
    MonteCarloReps = 999,
    ProspectiveStartDate = "1900/01/01",

    # Cluster Drilldown
    DrilldownClusterCutoff = 0.05, # DIFFERENTLY WORDED - SAME PARAM?

    # Miscellaneous Analysis
    ProspectiveFrequencyType = 1, # daily

    # Spatial Output
    LaunchMapViewer = "n",
    CompressKMLtoKMZ = "y",
    IncludeClusterLocationsKML = "n",
    ReportHierarchicalClusters = "y",
    CriteriaForReportingSecondaryClusters = 1, # NoCentersInOther

    # Temporal output
    OutputTemporalGraphHTML = "y",
    TemporalGraphReportType = 2, # report only significant clusters
    TemporalGraphSignificanceCutoff = 1, # cluster p-value cutoff for reporting
    # tutorial uses 0.01, but this results in
    # an error when running `satscan()`

    # Other output
    # PARAMS NOT AVAILABLE
    # ClusterSignificanceByRecurrence = "y",
    # ClusterSignificanceRecurrenceCutoff = 100,
    # ClusterSignificanceRecurrenceCutoffType = 3,
    # ClusterSignificanceByPvalue = "n",
    # ClusterSignificancePvalueCutoff, = 0.05

    # Line list
    # PARAMS NOT AVAILABLE
    # LineListCaseFile = "n",
    # LineListHeaderCaseFile = "n",
    # LineListEventCache = "...\input files\event_cache.txt",
    # EventGroupKML = "y",
    # EventGroupByKML = "disease_status_final",

    # Run Options
    LogRunToHistoryFile = "n"
  ))

  write.ss.prm(outdir, ls$name)

  ls$name
})

# Run Satscan
ssresults <- lapply(prm_files, \(x) {
  satscan(
    prmlocation = outdir,
    prmfilename = x,
    sslocation = "C:/Program Files/SaTScan",
    ssbatchfilename = "SaTScanBatch64",
    verbose = TRUE
  )
})

# Save results
saveRDS(ssresults, paste0(outdir, "satscan_results.rds"))

