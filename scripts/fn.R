# ESSENCE DATA ------------------------------------------------------------

# Build URL for Essence API
build_ess_url <- function(
    syndrome, # list of syndromes built as in `syndromes.R`
    start, # start date (YYYY-MM-DD)
    end = Sys.Date(), # end date (YYYY-MM-DD)
    data_source = c("hospital", "patient"),
    output = c("dd", "ts"),
    dd_fields = NULL,
    zipcodes = NULL
) {
  data_source <- match.arg(data_source)
  output <- match.arg(output)

  start <- as.Date(start); end <- as.Date(end)

  if (is.na(start) | is.na(end)) {
    stop("`start` and `end` must be valid dates formatted YYYY-MM-DD")
  }

  # Output parameters
  if (output == "dd") {
    if (is.null(dd_fields)) {
      params_out <- "aqtTarget=DataDetails"
    } else {
      dd_fields <- URLencode(dd_fields)

      dd_fields <- paste0("field=", c(dd_fields, "EssenceID"))

      params_out <- paste(
        c("aqtTarget=DataDetails", dd_fields),
        collapse = "&"
      )
    }

    op <- "dataDetails/csv?"
  } else if (output == "ts") {
    op <- "timeSeries?"

    params_out <- "aqtTarget=TimeSeries"
  }

  # Data source & geography parameters
  if (data_source == "hospital") {
    params_ds <- "datasource=va_hosp"

    params_geo <- paste(
      "geographySystem=hospital",
      "geography=mocenterpointercc",
      "geography=mochildrensmercyeastercc",
      "geography=mochildrensmercynorthlandercc",
      "geography=mochildrensmercyercc",
      "geography=moexcelsiorercc",
      "geography=moleessummitercc",
      "geography=molibertyercc",
      "geography=monothkcercc",
      "geography=moresearchkcercc",
      "geography=mostlukeseastercc",
      "geography=mostlukeskcercc",
      "geography=mostlukesnorthlandkcercc",
      "geography=mostmarysbluespringsercc",
      "geography=motrumanlakewoodercc",
      "geography=motrumanhospitalhillercc",
      sep = "&"
    )
  } else if (data_source == "patient") {
    params_ds <- "datasource=va_er"

    params_geo <- paste(
      "geographySystem=zipcode",
      paste0("geography=", paste(zipcodes, collapse = ",")),
      sep = "&"
    )
  }

  # Additional parameters
  params_add <- paste(
    "userId=5809",
    paste0("startDate=", format(start, "%d%b%Y")),
    paste0("endDate=", format(end, "%d%b%Y")),
    "percentParam=noPercent",
    "detector=probrepswitch",
    "timeResolution=daily",
    sep = "&"
  )

  params <- paste(
    params_out,
    params_ds,
    params_geo,
    params_add,
    sep = "&"
  )

  endpoint <- "https://moessence.inductivehealth.com/ih_essence/api/"

  paste0(
    endpoint,
    op,
    paste0(params, "&", syndrome)
  )
}

# Wrapper for `Rnssp::get_api_data()` to pull data details
get_ess_dd <- function(url, repair_colnames = TRUE) {
  Rnssp::get_api_data(
    url,
    fromCSV = TRUE,
    col_types = readr::cols(.default = "c"),
    name_repair = ifelse(repair_colnames, fix_colnames, "unique")
  )
}

# Wrapper for `Rnssp::get_api_data()` to pull time series
get_ess_ts <- function(url) {
  ls <- Rnssp::get_api_data(url)

  df <- ls$timeSeriesData

  colnames(df) <- fix_colnames(colnames(df))

  df$date <- as.Date(df$date)

  df
}

# Capture both the returned value and the message or error from a function call
capture_message <- function(expr) {
  output <- list()

  withCallingHandlers(
    expr,
    warning = function(w) {
      output <<- append(output, list(message = conditionMessage(w)))
    },
    message = function(m) {
      output <<- append(output, list(message = conditionMessage(m)))
    }
  )

  output <- append(output, list(data = expr), after = 0)

  # Remove the "no encoding supplied" message
  if ("message" %in% names(output)) {
    i <- which(names(output) == "message")

    p <- "No encoding supplied: defaulting to UTF-8."

    m <- output[i][!grepl(p, output[i])]

    m <- paste(m, collapse = " | ")

    output <- output[-i]

    output$message <- m
  }

  output
}

# Configure data downloaded from Essence
config_dd <- function(df, ansi_codes = ansi) {
  df <- df |>
    mutate(
      row_id = row_number(),
      date = as.Date(date, "%m/%d/%Y"),
      hospital_name = gsub("\\sOf\\s", " of ", str_to_title(hospital_name)),
      hospital_name_geo = gsub("\\s", "_", hospital_name)
    )

  # Find "exact" duplicates
  dupes <- suppressMessages(find_dupes(df, c(
    "date", "time", "age", "sex", "date_of_birth",
    "zip_code", "travel", "hospital_name",
    "visit_number"
  )))

  # Early return if no dupes are found
  if (is.null(dupes)) {
    return(list(
      data = select(df, -row_id),
      error_rate = NULL
    ))
  }

  # Remove redundant dupes
  df <- df |>
    anti_join(
      dupes |>
        distinct(dupe_id, .keep_all = TRUE),
      by = "row_id"
    )

  # Deduplicate using `visit_number`
  dupes <- suppressMessages(find_dupes(df, "visit_number"))

  # Early return if no dupes are found
  if (is.null(dupes)) {
    return(list(
      data = select(df, -row_id),
      error_rate = NULL
    ))
  }

  # If members of a dupe set have the same `patient_id`, `date`, and `zip_code`,
  # all but one are redundant. Only one from each set will be kept in `df`.
  ls <- lapply(unique(dupes$dupe_id), \(x) {
    dupeset <- dupes |> # filter a single dupe set
      filter(dupe_id == x)

    if (all(
      length(dupeset$patient_id) == 1,
      length(dupeset$date) == 1,
      length(dupeset$zip_code) == 1
    )) {
      dupeset[2:nrow(dupeset), ] # redundant dupes to remove
    }
  }) |>
    compact()

  # Remove redundant dupes
  if (length(ls) > 0) {
    df <- df |> # remove redundant dupes from `df`
      anti_join(ls, by = "row_id")

    dupes <- dupes |> # remove redundant dupe sets from `dupes`
      filter(!dupe_id %in% unique(ls$dupe_id))
  }

  # Among the remaining dupe sets, if they are indeed duplicates, it isn't
  # possible to determine which records have the correct date or ZIP code, both
  # of which are needed for the spatiotemporal analysis. Therefore, all records
  # will be kept. The potential error rate for both date and ZIP code will be
  # noted.

  # Find the number of duplicate sets with different values for each variable
  diffs <- lapply(unique(dupes$dupe_id), \(x) {
    dupeset <- dupes |> # filter a single dupe set
      filter(dupe_id == x)

    apply(dupeset, 2, \(c) { # find the number of unique values for each var
      length(unique(c))
    }) |>
      as.list() |>
      data.frame() |>
      mutate(
        n_dupes = nrow(dupeset),
        n_potential_errors = n_dupes - 1 # count the number of potential errors
      )                                  # presuming that one value (date or
  }) |>                                  # ZIP) is the correct one
    list_rbind()

  # Replace values > 1 with the number of potential errors. If the value is 1,
  # no potential errors are counted because the single value is presumed
  # correct.
  errors <- diffs |>
    mutate(across(
      -c(n_dupes, n_potential_errors),
      ~ ifelse(.x > 1, n_potential_errors, NA)
    )) |>
    select(-c(row_id, dupe_id, n_dupes, n_potential_errors))

  # Sum the potential errors for each variable
  errors <- colSums(errors, na.rm = TRUE) |>
    as_tibble(rownames = "var") |>
    rename(n = value) |>
    mutate(error_rate = n / nrow(df))

  # Clean values
  agegp <- c(
    "00-04" = "0-4", "05-17" = "5-17", "18-44" = "18-44",
    "45-64" = "45-64", "65-1000" = "65+", "Unknown" = "Unknown"
  )

  df <- df |>
    mutate(
      age_group = agegp[age_group],
      age_group = factor(age_group, agegp),
      patient_state2 = ansi_codes[patient_state],
      patient_state = if_else(
        grepl("[[:alpha:]]", patient_state),
        patient_state,
        patient_state2
      )
    ) |>
    select(-patient_state2)

  list(
    data = select(df, -row_id),
    error_rate = errors
  )
}

config_ts <- function(df) {
  # Add alert status, color, symbol, and radius
  lvl <- c("Normal", "Warning", "Anomaly")
  fill <- c("#0703fc", "#f2c00a", "#ff0000")
  clr <- c("#04029e", "#a17f03", "#a30202")
  shp <- c("circle", "diamond", "triangle")

  df <- df |>
    mutate(
      alert_status = case_when(
        color_id == 0 ~ lvl[1],
        color_id == 1 ~ lvl[1],
        color_id == 2 ~ lvl[2],
        color_id == 3 ~ lvl[3]
      ),
      alert_status = factor(alert_status, levels = lvl),
      alert_fill = fill[alert_status],
      alert_color = clr[alert_status],
      alert_symbol = shp[alert_status],
      alert_radius = 5,
      alert_line = 1
    )
}

# SPATIAL DATA ------------------------------------------------------------

get_centroids <- function(sf) {
  sf |>
    st_centroid() |>
    bind_cols(
      sf |>
        st_centroid() |>
        st_coordinates()
    ) |>
    select(zcta = ZCTA5CE20, long = X, lat = Y)
}

# SATSCAN -----------------------------------------------------------------

config_casefile <- function(df, var) {
  df |>
    filter(date < max(df$date)) |> # most recent date with complete data
    count(.data[[var]], date) |>
    select(all_of(var), n, date)
}

set_ss_opts <- function(casefile, coordfile, start, end) {
  ss.options(list(
    # Input
    CaseFile = casefile,
    PrecisionCaseTimes = 3, # day
    StartDate = start,
    EndDate = end,
    CoordinatesFile = coordfile,
    CoordinatesType = 1, # lat/long

    # Analysis
    AnalysisType = 4, # prospective spacetime
    ModelType = 2, # spacetime permutation
    ScanAreas = 1, # high rates
    TimeAggregationUnits = 3, # day

    # Output
    OutputGoogleEarthKML = "n",
    OutputShapefiles = "y",
    OutputCartesianGraph = "n",
    MostLikelyClusterEachCentroidDBase = "n",
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
    CompressKMLtoKMZ = "n",
    IncludeClusterLocationsKML = "n",
    ReportHierarchicalClusters = "y",
    CriteriaForReportingSecondaryClusters = 1, # NoCentersInOther

    # Temporal output
    OutputTemporalGraphHTML = "y",
    TemporalGraphReportType = 2, # report only significant clusters
    TemporalGraphSignificanceCutoff = 1, # cluster p-value cutoff for reporting
    # tutorial uses 0.01, but this results in
    # an error when running `satscan()`

    # Other output (PARAMS NOT AVAILABLE)
    # ClusterSignificanceByRecurrence = "y",
    # ClusterSignificanceRecurrenceCutoff = 100,
    # ClusterSignificanceRecurrenceCutoffType = 3,
    # ClusterSignificanceByPvalue = "n",
    # ClusterSignificancePvalueCutoff, = 0.05

    # Line list (PARAMS NOT AVAILABLE)
    # LineListCaseFile = "n",
    # LineListHeaderCaseFile = "n",
    # LineListEventCache = "...\input files\event_cache.txt",
    # EventGroupKML = "y",
    # EventGroupByKML = "disease_status_final",

    # Run Options
    LogRunToHistoryFile = "n"
  ))
}

run_satscan <- function(dir, file, satscan_exe) {
  inst_sf <- requireNamespace("sf", quietly = TRUE)

  if (!inst_sf) {
    message("The sf package must be installed to read shapefiles")
  }

  # Run Satscan batch executable
  cmd_output <- system(
    paste(shQuote(satscan_exe), paste0(dir, file, ".prm")),
    intern = TRUE
  )

  # Import Satscan outputs
  ls <- list(
    main = NA, col = NA, rr = NA, gis = NA, llr = NA,
    sci = NA, shapeclust = NA, shapegis = NA, prm = NA,
    cmd_output = cmd_output
  )

  xts <- c(
    ".txt", ".col.dbf", ".rr.dbf", ".gis.dbf",
    ".llr.dbf", ".sci.dbf", ".col.shp", ".gis.shp",
    ".col.prj", ".col.shx", ".gis.prj", ".gis.shx"
  )

  filenames <- sapply(xts, \(x) {
    paste0(dir, file, x)
  })

  if (file.exists(filenames[1])) {
    ls$main <- suppressWarnings(readLines(filenames[1]))
  }

  if (file.exists(filenames[2])) {
    ls$col <- foreign::read.dbf(filenames[2])
  }

  if (file.exists(filenames[3])) {
    ls$rr <- foreign::read.dbf(filenames[3])
  }

  if (file.exists(filenames[4])) {
    ls$gis <- foreign::read.dbf(filenames[4])
  }

  if (file.exists(filenames[5])) {
    ls$llr <- foreign::read.dbf(filenames[5])
  }

  if (file.exists(filenames[6])) {
    ls$sci <- foreign::read.dbf(filenames[6])
  }

  if (file.exists(filenames[7]) & inst_sf) {
    ls$shapeclust <- sf::st_read(dsn = dir, layer = paste0(file, ".col"))
  }

  if (file.exists(filenames[8]) & inst_sf) {
    ls$shapegis <- sf::st_read(dsn = dir, layer = paste0(file, ".gis"))
  }

  ls$prm <- readLines(paste0(dir, file, ".prm"))

  # Delete imported files
  suppressWarnings(file.remove(filenames))

  structure(ls, class = "satscan")
}

# SHINY DATA CONFIG -------------------------------------------------------

combine_all_data <- function(dirs) {
  ls <- lapply(dirs, \(d) {
    syn <- readRDS(paste0(d, "/syndrome_list.rds"))
    daterng <- readRDS(paste0(d, "/date_range.rds"))
    ts <- readRDS(paste0(d, "/essence_time_series.rds"))
    dd <- readRDS(paste0(d, "/essence_data_details.rds"))
    ss <- readRDS(paste0(d, "/satscan-output/satscan_results.rds"))

    list(syn = syn, daterng = daterng, ts = ts, dd = dd, ss = ss)
  })

  names(ls) <- gsub("data/|-", "", dirs)

  ls
}

get_list_data <- function(ls, date, name = NULL) {
  ls <- ls[[names(ls)[grepl(gsub("-", "", date), names(ls))]]]

  if (!is.null(name)) {
    ls[[name]]
  } else {
    ls
  }
}

syn_select_list <- function(ls) {
  ls2 <- as.list(names(ls))

  names(ls2) <- lapply(ls, \(x) x$name1)

  ls2
}

daterange_select_list <- function(x) {
  list(
    "Two weeks" = as.character(max(x) - 14),
    "30 days" = as.character(max(x) - 30),
    "90 days" = as.character(max(x) - 90),
    "180 days" = as.character(max(x) - 180),
    "One year" = as.character(max(x) - 365)
  )
}

filter_ess <- function(df, d1, d2 = NULL) {
  if (is.null(d2)) {
    d2 <- max(df$date)
  }

  df |>
    filter(
      date >= d1,
      date <= d2
    )
}

# Configure data for Highchart function
df_to_hc_list <- function(df) {
  list(
    list(
      data = lapply(1:nrow(df), \(r) {
        list(
          x = datetime_to_timestamp(df[r, "date"]),
          y = df[r, "count"],
          color = df[r, "alert_fill"],
          marker = list(
            symbol = df[r, "alert_symbol"],
            radius = df[r, "alert_radius"],
            lineWidth = df[r, "alert_line"],
            lineColor = df[r, "alert_color"]
          ),
          alert_status = df[r, "alert_status"]
        )
      })
    )
  )
}

get_ts_data <- function(ls, syndrome, daterange) {
  lapply(ls, \(ls2) {
    ls2[[syndrome]] |>
      filter_ess(as.Date(daterange)) |>
      df_to_hc_list()
  })
}

get_dd_data <- function(ls, syndrome, daterange) {
  lapply(ls, \(ls2) {
    ls2[[syndrome]] |>
      filter_ess(as.Date(daterange))
  })
}

significant_clusters_by_syndrome <- function(ls, syndromes) {
  ls <- lapply(ls, \(ls2) {
    x <- sapply(ls2, \(ls3) {
      if (is.data.frame(ls3$shapeclust)) {
        ls3$shapeclust |>
          st_drop_geometry() |>
          filter(p_value < .05) |>
          nrow()
      } else if (length(ls3) == 0) {
        NA
      } else if (is.na(ls3$shapeclust)) {
        0
      }
    })

    data.frame(
      abbr = names(x),
      clusters = x
    )
  })

  df <- ls$patient |>
    rename(clust_pat = clusters) |>
    bind_cols(
      ls$hospital |>
        select(clust_hosp = clusters)
    ) |>
    left_join(
      data.frame(
        syndrome = sapply(syndromes, \(ls) ls$name1),
        abbr = names(syndromes)
      ),
      by = "abbr"
    ) |>
    select(syndrome, clust_pat, clust_hosp)

  df
}

# Filter cluster and location data by p-value for a syndrome
filter_cluster_data <- function(ls, sig_pval) {
  if (!is.data.frame(ls$shapeclust) || nrow(ls$shapeclust) == 0) {
    return(ls)
  }

  if (sig_pval) {
    plvl <- .05
  } else {
    plvl <- 1.1
  }

  lapply(ls[grepl("gis|clust", names(ls))], \(df) {
    df <- df |>
      filter(p_value < plvl) |>
      mutate(lbl = paste("Cluster", cluster))

    if (nrow(df) == 0) {
      NULL
    } else {
      df
    }
  })
}

config_syndrome_data <- function(ls, syndrome, sig_pval) {
  ls <- lapply(
    list(
      patient = ls$patient[[syndrome]],
      hospital = ls$hospital[[syndrome]]
    ),
    filter_cluster_data,
    sig_pval = sig_pval
  )

  if (!is.null(ls$hospital$shapegis)) {
    # Find clusters with > 1 location and remove from `ls$hospital$shapegis` so
    # that only single point clusters in this dataset are mapped
    clusters <- ls$hospital$gis$cluster

    clusters <- unique(clusters[duplicated(clusters)])

    ls$hospital$shapegis <- ls$hospital$shapegis |>
      filter(!cluster %in% clusters)
  }

  ls
}

# Filter location geometries by cluster
filter_location_geometries <- function(ls, geo, var) {
  clust <- ls$shapeclust # contains clusters
  loc <- ls$gis # contains locations within each cluster

  if (!is.data.frame(clust) || nrow(clust) == 0) {
    return(NULL)
  }

  sf <- lapply(clust$cluster, \(x) {
    sfc <- geo |>
      filter(.data[[var]] %in% loc$loc_id[loc$cluster == x]) |>
      st_combine()

    st_set_geometry(data.frame(cluster = x), sfc)
  })

  sf <- do.call(rbind, sf)

  # Add cluster label for map
  sf |>
    mutate(lbl = paste("Cluster", cluster))
}

# SHINY UI ----------------------------------------------------------------

select_input_syndrome <- function(input_id, ls) {
  selectInput(
    inputId = input_id,
    label = "Syndrome",
    choices = ls,
    multiple = FALSE,
    selected = ls[[1]]
  )
}

date_input_analysis <- function(input_id, dates) {
  dateInput(
    inputId = input_id,
    label = "Analysis date",
    value = max(dates),
    min = min(dates),
    max = max(dates)
  )
}

radio_buttons_daterange <- function(input_id, ls) {
  radioButtons(
    inputId = input_id,
    label = "Date range",
    choices = ls,
    selected = ls[[2]]
  )
}

syndrome_title_tag <- function(x, ls) {
  tags$h3(names(ls)[which(ls == x)], class = "cluster-tab-title")
}

card_dc <- function(...) {
  card(
    ...,
    min_height = "300px",
    max_height = "400px"
  )
}

# PLOTS -------------------------------------------------------------------

# Time series plot
ts_plot <- function(ls, title) {
  highchart() |>
    hc_add_series_list(ls) |>
    hc_xAxis(
      type = "datetime",
      title = list(text = "Date"),
      labels = list(format = "{value:%b %d}")
    ) |>
    hc_yAxis(
      title = list(text = "Count")
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(formatter = JS(
      "function() {
        const dt = new Date(this.x);
        return dt.toDateString() + '<br>' +
        `Count: <b>${this.y}</b>` + '<br>' +
        `Alert status: <b>${this.point.alert_status}</b>`;
      }"
    )) |>
    hc_title(text = title)
}

# Convert color names and hex colors to RGBA string for CSS
color2rgba <- function(color, alpha = 1) {
  if (!grepl("^#", color)) {
    x <- col2rgb(color)
    args <- as.list(x)
    names(args) <- rownames(x)
    args <- append(args, list(maxColorValue = 255))
    color <- do.call(rgb, args)
  }

  rgba <- col2rgb(color) |>
    as.numeric() |>
    paste(collapse = ",") |>
    paste(alpha, sep = ",")

  paste0("rgba(", rgba, ")")
}

# Create a custom leaflet legend to add to the `html` arg in `addControl()`
custom_legend_row <- function(ls) {
  if ("class" %in% names(ls)) {
    icon <- paste0(
      "    <div class = '", ls$class, "'>",
      "</div>\n"
    )
  } else {
    icon <- paste0(
      "    <div style = '",
      "background: ", color2rgba(ls$fill, ls$opac2), "; ",
      "width:20px; height:20px; ",
      "border: ", ls$wt, "px solid ", color2rgba(ls$clr, ls$opac1), "; ",
      "border-radius: ", switch(ls$shp, square = "0%;", circle = "50%;"), "'>",
      "</div>\n"
    )
  }

  label <- paste0(
    "    <div style = 'padding-left: 5px;'>",
    ls$name,
    "</div>\n"
  )

  paste0(
    "  <div style = 'display: flex; align-items: center; margin: 1px 0;'>\n",
    icon,
    label,
    "  </div>\n"
  )
}

custom_legend_combine <- function(ls) {
  paste0(
    "<div style = 'line-height: 0px;'>\n",
    paste(ls, collapse = "  <br>\n"),
    "</div>"
  )
}

# Leaflet map showing study area and syndrome clusters using Satscan output
cluster_map <- function(
  cluster_locations,
  cluster_points = NULL,
  location_boundaries,
  kc_boundary = geo$city,
  hospital_locations = NULL,
  gp,
  zoom_level
) {
  # Map center point
  center <- st_coordinates(st_centroid(st_union(location_boundaries))) |>
    as.data.frame()

  legend_rows <- lapply(gp, custom_legend_row)

  map <- leaflet(
    options = leafletOptions(scrollWheelZoom = FALSE)
  ) |>
    setView(lng = center$X, lat = center$Y, zoom = zoom_level) |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      data = location_boundaries,
      weight = gp$study$wt,
      color = gp$study$clr,
      opacity = gp$study$opac1,
      fillColor = gp$study$fill,
      fillOpacity = gp$study$opac2
    ) |>
    addPolygons(
      data = kc_boundary,
      weight = gp$kc$wt,
      color = gp$kc$clr,
      opacity = gp$kc$opac1,
      fillColor = gp$kc$fill,
      fillOpacity = gp$kc$opac2
    )

  # Add cluster regions
  if (!is.null(cluster_locations)) {
    map <- map |>
      addPolygons(
        data = cluster_locations,
        layerId = ~cluster,
        weight = gp$clust$wt,
        color = gp$clust$clr,
        opacity = gp$clust$opac1,
        fillColor = gp$clust$fill,
        fillOpacity = gp$clust$opac2,
        label = ~lbl,
        highlightOptions = highlightOptions(
          opacity = 1,
          fillOpacity = .5
        )
      )
  } else {
    legend_rows <- legend_rows[-which(names(legend_rows) == "clust")]
  }

  # Add hospital locations
  if (!is.null(hospital_locations)) {
    hospicon <- makeIcon(
      iconUrl = "www/img/transparent-square.svg",
      iconWidth = 12,
      iconHeight = 12,
      className = "plus"
    )

    map <- map |>
      addMarkers(
        data = hospital_locations,
        icon = hospicon,
        label = ~hospital_name
      )
  }

  # Add cluster points
  if (!is.null(cluster_points)) {
    map <- map |>
      addCircleMarkers(
        data = cluster_points,
        radius = 10,
        layerId = ~cluster,
        weight = gp$clust$wt,
        color = gp$clust$clr,
        opacity = gp$clust$opac1,
        fillColor = gp$clust$fill,
        fillOpacity = gp$clust$opac2,
        label = ~lbl
      )
  }

  # Add legend
  legend_html <- custom_legend_combine(legend_rows)

  map |>
    addControl(
      html = legend_html,
      position = "bottomright"
    )
}

# TABLES ------------------------------------------------------------------

# Modify column labels
mod_col_labels <- function(x) {
  str_to_sentence(gsub("_", " ", x))
}

dd_table <- function(df, var, replace_nm = NULL) {
  df <- df |>
    count(.data[[var]])

  if (!is.null(replace_nm)) {
    df <- df |>
      rename(any_of(setNames(var, replace_nm)))
  }

  df |>
    rename_with(mod_col_labels) |>
    reactable(
      columns = list(
        "N" = colDef(format = colFormat(separators = TRUE))
      ),
      sortable = FALSE,
      pagination = FALSE
    )
}

# Table showing the number of clusters detected for each syndrome
clustcount_table <- function(df) {
  bold_text <- function(x) {
    if (!is.na(x) && x > 0) {
      list(fontWeight = "bold")
    }
  }

  pink_bg <- function(r) {
    x <- df[r, "clust_pat"]
    y <- df[r, "clust_hosp"]

    if ((!is.na(x) && x > 0) | (!is.na(y) && y > 0)) {
      list(background = "#fcc7c7")
    }
  }

  df |>
    reactable(
      columns = list(
        syndrome = colDef(name = "Syndrome"),
        clust_pat = colDef(
          name = "ER visits by patient location",
          style = bold_text,
          na = "-"
        ),
        clust_hosp = colDef(
          name = "ER visits by hospital location",
          style = bold_text,
          na = "-"
        )
      ),
      columnGroups = list(
        colGroup(
          name = "Number of clusters",
          columns = c("clust_pat", "clust_hosp")
        )
      ),
      rowStyle = pink_bg,
      sortable = FALSE,
      pagination = FALSE
    )
}

# Table with cluster data
cluster_table <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }

  replace <- c(
    "Locations" = "Number loc",
    "Test statistic" = "Test stat",
    "P-value" = "P value",
    "RI (days)" = "Recurr int",
    "Obs/exp" = "Ode"
  )

  df <- df |>
    st_drop_geometry() |>
    select(
      cluster, start_date, end_date, number_loc, test_stat, p_value,
      recurr_int, observed, expected, ode
    ) |>
    mutate(
      across(
        c(start_date, end_date),
        ~ format(as.Date(.x, "%Y/%m/%d"), "%b %d, %Y")
      ),
      across(
        c(test_stat, ode),
        ~ round_ties_away(.x, 2)
      ),
      p_value = signif(p_value, 1),
      expected = round_ties_away(expected, 0)
    ) |>
    rename_with(mod_col_labels) |>
    rename(any_of(replace))

  col_defs <- lapply(colnames(df), \(x) {
    if (x %in% c("P-value", "RI (days)")) {
      # Format as scientific notation
      colDef(cell = JS(
        "function(cellInfo) {
            return cellInfo.value.toExponential(1)
        }"
      ))
    } else if (is.numeric(df[[x]])) {
      # Use comma separators
      colDef(format = colFormat(separators = TRUE))
    }
  })

  names(col_defs) <- colnames(df)

  df |>
    reactable(
      columns = compact(col_defs),
      sortable = FALSE,
      pagination = FALSE,
      selection = "single",
      onClick = "select"
    )
}

# Table with location data for a given cluster
location_table <- function(df, id = NULL, type = c("patient", "hospital")) {
  if (is.null(id)) {
    return(NULL)
  }

  type <- match.arg(type)

  replace <- c(
    "Cluster" = "cluster",
    "Observed" = "loc_obs",
    "Expected" = "loc_exp",
    "Obs/exp" = "loc_ode"
  )

  if (type == "patient") {
    df <- df |>
      mutate(loc_id = as.numeric(as.character(loc_id)))

    replace <- c(
      "ZCTA" = "loc_id",
      replace
    )
  } else if (type == "hospital") {
    df <- df |>
      mutate(
        loc_id = gsub("_", " ", loc_id) |>
          str_to_title() |>
          sub(pattern = "\\sOf\\s", replacement = " of ")
      )

    replace <- c(
      "Hospital" = "loc_id",
      replace
    )
  }

  df |>
    st_drop_geometry() |>
    filter(cluster == id) |>
    select(loc_id, cluster, loc_obs, loc_exp, loc_ode) |>
    mutate(
      loc_exp = round_ties_away(loc_exp, 0),
      loc_ode = round_ties_away(loc_ode, 2)
    ) |>
    arrange(loc_id) |>
    rename(any_of(replace)) |>
    reactable(
      sortable = FALSE,
      pagination = FALSE
    )
}

