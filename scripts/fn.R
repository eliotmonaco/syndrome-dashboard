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
    error = function(e) {
      output <<- append(output, list(message = conditionMessage(e)))
    },
    message = function(m) {
      output <<- append(output, list(message = conditionMessage(m)))
    }
  )

  output <- append(output, list(data = expr), after = 0)

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
config_dd <- function(df) {
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

    apply(dupeset, 2, \(c) { # find the number of unique values for each variable
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

  list(
    data = select(df, -row_id),
    error_rate = errors
  )
}

config_ts <- function(df) {
  # Add alert status, color, symbol, and radius
  lvl <- c("Normal", "Warning", "Anomaly")
  pal <- c("#0703fc", "#f2c00a", "#ff0000")
  shp <- c("circle", "diamond", "triangle")
  rad <- c(4, 5, 4)

  df <- df |>
    mutate(
      alert_status = case_when(
        color_id == 0 ~ lvl[1],
        color_id == 1 ~ lvl[1],
        color_id == 2 ~ lvl[2],
        color_id == 3 ~ lvl[3]
      ),
      alert_status = factor(alert_status, levels = lvl),
      alert_color = pal[alert_status],
      alert_symbol = shp[alert_status],
      alert_radius = rad[alert_status]
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

# SHINY DATA CONFIG -------------------------------------------------------

filter_ts <- function(df, d1, d2 = NULL) {
  if (is.null(d2)) {
    d2 <- max(df$date)
  }

  df |>
    filter(
      date >= d1,
      date <= d2
    )
}

df_to_hc_list <- function(df) {
  list(
    list(
      data = lapply(1:nrow(df), \(r) {
        list(
          x = datetime_to_timestamp(df[r, "date"]),
          y = df[r, "count"],
          color = df[r, "alert_color"],
          marker = list(
            symbol = df[r, "alert_symbol"],
            radius = df[r, "alert_radius"]
          ),
          alert_status = df[r, "alert_status"]
        )
      })
    )
  )
}

get_satscan_results <- function(ls, date) {
  ls[[names(ls)[grepl(gsub("-", "", date), names(ls))]]]
}

significant_clusters_by_syndrome <- function(ls) {
  ls <- lapply(ls, \(ls2) {
    x <- sapply(ls2, \(ls3) {
      if (is.data.frame(ls3$shapeclust)) {
        ls3$shapeclust |>
          st_drop_geometry() |>
          filter(p_value < .05) |>
          nrow()
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
        syndrome = names(syn_names),
        abbr = unlist(syn_names)
      ),
      by = "abbr"
    ) |>
    select(syndrome, clust_pat, clust_hosp)

  df
}

get_significant_clusters <- function(ls, sig_pval = FALSE) {
  if (!is.data.frame(ls$shapeclust) || nrow(ls$shapeclust) == 0) {
    return(ls)
  }

  if (sig_pval) {
    plvl <- .05
  } else {
    plvl <- 1.1
  }

  ls$shapeclust <- ls$shapeclust |>
    filter(p_value < plvl)

  if (nrow(ls$shapeclust) == 0) {
    ls$shapeclust <- NULL
  }

  ls$gis <- ls$gis |>
    filter(p_value < plvl)

  if (nrow(ls$gis) == 0) {
    ls$gis <- NULL
  }

  ls
}

# Filter cluster and location data by p-value for a syndrome
filter_cluster_data <- function(ls, syndrome, sig_pval = TRUE) {
  lapply(
    list(
      patient = ls$patient[[syndrome]],
      hospital = ls$hospital[[syndrome]]
    ),
    get_significant_clusters,
    sig_pval = sig_pval
  )
}

# Filter map regions by cluster
filter_cluster_regions <- function(ls, geo, var) {
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

syndrome_title_tag <- function(x, ls = syn_names) {
  tags$h3(names(ls)[which(ls == x)], class = "cluster-tab-title")
}

# PLOTTING/VIZ ------------------------------------------------------------

# Time series plot
ts_plot <- function(ls, title) {
  fn <- JS(
    "function() {
        const dt = new Date(this.x);
        return dt.toDateString() + '<br>' +
        `Count: <b>${this.y}</b>` + '<br>' +
        `Alert status: <b>${this.point.alert_status}</b>`;
      }"
  )

  highchart() |>
    hc_add_series_list(ls) |>
    hc_xAxis(
      type = "datetime",
      title = list(
        text = "Date"#,
        # style = list(fontSize = "1.2em !important")
      ),
      labels = list(
        format = "{value:%b %d}"#,
        # style = list(fontSize = "1.2em !important")
      )
    ) |>
    hc_yAxis(
      title = list(
        text = "Count"#,
        # style = list(fontSize = "1.2em !important")
      )#,
      # labels = list(
      #   style = list(fontSize = "1.2em !important")
      # )
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(formatter = fn) |>
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

# Leaflet map showing KC ZCTAs and syndrome clusters using Satscan output
cluster_map <- function(
  clusters,
  cluster_regions,
  zcta_boundaries = geo$zctas,
  kc_boundary = geo$city,
  hospital_locations = NULL
) {
  # Map center point
  center <- as.data.frame(st_coordinates(st_centroid(st_union(kc_boundary))))

  # Graphical parameters for shapes and markers
  gp <- list(
    zcta = list(
      name = "ZCTA boundaries",
      clr = "#aaa",
      fill = "#aaa",
      wt = 2,
      opac1 = 1,
      opac2 = .1,
      shp = "square"
    ),
    kc = list(
      name = "KC boundary",
      clr = "#024cbf",
      fill = "#024cbf",
      wt = 2,
      opac1 = 1,
      opac2 = .1,
      shp = "square"
    ),
    clust = list(
      name = "Cluster region",
      clr = "red",
      fill = "red",
      wt = 2,
      opac1 = .5,
      opac2 = .2,
      shp = "square"
    ),
    hosp = list(
      name = "Hospital",
      class = "plus"
    )
  )

  legend_rows <- lapply(gp, custom_legend_row)

  map <- leaflet(
    options = leafletOptions(scrollWheelZoom = FALSE)
  ) |>
    setView(lng = center$X, lat = center$Y, zoom = 9) |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      data = zcta_boundaries,
      weight = gp$zcta$wt,
      color = gp$zcta$clr,
      opacity = gp$zcta$opac1,
      fillColor = gp$zcta$fill,
      fillOpacity = gp$zcta$opac2
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
  if (!is.null(cluster_regions)) {
    map <- map |>
      addPolygons(
        data = cluster_regions,
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
      iconWidth = 20,
      iconHeight = 20,
      className = "plus"
    )

    map <- map |>
      addMarkers(
        data = hospital_locations,
        icon = hospicon,
        label = ~hospital_name
      )
  } else {
    legend_rows <- legend_rows[-which(names(legend_rows) == "hosp")]
  }

  # Add legend
  legend_html <- custom_legend_combine(legend_rows)

  map |>
    addControl(
      html = legend_html,
      position = "bottomright"
    )
}

# Modify column labels
mod_col_labels <- function(x) {
  str_to_sentence(gsub("_", " ", x))
}

# Table showing the number of clusters detected for each syndrome
clustcount_table <- function(df) {
  bold_text <- function(x) {
    if (x > 0) {
      list(fontWeight = "bold")
    }
  }

  df |>
    reactable(
      columns = list(
        syndrome = colDef(name = "Syndrome"),
        clust_pat = colDef(
          name = "ER visits by patient location",
          style = bold_text
        ),
        clust_hosp = colDef(
          name = "ER visits by hospital location",
          style = bold_text
        )
      ),
      columnGroups = list(
        colGroup(
          name = "Number of clusters",
          columns = c("clust_pat", "clust_hosp")
        )
      ),
      rowStyle = \(r) {
        if (df[r, "clust_pat"] > 0 | df[r, "clust_hosp"] > 0) {
          list(background = "pink")
        }
      },
      sortable = FALSE,
      pagination = FALSE
    )
}

# Table with cluster data
cluster_table <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }

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
    rename(
      "locations" = number_loc,
      "test statistic" = test_stat,
      "p-value" = p_value,
      "recurrence interval (days)" = recurr_int,
      "obs/exp" = ode
    ) |>
    rename_with(mod_col_labels)

  col_defs <- lapply(colnames(df), \(x) {
    if (is.numeric(df[[x]])) {
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

  if (type == "patient") {
    df <- df |>
      mutate(loc_id = as.numeric(as.character(loc_id)))

    replace <- c(
      "ZCTA" = "loc_id",
      "Cluster" = "cluster",
      "Observed" = "loc_obs",
      "Expected" = "loc_exp",
      "Obs/exp" = "loc_ode"
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
      "Cluster" = "cluster",
      "Observed" = "loc_obs",
      "Expected" = "loc_exp",
      "Obs/exp" = "loc_ode"
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

