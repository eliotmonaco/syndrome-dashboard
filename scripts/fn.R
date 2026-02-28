
# Essence -----------------------------------------------------------------

# Build URL for Essence API
build_ess_url <- function(
    syndrome, # list of syndromes built as in `syndromes.R`
    start, # start date (YYYY-MM-DD)
    end = Sys.Date(), # end date (YYYY-MM-DD)
    data_source = c("hospital", "patient"), # data by hospital or patient
                                            # location
    output = c("dd", "tb", "ts"), # dd = data details, tb = table builder,
                                  # ts = time series
    dd_fields = NULL, # if output is "dd", add desired fields here as character
                      # vector (e.g., "Date" or "ZipCode")
    tb_col = NULL, # table builder column (e.g., "timeResolution")
    tb_rows = NULL, # table builder rows (e.g., "geographyzipcode")
    zipcodes = FALSE # use zip code free text as geography with zips from kcData
) {
  start <- as.Date(start); end <- as.Date(end)

  if (is.na(start) | is.na(end)) {
    stop("`start` and `end` must be valid dates formatted YYYY-MM-DD")
  }

  data_source <- match.arg(data_source)

  output <- match.arg(output)

  # Output parameters
  if (output == "dd") {
    if (is.null(dd_fields)) {
      params_out <- "aqtTarget=DataDetails"

      # stop("When `output` is \"dd\", `dd_fields` cannot be empty")
    } else {
      dd_fields <- URLencode(dd_fields)

      dd_fields <- paste0("field=", c(dd_fields, "EssenceID"))

      params_out <- paste(
        c("aqtTarget=DataDetails", dd_fields),
        collapse = "&"
      )
    }

    op <- "dataDetails/csv?"

  } else if (output == "tb") {
    op <- "tableBuilder/csv?"

    if (length("tb_col") > 1) {
      stop("`tb_col` must have length of 1")
    }

    if (is.null(tb_col) || is.null(tb_rows)) {
      stop("When `output` is \"tb\", `tb_col` and `tb_rows` cannot be empty")
    }

    params_out <- paste(
      "aqtTarget=TableBuilder",
      paste0("columnField=", tb_col),
      paste(paste0("rowFields=", tb_rows), collapse = "&"),
      sep = "&"
    )
  } else if (output == "ts") {
    op <- "timeSeries?"

    params_out <- "aqtTarget=TimeSeries"
  }

  # Data source & geography parameters
  if (data_source == "hospital") {
    params_ds <- "datasource=va_hosp"

    params_geo <- paste(
      "geographySystem=hospital",
      "geography=mochildrensmercyercc",
      "geography=mochildrensmercynorthlandercc",
      "geography=moresearchkcercc",
      "geography=mostlukeskcercc",
      "geography=mostlukesnorthlandkcercc",
      "geography=motrumanhospitalhillercc",
      "geography=motrumanlakewoodercc",
      "geography=mostjosephkcercc",
      sep = "&"
    )
  } else if (data_source == "patient") {
    params_ds <- "datasource=va_er"

    params_geo <- paste(
      "geographySystem=region",
      "geography=mo_clay",
      "geography=mo_jackson",
      "geography=mo_platte",
      sep = "&"
    )
  }

  # ZIP code free text
  if (zipcodes) {
    params_geo <- paste(
      "geographySystem=zipcode",
      paste0(
        "geography=",
        paste(kcData::geoids$zcta$ids2024, collapse = ",")
      ),
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
get_ess_dd <- function(url, fix_colnames = TRUE) {
  Rnssp::get_api_data(
    url,
    fromCSV = TRUE,
    col_types = readr::cols(.default = "c"),
    name_repair = ifelse(fix_colnames, setmeup::fix_colnames, "unique")
  )
}

# Wrapper for `Rnssp::get_api_data()` to pull time series
get_ess_ts <- function(url) {
  ls <- Rnssp::get_api_data(url)

  df <- ls$timeSeriesData

  colnames(df) <- setmeup::fix_colnames(colnames(df))

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
  # Assign row ID and convert `date` to date
  df <- df |>
    mutate(
      row_id = row_number(), .before = 1,
      date = as.Date(date, "%m/%d/%Y")
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

  # Remove dupes
  df <- df |>
    anti_join(dupes, by = "row_id")

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

# Satscan -----------------------------------------------------------------

config_casefile <- function(df) {
  zctas <- sort(unique(unlist(kcData::geoids$zcta)))

  df |>
    filter(
      zip_code %in% zctas, # zip codes in KC only
      date < max(df$date) # most recent date with complete data
    ) |>
    count(zip_code, date) |>
    select(zip_code, n, date)
}

# Shiny config ------------------------------------------------------------

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

config_ss_output <- function(ls, sig_pval = FALSE) {
  if (sig_pval) {
    plvl <- .05
  } else {
    plvl <- 1
  }

  ls$shapeclust <- ls$shapeclust |>
    filter(P_VALUE < plvl)

  ls$gis <- ls$gis |>
    filter(P_VALUE < plvl) |>
    st_as_sf(
      coords = c("LOC_LONG", "LOC_LAT"),
      crs = "WGS84"
    )

  ls
}

# Plotting/viz ------------------------------------------------------------

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

cluster_map <- function(clusters, locations, zctas = kcmap) {
  center <- as.data.frame(st_coordinates(st_centroid(st_union(zctas))))

  leaflet(zctas) |>
    setView(lng = center$X, lat = center$Y, zoom = 9) |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      weight = 2,
      # popup = ~top5,
      # popupOptions = popupOptions(maxWidth = 500),
      color = "#024cbf",
      opacity = .5,
      fillColor = "#024cbf",
      fillOpacity = .1
    ) |>
    addPolygons(
      data = clusters,
      layerId = ~CLUSTER,
      stroke = TRUE,
      color = "red",
      weight = 3,
      opacity = .5,
      fillColor = "red",
      fillOpacity = .2,
      highlightOptions = highlightOptions(
        opacity = 1
      )
    ) |>
    addCircleMarkers(
      data = locations,
      radius = 2,
      # layerId = ~id,
      stroke = FALSE,
      fillColor = "black",
      fillOpacity = 1
    )
}

cluster_table <- function(df, id = NULL) {
  if (nrow(df) == 0) {
    return(NULL)
  }

  tbl <- df |>
    st_drop_geometry() |>
    select(
      CLUSTER, START_DATE, END_DATE, NUMBER_LOC, TEST_STAT, P_VALUE,
      RECURR_INT, OBSERVED, EXPECTED, ODE
    ) |>
    gt() |>
    fmt_date(
      columns = c("START_DATE", "END_DATE"),
      date_style = "m_day_year"
    )

  # Highlight row when shape is selected in Leaflet map
  if (!is.null(id)) {
    tbl <- tbl |>
      tab_style(
        style = cell_fill(color = "pink"),
        locations = cells_body(rows = CLUSTER == id)
      )
  }

  tbl
}

location_table <- function(df, id = NULL) {
  if (is.null(id)) {
    return(NULL)
  }

  df |>
    st_drop_geometry() |>
    filter(CLUSTER == id) |>
    mutate(LOC_ID = as.numeric(as.character(LOC_ID))) |>
    arrange(LOC_ID) |>
    select(LOC_ID, CLUSTER, starts_with("LOC_")) |>
    gt()
}

