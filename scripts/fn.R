
# ESSENCE -----------------------------------------------------------------

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

# SATSCAN -----------------------------------------------------------------

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

significant_clusters_by_syndrome <- function(ls) {
  ls <- lapply(ssresults, \(ls) {
    x <- sapply(ls, \(ls2) {
      if (is.data.frame(ls2$shapeclust)) {
        ls2$shapeclust |>
          st_drop_geometry() |>
          filter(p_value < .05) |>
          nrow()
      } else if (is.na(ls2$shapeclust)) {
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

# SHINY CONFIG ------------------------------------------------------------

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
  if (!is.data.frame(ls$shapeclust) || nrow(ls$shapeclust) == 0) {
    return(ls)
  }

  if (sig_pval) {
    plvl <- .05
  } else {
    plvl <- 1
  }

  ls$shapeclust <- ls$shapeclust |>
    filter(p_value < plvl)

  ls$gis <- ls$gis |>
    filter(p_value < plvl) |>
    st_as_sf(
      coords = c("loc_long", "loc_lat"),
      crs = "WGS84"
    )

  ls
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

# Convert map shape style attributes to style string for `addLegend()`
legend_icon <- function(ls) {
  paste(
    color2rgba(ls$fill, ls$op2),
    "width:20px;height:20px",
    paste0("border:", ls$wt, "px solid ", color2rgba(ls$clr, ls$op1)),
    paste0("border-radius:", switch(ls$shp, square = "0%", circle = "50%")),
    sep = ";"
  )
}

# Convert legend labels to div string for `addLegend()`
legend_label <- function(x) {
  style <- "display:inline-block;height:20px;margin-top:4px;line-height:20px;"

  paste0("<div style='", style, "'>", x, "</div>")
}

# Leaflet map showing KC ZCTAs and syndrome clusters using Satscan output
cluster_map <- function(
  clusters,
  locations,
  zctas_full = kczctafull,
  zctas = kczcta
) {
  # Map center point
  center <- as.data.frame(st_coordinates(st_centroid(st_union(zctas))))

  # Shape style attributes
  pal <- list(
    a = list(
      clr = "#aaa",
      fill = "#aaa",
      wt = 2,
      op1 = 1,
      op2 = .1,
      shp = "square"
    ),
    b = list(
      clr = "#024cbf",
      fill = "#024cbf",
      wt = 2,
      op1 = 1,
      op2 = .1,
      shp = "square"
    ),
    c = list(
      clr = "red",
      fill = "red",
      wt = 2,
      op1 = .5,
      op2 = .2,
      shp = "square"
    )#,
    # d = list(
    #   clr = "red",
    #   fill = "red",
    #   wt = 3,
    #   op1 = .5,
    #   op2 = 0,
    #   shp = "circle"
    # )
  )

  legend_colors <- sapply(pal, legend_icon)

  legend_labels <- legend_label(c(
    "ZCTA regions outside KC",
    "ZCTA regions within KC",
    "ZCTAs in cluster"#,
    # "Cluster circles"
  ))

  map <- leaflet(
    options = leafletOptions(scrollWheelZoom = FALSE)
  ) |>
    setView(lng = center$X, lat = center$Y, zoom = 9) |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      data = zctas_full,
      weight = pal$a$wt,
      color = pal$a$clr,
      opacity = pal$a$op1,
      fillColor = pal$a$fill,
      fillOpacity = pal$a$op2
    ) |>
    addPolygons(
      data = zctas,
      weight = pal$b$wt,
      color = pal$b$clr,
      opacity = pal$b$op1,
      fillColor = pal$b$fill,
      fillOpacity = pal$b$op2
    ) #|>
    # addCircleMarkers(
    #   data = locations,
    #   radius = 2,
    #   stroke = FALSE,
    #   fillColor = "black",
    #   fillOpacity = 1
    # )

  # Add ZCTA cluster regions and circles
  if (is.data.frame(clusters) && length(clusters$cluster) > 0) {
    clust_zcta <- lapply(clusters$cluster, \(x) {
      zctas_full |>
        filter(ZCTA5CE20 %in% locations$loc_id[locations$cluster == x]) |>
        st_combine() |>
        st_as_sf()
    }) |>
      list_rbind() |>
      bind_cols(st_drop_geometry(clusters[, "cluster"])) |>
      st_as_sf()

    map <- map |>
      addPolygons(
        data = clust_zcta,
        layerId = ~cluster,
        weight = pal$c$wt,
        color = pal$c$clr,
        opacity = pal$c$op1,
        fillColor = pal$c$fill,
        fillOpacity = pal$c$op2,
        highlightOptions = highlightOptions(
          opacity = 1
        )
      ) #|>
      # addPolygons(
      #   data = clusters,
      #   layerId = ~cluster,
      #   weight = pal$d$wt,
      #   color = pal$d$clr,
      #   opacity = pal$d$op1,
      #   fillColor = pal$d$fill,
      #   fillOpacity = pal$d$op2,
      #   highlightOptions = highlightOptions(
      #     opacity = 1
      #   )
      # )
  } else {
    legend_colors <- legend_colors[1:2]
    legend_labels <- legend_labels[1:2]
  }

  map |>
    addLegend(
      position = "bottomright",
      colors = legend_colors,
      labels = legend_labels,
      opacity = 1
    )
}

# Modify column labels
mod_col_labels <- function(x) {
  str_to_sentence(gsub("_", " ", x))
}

# Table showing the number of clusters detected for each syndrome
clustcount_table <- function(df) {
  df |>
    gt() |>
    tab_style(
      style = cell_fill(color = "pink"),
      locations = cells_body(rows = clust_pat > 0 | clust_hosp > 0)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = clust_pat, rows = clust_pat > 0)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = clust_hosp, rows = clust_hosp > 0)
    ) |>
    cols_label(
      syndrome = "Syndrome",
      clust_pat = "Clusters by patient residence",
      clust_hosp = "Clusters by hospital location"
    ) |>
    tab_options(
      data_row.padding = 2
    )
}

# Table with cluster data
cluster_table <- function(df, id = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }

  tbl <- df |>
    st_drop_geometry() |>
    select(
      cluster, start_date, end_date, number_loc, test_stat, p_value,
      recurr_int, observed, expected, ode
    ) |>
    gt() |>
    fmt_date(
      columns = c("start_date", "end_date"),
      date_style = "m_day_year"
    ) |>
    fmt_number(where(is.numeric), decimals = 0) |>
    fmt_number(test_stat, decimals = 2) |>
    fmt_number(p_value, n_sigfig = 1) |>
    fmt_number(ode, decimals = 2)


  # Highlight row when shape is selected in Leaflet map
  if (!is.null(id)) {
    tbl <- tbl |>
      tab_style(
        style = cell_fill(color = "pink"),
        locations = cells_body(rows = cluster == id)
      )
  }

  tbl |>
    cols_label_with(fn = mod_col_labels) |>
    cols_label(
      number_loc = "Locations",
      test_stat = "Test statistic",
      p_value = "P-value",
      recurr_int = "Recurrence interval",
      ode = "Obs/Exp"
    ) |>
    tab_options(
      data_row.padding = 2
    )
}

# Table with location data for a given cluster
location_table <- function(df, id = NULL) {
  if (is.null(id)) {
    return(NULL)
  }

  df |>
    st_drop_geometry() |>
    filter(cluster == id) |>
    mutate(loc_id = as.numeric(as.character(loc_id))) |>
    arrange(loc_id) |>
    select(loc_id, cluster, starts_with("loc_")) |>
    gt() |>
    fmt_number(where(is.numeric) & !loc_id, decimals = 0) |>
    fmt_number(loc_ode, decimals = 2) |>
    cols_label_with(fn = mod_col_labels) |>
    cols_label(
      loc_id = "ZIP code",
      loc_obs = "Observed",
      loc_exp = "Expected",
      loc_ode = "Obs/Exp"
    ) |>
    tab_options(
      data_row.padding = 2
    )
}

