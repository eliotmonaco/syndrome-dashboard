# ESSENCE DATA ------------------------------------------------------------

# Build URL for Essence API
build_ess_url <- function(
    syndrome, # list of syndromes built as in `syndromes.R`
    start, # start date (YYYY-MM-DD)
    end = Sys.Date(), # end date (YYYY-MM-DD)
    data_source = c("hospital", "patient"), # data by hospital or patient
                                            # location
    output = c("dd", "tb", "ts"), # dd = data details, tb = table builder,
                                  # ts = time series
    dd_fields = NULL # if output is "dd", add desired fields here as character
                     # vector (e.g., "Date" or "ZipCode")
    # tb_col = NULL, # table builder column (e.g., "timeResolution")
    # tb_rows = NULL, # table builder rows (e.g., "geographyzipcode")
    # zipcodes = FALSE # use zip code free text as geography with zips from kcData
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
    } else {
      dd_fields <- URLencode(dd_fields)

      dd_fields <- paste0("field=", c(dd_fields, "EssenceID"))

      params_out <- paste(
        c("aqtTarget=DataDetails", dd_fields),
        collapse = "&"
      )
    }

    op <- "dataDetails/csv?"

  # } else if (output == "tb") {
  #   op <- "tableBuilder/csv?"
  #
  #   if (length("tb_col") > 1) {
  #     stop("`tb_col` must have length of 1")
  #   }
  #
  #   if (is.null(tb_col) || is.null(tb_rows)) {
  #     stop("When `output` is \"tb\", `tb_col` and `tb_rows` cannot be empty")
  #   }
  #
  #   params_out <- paste(
  #     "aqtTarget=TableBuilder",
  #     paste0("columnField=", tb_col),
  #     paste(paste0("rowFields=", tb_rows), collapse = "&"),
  #     sep = "&"
  #   )
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

  # # ZIP code free text
  # if (zipcodes) {
  #   params_geo <- paste(
  #     "geographySystem=zipcode",
  #     paste0(
  #       "geography=",
  #       paste(kcData::geoids$zcta$ids2024, collapse = ",")
  #     ),
  #     sep = "&"
  #   )
  # }

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

config_casefile <- function(df, zctas) {
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

get_significant_clusters <- function(ls, sig_pval = FALSE) {
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

filter_cluster_zctas <- function(ls, zctas_full = zctas$city_full) {
  lapply(ls, \(ls2) {
    clust <- ls2$shapeclust # contains clusters
    loc <- ls2$gis # contains zip codes within each cluster

    if (!is.data.frame(clust) || nrow(clust) == 0) {
      return(NULL)
    }

    sf <- lapply(clust$cluster, \(x) {
      sfc <- zctas_full |>
        filter(ZCTA5CE20 %in% loc$loc_id[loc$cluster == x]) |>
        st_combine()

      st_set_geometry(data.frame(cluster = x), sfc)
    })

    sf <- do.call(rbind, sf)

    # Add cluster label for map
    sf |>
      mutate(lbl = paste("Cluster", cluster))
  })
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
  cluster_zctas,
  zctas_full = zctas$city_full,
  zctas_clipped = zctas$city_clipped,
  hospital_locations = NULL
) {
  # Map center point
  center <- as.data.frame(st_coordinates(st_centroid(st_union(zctas_clipped))))

  # Graphical parameters for shapes and markers
  gp <- list(
    zcta1 = list(
      name = "ZCTA area outside KC",
      clr = "#aaa",
      fill = "#aaa",
      wt = 2,
      opac1 = 1,
      opac2 = .1,
      shp = "square"
    ),
    zcta2 = list(
      name = "ZCTA area within KC",
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
      data = zctas_full,
      weight = gp$zcta1$wt,
      color = gp$zcta1$clr,
      opacity = gp$zcta1$opac1,
      fillColor = gp$zcta1$fill,
      fillOpacity = gp$zcta1$opac2
    ) |>
    addPolygons(
      data = zctas_clipped,
      weight = gp$zcta2$wt,
      color = gp$zcta2$clr,
      opacity = gp$zcta2$opac1,
      fillColor = gp$zcta2$fill,
      fillOpacity = gp$zcta2$opac2
    )

  # Add ZCTA cluster regions
  if (!is.null(cluster_zctas)) {
    map <- map |>
      addPolygons(
        data = cluster_zctas,
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
location_table <- function(df, id = NULL) {
  if (is.null(id)) {
    return(NULL)
  }

  df |>
    st_drop_geometry() |>
    filter(cluster == id) |>
    select(loc_id, cluster, loc_obs, loc_exp, loc_ode) |>
    mutate(
      loc_id = as.numeric(as.character(loc_id)),
      loc_exp = round_ties_away(loc_exp, 0),
      loc_ode = round_ties_away(loc_ode, 2)
    ) |>
    arrange(loc_id) |>
    rename(
      "ZIP code" = loc_id,
      "Cluster" = cluster,
      "Observed" = loc_obs,
      "Expected" = loc_exp,
      "Obs/exp" = loc_ode
    ) |>
    reactable(
      sortable = FALSE,
      pagination = FALSE
    )
}

