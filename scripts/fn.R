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
      stop("When `output` is \"dd\", `dd_fields` cannot be empty")
    }

    op <- "dataDetails/csv?"

    dd_fields <- paste0("field=", c(dd_fields, "EssenceID"))

    params_out <- paste(
      c("aqtTarget=DataDetails", dd_fields),
      collapse = "&"
    )
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
        paste(sort(unique(unlist(
          kcData::geoids$zcta
        ))), collapse = ",")
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

# Wrapper for `Rnssp::get_api_data()` using Data details
get_ess_dd <- function(url) {
  Rnssp::get_api_data(
    url,
    fromCSV = TRUE,
    col_types = readr::cols(.default = "c"),
    name_repair = setmeup::fix_colnames
  )
}

# Capture both the message/error and the returned value from a function call
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

  output <- append(output, list(data = expr))

  output
}

# Configure data downloaded from Essence
config_ess <- function(ess) {
  # Assign row ID and convert `date` to date
  ess <- ess |>
    mutate(
      row_id = row_number(), .before = 1,
      date = as.Date(date, "%m/%d/%Y")
    )

  # Find "exact" duplicates
  dupes <- suppressMessages(find_dupes(ess, c(
    "date", "time", "age", "sex", "date_of_birth",
    "zip_code", "travel", "hospital_name",
    "visit_number"
  )))

  # Early return if no dupes are found
  if (is.null(dupes)) {
    return(list(
      data = select(ess, -row_id),
      error_rate = NULL
    ))
  }

  # Remove dupes
  ess <- ess |>
    anti_join(dupes, by = "row_id")

  # Deduplicate using `visit_number`
  dupes <- suppressMessages(find_dupes(ess, "visit_number"))

  # Early return if no dupes are found
  if (is.null(dupes)) {
    return(list(
      data = select(ess, -row_id),
      error_rate = NULL
    ))
  }

  # If members of a dupe set have the same `patient_id`, `date`, and `zip_code`,
  # all but one are redundant. Only one from each set will be kept in `ess`.
  ls <- lapply(unique(dupes$dupe_id), \(x) {
    df <- dupes |> # filter a single dupe set
      filter(dupe_id == x)

    if (all(
      length(df$patient_id) == 1,
      length(df$date) == 1,
      length(df$zip_code) == 1
    )) {
      df[2:nrow(df), ] # redundant dupes to remove
    }
  }) |>
    compact()

  # Remove redundant dupes
  if (length(ls) > 0) {
    ess <- ess |> # remove redundant dupes from `ess`
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
    df <- dupes |> # filter a single dupe set
      filter(dupe_id == x)

    apply(df, 2, \(c) { # find the number of unique values for each variable
      length(unique(c))
    }) |>
      as.list() |>
      data.frame() |>
      mutate(
        n_dupes = nrow(df),
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
    mutate(error_rate = n / nrow(ess))

  list(
    data = select(ess, -row_id),
    error_rate = errors
  )
}

syn_ggplot <- function(df, alert) {
  requireNamespace("ggplot2")

  pal <- c("#000000", "#f2c00a", "#ff0000")
  pal <- setNames(pal, c("Normal", "Warning", "Alert"))

  p <- df |>
    ggplot(aes(x = date, y = count, color = syndrome_full)) +
    geom_line(linewidth = 1) +
    guides(color = guide_legend(title = "Syndrome name", order = 1))


  if (alert) {
    p <- p +
      ggnewscale::new_scale_color() +
      # geom_point(aes(color = alert_status), size = 3) +
      ggiraph::geom_point_interactive(
        aes(
          color = alert_status,
          data_id = id,
          tooltip = count
        ),
        size = 3,
        hover_nearest = TRUE
      ) +
      guides(color = guide_legend(title = "Alert status", order = 2)) +
      scale_color_manual(values = pal)
  } else {
    p <- p +
      geom_point(size = 3)
  }

  p +
    labs(x = "\nDate", y = "Count\n") +
    theme_minimal()
}

syn_highchart <- function(df, alert) {
  requireNamespace("highcharter")

  if (alert) {
    p <- df |>
      hchart(
        type = "line",
        hcaes(x = date, y = count, group = syndrome_full, color = alert_color)
      ) |>
      hc_plotOptions(
        line = list(
          marker = list(radius = 4, symbol = "circle")
        )
      )

    fn <- JS(
      "function() {
        const dt = new Date(this.x);
        return dt.toDateString() + '<br>' +
        `Count: <b>${this.y}</b>` + '<br>' +
        `Alert status: <b>${this.point.alert_status}</b>`;
      }"
    )
  } else {
    p <- df |>
      hchart(
        type = "line",
        hcaes(x = date, y = count, group = syndrome_full)
      )

    fn <- JS(
      "function() {
        const dt = new Date(this.x);
        return dt.toDateString() + '<br>' +
        `Count: <b>${this.y}</b>`;
      }"
    )
  }

  p |>
    hc_legend(
      align = "right",
      verticalAlign = "middle",
      layout = "vertical",
      itemStyle = list(fontSize = "1.2em !important")
    ) |>
    hc_tooltip(
      formatter = fn
    ) |>
    hc_xAxis(
      title = list(
        text = "Date",
        style = list(fontSize = "1.2em !important")
      ),
      labels = list(
        format = "{value:%b %d}",
        style = list(fontSize = "1.2em !important")
      )
    ) |>
    hc_yAxis(
      title = list(
        text = "Count",
        style = list(fontSize = "1.2em !important")
      ),
      labels = list(
        style = list(fontSize = "1.2em !important")
      )
    )
}
