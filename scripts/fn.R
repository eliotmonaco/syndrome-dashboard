# Build URLs for Essence API
build_url <- function(
    syndrome, # list of syndromes built as in `syndromes.R`
    start, # start date
    end = Sys.Date(), # end date
    type = c("ts", "dd")) { # "ts" = time series, "dd" = data details
  requireNamespace("Rnssp")

  type <- match.arg(type)

  if (type == "ts") {
    data <- "timeSeries?"

    params1 <- "aqtTarget=TimeSeries"
  } else if (type == "dd") {
    data <- "dataDetails/csv?"

    params1 <- paste(
      "aqtTarget=DataDetails",
      "field=Date",
      "field=Age",
      "field=Sex",
      "field=ZipCode",
      sep = "&"
    )
  }

  ep <- "https://moessence.inductivehealth.com/ih_essence/api/"

  params2 <- paste(
    "datasource=va_hosp",
    "userId=5809",
    "geographySystem=hospital",
    "geography=mochildrensmercyercc",
    "geography=mochildrensmercynorthlandercc",
    "geography=moresearchkcercc",
    "geography=mostlukeskcercc",
    "geography=mostlukesnorthlandkcercc",
    "geography=motrumanhospitalhillercc",
    "geography=motrumanlakewoodercc",
    "geography=mostjosephkcercc",
    "startDate=1Jan2025",
    "endDate=11Sep2025",
    "percentParam=noPercent",
    "detector=probrepswitch",
    "timeResolution=daily",
    sep = "&"
  )

  params <- paste0(params1, "&", params2)

  params <- Rnssp::change_dates(params, start_date = start, end_date = end)

  paste0(ep, data, paste0(params, "&", syndrome))
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
