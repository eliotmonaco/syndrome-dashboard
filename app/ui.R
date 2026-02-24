page_navbar(
  title = "Syndrome tracker and cluster detection",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#8fccbf"),

  sidebar = sidebar(
    width = 300,
    bg = "#ebf5f3",
    selectInput(
      inputId = "syn",
      label = "Syndrome",
      choices = syn_names,
      multiple = FALSE,
      selected = syn_names[[1]][1]
    ),
    dateRangeInput(
      inputId = "date",
      label = "Date range",
      start = max(ts0$date) - 7,
      end = max(ts0$date),
      min = min(ts0$date),
      max = max(ts0$date)
    ),
    # checkboxInput(
    #   inputId = "alert",
    #   label = "Show ESSENCE alerts",
    #   value = TRUE
    # )
  ),

  nav_panel(
    "Time series",
    layout_columns(
      card(
        highchartOutput("ts")
      )
    )
  ),

  nav_panel(
    "SaTScan cluster detection",
    layout_columns(
      card(

      )
    )
  )

)
