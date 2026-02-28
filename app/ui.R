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
      selected = syn_names[[1]]
    ),
    radioButtons(
      inputId = "dtrng1",
      label = "Date range",
      choices = date_buttons,
      selected = date_buttons[[2]]
    ),
    checkboxInput(
      inputId = "sigp",
      label = "Include only clusters with significant p-values (< 0.05)",
      value = FALSE
    )
  ),

  nav_panel(
    "Time series",
    page_fillable(
      card(
        card_header("ER visits by patient residence"),
        highchartOutput("tspat"),
        tags$div("This dataset consists of records in which the patient's ZIP code is at least partly within the Kansas City boundary. Therefore, a small number of records will represent non-residents of Kansas City.")
      ),
      card(
        card_header("ER visits by hospital location"),
        highchartOutput("tshosp"),
        tags$div("This dataset consists of records in which the hospital is within the Kansas City boundary. This intentionally captures all individuals, both residents and non-residents, seen at these hospitals.")
      )
    )
  ),

  nav_panel(
    "SaTScan cluster detection",
    navset_tab(
      nav_panel(
        "By patient residence",
        card(leafletOutput("clustermap_pat")),
        card(DTOutput("clustertbl_pat"))
      ),
      nav_panel(
        "By hospital location",
        card(leafletOutput("clustermap_hosp")),
        card(DTOutput("clustertbl_hosp"))
      )
    )
  )

)
