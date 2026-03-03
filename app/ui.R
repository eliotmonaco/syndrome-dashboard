page_navbar(
  title = "Syndrome tracker and cluster detection",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#8fccbf") |>
    bs_add_rules(sass::sass_file("custom.scss")),

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
      label = "Show clusters with significant p-values (< 0.05) only",
      value = TRUE
    )
  ),

  nav_panel(
    "Time series",
    page_fillable(
      card(
        card_header("ER visits by patient residence"),
        highchartOutput("tspat"),
        tags$div(paste(
          "This dataset consists of records in which the patient's ZIP code is",
          "at least partly within the Kansas City boundary."
        ))
      ),
      card(
        card_header("ER visits by hospital location"),
        highchartOutput("tshosp"),
        tags$div(paste(
          "This dataset consists of records in which the hospital is within",
          "the Kansas City boundary."
        ))
      )
    )
  ),

  nav_panel(
    "SaTScan cluster detection",
    navset_tab(
      nav_panel(
        "Overview",
        card(gt_output("clustcounts"), height = "80vh")
      ),
      nav_panel(
        "Clusters by patient residence",
        layout_column_wrap(
          card(leafletOutput("pmap")),
          card(
            card_header("Cluster locations"),
            reactableOutput("ploc")
          ),
          height = "50vh"
        ),
        # card(textOutput("txt")),
        card(
          card_header("Clusters"),
          # textOutput("tbltitle1"),
          reactableOutput("pclust"),
          height = "30vh"
        )
      ),
      nav_panel(
        "Clusters by hospital location",
        layout_column_wrap(
          card(leafletOutput("hmap")),
          card(
            card_header("Cluster locations"),
            reactableOutput("hloc")
          ),
          height = "50vh"
        ),
        card(
          card_header("Clusters"),
          reactableOutput("hclust"),
          height = "30vh"
        )
      )
    )
  ),

  nav_panel(
    "About",
    card(markdown(readLines("../scripts/about.md")))
  )

)
