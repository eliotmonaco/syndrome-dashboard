page_navbar(
  title = "Syndrome tracker and cluster detection",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#8fccbf") |>
    bs_add_rules(sass::sass_file("www/sass/custom.scss")),

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
        tags$div(ts_pat_text)
      ),
      card(
        card_header("ER visits by hospital location"),
        highchartOutput("tshosp"),
        tags$div(ts_hosp_text)
      )
    )
  ),

  nav_panel(
    "SaTScan cluster detection",
    navset_tab(
      nav_panel(
        "Overview",
        card(
          reactableOutput("clustct"),
          class = "overview-tbl"
        )
      ),
      nav_panel(
        "Clusters by patient residence",
        htmlOutput("syn1"),
        layout_column_wrap(
          card(
            leafletOutput("pmap"),
            class = "map-zip-row"
          ),
          card(
            card_header("Cluster locations"),
            reactableOutput("ploc"),
            class = "map-zip-row"
          )
        ),
        card(
          card_header("Clusters"),
          reactableOutput("pclust"),
          class = "clust-tbl-row"
        )
      ),
      nav_panel(
        "Clusters by hospital location",
        htmlOutput("syn2"),
        layout_column_wrap(
          card(
            leafletOutput("hmap"),
            class = "map-zip-row"
          ),
          card(
            card_header("Cluster locations"),
            reactableOutput("hloc"),
            class = "map-zip-row"
          )
        ),
        card(
          card_header("Clusters"),
          reactableOutput("hclust"),
          class = "clust-tbl-row"
        )
      )
    )
  ),

  nav_panel(
    "About",
    card(markdown(readLines("scripts/about.md")))
  )

)
