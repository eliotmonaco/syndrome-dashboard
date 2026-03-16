page_navbar(
  title = "Kansas City Syndrome Tracker",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#8fccbf") |>
    bs_add_rules(sass::sass_file("www/sass/custom.scss")),

  nav_panel(
    "Time series",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "syn1",
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
        )
      ),
      card(
        card_header("ER visits by patient location"),
        highchartOutput("tspat"),
        tags$div(uitext$tspat)
      ),
      card(
        card_header("ER visits by hospital location"),
        highchartOutput("tshosp"),
        tags$div(uitext$tshosp)
      )
    )
  ),

  nav_panel(
    "Data characteristics",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "syn2",
          label = "Syndrome",
          choices = syn_names,
          multiple = FALSE,
          selected = syn_names[[1]]
        ),
        radioButtons(
          inputId = "dtrng2",
          label = "Date range",
          choices = date_buttons,
          selected = date_buttons[[2]]
        )
      ),
      layout_column_wrap(
        width = "300px",
        fill = FALSE,
        class = "data-char",
        card_dc(reactableOutput("ddtbl1")),
        card_dc(reactableOutput("ddtbl2")),
        card_dc(reactableOutput("ddtbl3")),
        card_dc(reactableOutput("ddtbl4")),
        card_dc(reactableOutput("ddtbl5")),
        card_dc(reactableOutput("ddtbl6")),
        card_dc(reactableOutput("ddtbl7")),
      )
    )
  ),

  nav_panel(
    "Clusters",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "syn3",
          label = "Syndrome",
          choices = syn_names,
          multiple = FALSE,
          selected = syn_names[[1]]
        ),
        dateInput(
          inputId = "dt",
          label = "Analysis date",
          value = max(dt),
          min = min(dt),
          max = max(dt)
        ),
        checkboxInput(
          inputId = "sigp",
          label = "Show clusters where p < 0.05 only",
          value = TRUE
        )
      ),
      navset_tab(
        nav_panel(
          "Overview",
          card(
            p(uitext$cctbl),
            reactableOutput("clustct"),
            class = "overview-tbl"
          )
        ),
        nav_panel(
          "Clusters by patient location",
          htmlOutput("titlesyn1"),
          layout_column_wrap(
            card(
              leafletOutput("pmap"),
              class = "map-zip-row"
            ),
            card(
              card_header("Locations in cluster"),
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
          htmlOutput("titlesyn2"),
          layout_column_wrap(
            card(
              leafletOutput("hmap"),
              class = "map-zip-row"
            ),
            card(
              card_header("Locations in cluster"),
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
    )
  ),

  nav_panel(
    "About",
    card(markdown(readLines("scripts/about.md")))
  )

)
