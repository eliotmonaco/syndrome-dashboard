page_navbar(
  title = "Kansas City Syndrome Tracker",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#8fccbf") |>
    bs_add_rules(sass::sass_file("www/sass/custom.scss")),

  nav_panel(
    "Time series",
    layout_sidebar(
      sidebar = sidebar(
        date_input_analysis("date1", dt),
        select_input_syndrome("syn1", synselect1),
        radio_buttons_daterange("dtrng1", daterng1)
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
        date_input_analysis("date2", dt),
        select_input_syndrome("syn2", synselect1),
        radio_buttons_daterange("dtrng2", daterng1)
      ),
      navset_tab(
        nav_panel(
          "Data by patient location",
          layout_column_wrap(
            width = "300px",
            card_dc(reactableOutput("ddtblp1")),
            card_dc(reactableOutput("ddtblp2")),
            card_dc(reactableOutput("ddtblp3")),
            card_dc(reactableOutput("ddtblp4")),
            card_dc(reactableOutput("ddtblp5")),
            card_dc(reactableOutput("ddtblp6")),
            card_dc(reactableOutput("ddtblp7"))
          )
        ),
        nav_panel(
          "Data by hospital location",
          layout_column_wrap(
            width = "300px",
            card_dc(reactableOutput("ddtblh1")),
            card_dc(reactableOutput("ddtblh2")),
            card_dc(reactableOutput("ddtblh3")),
            card_dc(reactableOutput("ddtblh4")),
            card_dc(reactableOutput("ddtblh5")),
            card_dc(reactableOutput("ddtblh6")),
            card_dc(reactableOutput("ddtblh7"))
          )
        )
      )
    )
  ),

  nav_panel(
    "Clusters",
    layout_sidebar(
      sidebar = sidebar(
        date_input_analysis("date3", dt),
        select_input_syndrome("syn3", synselect1),
        checkboxInput(
          inputId = "sigp",
          label = uitext$cblabel,
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
