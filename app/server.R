function(input, output, session) {

  # output$text <- renderText({names(maps)})

  # Time series by patient
  tspat <- reactive({
    req(input$syn, input$dtrng1)

    df <- filter_ts(ts$patient[[input$syn]], as.Date(input$dtrng1))

    df_to_hc_list(df)
  })

  # Time series by hospital
  tshosp <- reactive({
    req(input$syn, input$dtrng1)

    df <- filter_ts(ts$hospital[[input$syn]], as.Date(input$dtrng1))

    df_to_hc_list(df)
  })

  # Line plot: time series by patient
  output$tspat <- renderHighchart({
    req(tspat())

    ttl <- names(syn_names)[which(syn_names == input$syn)]

    ts_plot(tspat(), title = ttl)
  })

  # Line plot: time series by hospital
  output$tshosp <- renderHighchart({
    req(tshosp())

    ttl <- names(syn_names)[which(syn_names == input$syn)]

    ts_plot(tshosp(), title = ttl)
  })

  # Filter cluster data
  clust <- reactive({
    req(input$syn)

    lapply(
      list(
        patient = ssresults$patient[[input$syn]],
        hospital = ssresults$hospital[[input$syn]]
      ),
      config_ss_output,
      sig_pval = input$sigp
    )
  })

  # Cluster map (by patient)
  output$clustermap_pat <- renderLeaflet({
    req(clust(), input$syn)

    cluster_map(clust()$patient$shapeclust, clust()$patient$gis)
  })

  # Cluster map (by hospital)
  output$clustermap_hosp <- renderLeaflet({
    req(clust(), input$syn)

    cluster_map(clust()$hospital$shapeclust, clust()$hospital$gis)
  })

  # Cluster data table (by patient)
  output$clustertbl_pat <- renderDT({
    req(clust(), input$syn)

    cluster_table(clust()$patient$shapeclust)
  })

  # Cluster data table (by hospital)
  output$clustertbl_hosp <- renderDT({
    req(clust(), input$syn)

    cluster_table(clust()$hospital$shapeclust)
  })

}
