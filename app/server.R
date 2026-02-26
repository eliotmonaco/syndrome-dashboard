function(input, output, session) {

  # output$text <- renderText({names(maps)})

  # Time series by patient
  tspat <- reactive({
    req(input$syn, input$dtrng1)

    # filter_ts(ts$patient[[input$syn]], input$dtrng1[1], input$dtrng1[2])
    filter_ts(ts$patient[[input$syn]], as.Date(input$dtrng1))
  })

  # Time series by hospital
  tshosp <- reactive({
    req(input$syn, input$dtrng1)

    # filter_ts(ts$hospital[[input$syn]], input$dtrng1[1], input$dtrng1[2])
    filter_ts(ts$hospital[[input$syn]], as.Date(input$dtrng1))
  })

  # Line plot: time series by patient
  output$tspat <- renderHighchart({
    req(tspat())

    ttl <- names(syn_names)[which(syn_names == input$syn)]

    p <- syn_highchart(tspat(), title = ttl)

    p
  })

  # Line plot: time series by hospital
  output$tshosp <- renderHighchart({
    req(tshosp())

    ttl <- names(syn_names)[which(syn_names == input$syn)]

    p <- syn_highchart(tshosp(), title = ttl)

    p
  })

  # Filter cluster data
  clust <- reactive({
    req(input$syn)

    filter_ss_output(ssresults, input$syn, input$sigp)
  })

  # Cluster map (by patient)
  output$clustermap_pat <- renderPlot({
    req(clust(), input$syn)

    nm <- paste0("patient.", input$syn)

    pts <- get_cluster_points(clust()[[nm]]$gis)

    cluster_map(clust()[[nm]]$shapeclust, pts)
  })

  # Cluster map (by hospital)
  output$clustermap_hosp <- renderPlot({
    req(clust(), input$syn)

    nm <- paste0("hospital.", input$syn)

    pts <- get_cluster_points(clust()[[nm]]$gis)

    cluster_map(clust()[[nm]]$shapeclust, pts)
  })

  # Cluster data table (by patient)
  output$clustertbl_pat <- renderDT({
    req(clust(), input$syn)

    nm <- paste0("patient.", input$syn)

    cluster_table(clust()[[nm]]$shapeclust)
  })

  # Cluster data table (by hospital)
  output$clustertbl_hosp <- renderDT({
    req(clust(), input$syn)

    nm <- paste0("hospital.", input$syn)

    cluster_table(clust()[[nm]]$shapeclust)
  })

}
