function(input, output, session) {

  # REACTIVES ---------------------------------------------------------------

  rv <- reactiveValues()

  # Initialize as NULL for validation message to appear in cluster location
  # table even when p-value checkbox is not selected
  rv$pclustid <- NULL; rv$hclustid <- NULL

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

  # Filter cluster data
  clustdata <- reactive({
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

  # PLOTS -------------------------------------------------------------------

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

  # Cluster map (by patient)
  output$pmap <- renderLeaflet({
    req(clustdata(), input$syn)

    cluster_map(clustdata()$patient$shapeclust, clustdata()$patient$gis)
  })

  # Cluster map (by hospital)
  output$hmap <- renderLeaflet({
    req(clustdata(), input$syn)

    cluster_map(clustdata()$hospital$shapeclust, clustdata()$hospital$gis)
  })

  # TABLES ------------------------------------------------------------------

  # Cluster counts
  output$clustcounts <- render_gt({
    ssresults |>
      significant_clusters_by_syndrome() |>
      clustcount_table()

  })

  # Cluster data table (by patient)
  output$pclust <- render_gt({
    req(clustdata())

    tbl <- cluster_table(clustdata()$patient$shapeclust, rv$pclustid)

    validate(need(tbl, "No clusters detected"))

    tbl
  })

  # Cluster data table (by hospital)
  output$hclust <- render_gt({
    req(clustdata())

    tbl <- cluster_table(clustdata()$hospital$shapeclust, rv$hclustid)

    validate(need(tbl, "No clusters detected"))

    tbl
  })

  # Location data table (by patient)
  output$ploc <- render_gt({
    req(clustdata())

    tbl <- location_table(clustdata()$patient$gis, rv$pclustid)

    validate(need(rv$pclustid, "Select a cluster on the map to see locations"))

    tbl
  })

  # Location data table (by hospital)
  output$hloc <- render_gt({
    req(clustdata())

    tbl <- location_table(clustdata()$hospital$gis, rv$hclustid)

    validate(need(rv$hclustid, "Select a cluster on the map to see locations"))

    tbl
  })

  # OBSERVERS ---------------------------------------------------------------

  # Reset cluster ID as NULL when a new syndrome is selected
  observeEvent(input$syn, {
    rv$pclustid <- NULL; rv$hclustid <- NULL
  })

  # Capture cluster ID on map click (by patient)
  observeEvent(input$pmap_shape_click, {
    rv$pclustid <- input$pmap_shape_click$id
  })

  # Capture cluster ID on map click (by hospital)
  observeEvent(input$hmap_shape_click, {
    rv$hclustid <- input$hmap_shape_click$id
  })

}
