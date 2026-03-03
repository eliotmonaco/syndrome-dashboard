function(input, output, session) {

  # REACTIVES ---------------------------------------------------------------

  rv <- reactiveValues()

  # Initialize as NULL for validation message to appear in cluster location
  # table even when p-value checkbox is not selected
  rv$pmapid <- NULL; rv$hmapid <- NULL

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

    filter_cluster_data(ssresults, input$syn, input$sigp)
  })

  # Filter ZCTA cluster regions
  clustzcta <- reactive({
    req(clustdata())

    filter_cluster_zctas(clustdata())
  })

  # TEXT --------------------------------------------------------------------

  # output$tbltitle1 <- renderText({
  #   req(input$syn)
  #
  #   paste(
  #     names(syn_names)[syn_names == input$syn],
  #     "clusters"
  #   )
  # })

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
    req(clustdata(), clustzcta(), input$syn)

    cluster_map(
      clusters = clustdata()$patient$shapeclust,
      cluster_zctas = clustzcta()$patient
    )
  })

  # Cluster map (by hospital)
  output$hmap <- renderLeaflet({
    req(clustdata(), clustzcta(), input$syn)

    cluster_map(
      clusters = clustdata()$hospital$shapeclust,
      cluster_zctas = clustzcta()$hospital
    )
  })

  # TABLES ------------------------------------------------------------------

  # Cluster counts
  output$clustcounts <- render_gt({
    ssresults |>
      significant_clusters_by_syndrome() |>
      clustcount_table()

  })

  # Cluster data table (by patient)
  output$pclust <- renderReactable({
    req(clustdata())

    tbl <- cluster_table(clustdata()$patient$shapeclust)

    validate(need(tbl, clust_val_text))

    tbl
  })

  # Cluster data table (by hospital)
  output$hclust <- renderReactable({
    req(clustdata())

    tbl <- cluster_table(clustdata()$hospital$shapeclust)

    validate(need(tbl, clust_val_text))

    tbl
  })

  # Location data table (by patient)
  output$ploc <- renderReactable({
    req(clustdata())

    tbl <- location_table(
      clustdata()$patient$gis,
      id = rv$pmapid
    )

    validate(need(rv$pmapid, loc_val_text))

    tbl
  })

  # Location data table (by hospital)
  output$hloc <- renderReactable({
    req(clustdata())

    tbl <- location_table(
      clustdata()$hospital$gis,
      id = rv$hmapid
    )

    validate(need(rv$hmapid, loc_val_text))

    tbl
  })

  # OBSERVERS ---------------------------------------------------------------

  # Reset map cluster ID as NULL when a new syndrome is selected
  observeEvent(input$syn, {
    rv$pmapid <- NULL; rv$hmapid <- NULL
  })

  # Get map cluster ID and update cluster table row selection
  observeEvent(input$pmap_shape_click, {
    rv$pmapid <- input$pmap_shape_click$id

    updateReactable("pclust", selected = ifelse(
      is.null(rv$pmapid), NA, rv$pmapid
    ))

    # output$txt <- renderText({rv$pmapid})

    # leafletProxy("pmap") |>
    #   map_add_cluster(clustzcta()$patient, rv$pmapid) |>
    #   map_remove_cluster(clustzcta()$patient, rv$pmapid)
  })

  observeEvent(input$hmap_shape_click, {
    rv$hmapid <- input$hmap_shape_click$id

    updateReactable("hclust", selected = ifelse(
      is.null(rv$hmapid), NA, rv$hmapid
    ))
  })

  # When cluster table row is selected, update map cluster ID
  observeEvent(getReactableState("pclust"), {
    rv$pmapid <- getReactableState("pclust", name = "selected")
  })

  observeEvent(getReactableState("hclust"), {
    rv$hmapid <- getReactableState("hclust", name = "selected")
  })

}
