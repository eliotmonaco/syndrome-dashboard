function(input, output, session) {

  # REACTIVES ---------------------------------------------------------------

  rv <- reactiveValues()

  # Initialize map cluster IDs as NULL for validation message to appear in
  # cluster location table even when p-value checkbox is not selected
  rv$pmapid <- NULL; rv$hmapid <- NULL

  # Time series data
  tspat <- reactive({
    req(rv$syn, input$dtrng1)

    df <- filter_ts(ts$patient[[rv$syn]], as.Date(input$dtrng1))

    df_to_hc_list(df)
  })

  tshosp <- reactive({
    req(rv$syn, input$dtrng1)

    df <- filter_ts(ts$hospital[[rv$syn]], as.Date(input$dtrng1))

    df_to_hc_list(df)
  })

  # Satscan results
  ss <- reactive({
    req(input$dt)

    get_satscan_results(ssfull, input$dt)
  })

  # Filter cluster data
  clustdata <- reactive({
    req(rv$syn)

    config_syndrome_data(ss(), rv$syn, input$sigp)
  })

  # Filter cluster locations for mapping
  clustloc <- reactive({
    req(clustdata())

    list(
      patient = filter_location_geometries(
        clustdata()$patient,
        geo = geo$zctas,
        var = "GEOID20"
      ),
      hospital = filter_location_geometries(
        clustdata()$hospital,
        geo = clustdata()$hospital$shapeclust,
        var = "loc_id"
      )
    )
  })

  # TEXT --------------------------------------------------------------------

  output$syn1 <- renderUI({
    syndrome_title_tag(rv$syn)
  })

  output$syn2 <- renderUI({
    syndrome_title_tag(rv$syn)
  })

  # PLOTS -------------------------------------------------------------------

  # Line plot: time series by patient
  output$tspat <- renderHighchart({
    req(tspat())

    ttl <- names(syn_names)[which(syn_names == rv$syn)]

    ts_plot(tspat(), title = ttl)
  })

  # Line plot: time series by hospital
  output$tshosp <- renderHighchart({
    req(tshosp())

    ttl <- names(syn_names)[which(syn_names == rv$syn)]

    ts_plot(tshosp(), title = ttl)
  })

  # Cluster map (by patient)
  output$pmap <- renderLeaflet({
    req(clustdata(), clustloc(), rv$syn)

    cluster_map(
      cluster_locations = clustloc()$patient,
      location_boundaries = geo$zctas,
      kc_boundary = geo$city,
      gp = gp_pat
    )
  })

  # Cluster map (by hospital)
  output$hmap <- renderLeaflet({
    req(clustdata(), clustloc(), rv$syn)

    cluster_map(
      cluster_locations = clustloc()$hospital,
      cluster_points = clustdata()$hospital$shapegis,
      location_boundaries = geo$counties,
      kc_boundary = geo$city,
      hospital_locations = geo$hosp,
      gp = gp_hosp
    )
  })

  # TABLES ------------------------------------------------------------------

  # Cluster count table
  output$clustct <- renderReactable({
    ss() |>
      significant_clusters_by_syndrome() |>
      clustcount_table()
  })

  # Cluster data tables
  output$pclust <- renderReactable({
    validate(need(clustdata()$patient$shapeclust, uitext$val_clust))

    cluster_table(clustdata()$patient$shapeclust)
  })

  output$hclust <- renderReactable({
    validate(need(clustdata()$hospital$shapeclust, uitext$val_clust))

    cluster_table(clustdata()$hospital$shapeclust)
  })

  # Location data tables
  output$ploc <- renderReactable({
    validate(need(rv$pmapid, uitext$val_loc))

    location_table(
      clustdata()$patient$gis,
      id = rv$pmapid,
      type = "patient"
    )
  })

  output$hloc <- renderReactable({
    validate(need(rv$hmapid, uitext$val_loc))

    location_table(
      clustdata()$hospital$gis,
      id = rv$hmapid,
      type = "hospital"
    )
  })

  # OBSERVERS ---------------------------------------------------------------

  # Update syndrome selection when any relevant select input is changed
  observeEvent(input$syn1, {
    rv$syn <- input$syn1

    updateSelectInput(session, "syn2", selected = rv$syn)
  })

  observeEvent(input$syn2, {
    rv$syn <- input$syn2

    updateSelectInput(session, "syn1", selected = rv$syn)
  })

  # Reset map cluster ID as NULL when a new syndrome is selected
  observeEvent(rv$syn, {
    rv$pmapid <- NULL; rv$hmapid <- NULL
  })

  # Get map cluster ID and update cluster table row selection
  observeEvent(input$pmap_shape_click, {
    rv$pmapid <- input$pmap_shape_click$id

    updateReactable("pclust", selected = ifelse(
      is.null(rv$pmapid), NA, rv$pmapid
    ))
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
