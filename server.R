function(input, output, session) {

  # REACTIVES ---------------------------------------------------------------

  rv <- reactiveValues()

  # Initialize map cluster IDs as NULL for validation message to appear in
  # cluster location table even when p-value checkbox is not selected
  rv$pmapid <- NULL; rv$hmapid <- NULL

  # Syndrome list
  syn <- reactive({
    req(rv$date)
    get_list_data(dbdata, rv$date, "syn")
  })

  # Input choices for syndromes
  synselect <- reactive({
    req(syn())
    syn_select_list(syn())
  })

  # Input choices for date range
  daterng <- reactive({
    req(rv$date)
    dbdata |>
      get_list_data(rv$date, "daterng")
      daterange_select_list()
  })

  # Time series data
  ts <- reactive({
    req(rv$date, rv$syn, rv$dtrng)
    dbdata |>
      get_list_data(rv$date, "ts") |>
      get_ts_data(rv$syn, rv$dtrng)
  })

  # Data details data
  dd <- reactive({
    req(rv$date, rv$syn, rv$dtrng)
    dbdata |>
      get_list_data(rv$date, "dd") |>
      get_dd_data(rv$syn, rv$dtrng)
  })

  # Satscan results
  ss <- reactive({
    req(rv$date)
    get_list_data(dbdata, rv$date, "ss")
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

  output$titlesyn1 <- renderUI({
    syndrome_title_tag(rv$syn, synselect())
  })

  output$titlesyn2 <- renderUI({
    syndrome_title_tag(rv$syn, synselect())
  })

  # PLOTS -------------------------------------------------------------------

  # Line plot: time series by patient
  output$tspat <- renderHighchart({
    req(ts()$patient, synselect())
    ts_plot(
      ts()$patient,
      title = names(synselect())[which(synselect() == rv$syn)]
    )
  })

  # Line plot: time series by hospital
  output$tshosp <- renderHighchart({
    req(ts()$hospital, synselect())
    ts_plot(
      ts()$hospital,
      title = names(synselect())[which(synselect() == rv$syn)]
    )
  })

  # Cluster map (by patient)
  output$pmap <- renderLeaflet({
    req(clustdata(), clustloc(), rv$syn, input$zoom)
    cluster_map(
      cluster_locations = clustloc()$patient,
      location_boundaries = geo$zctas,
      kc_boundary = geo$city,
      gp = gp_pat,
      zoom_level = input$zoom
    )
  })

  # Cluster map (by hospital)
  output$hmap <- renderLeaflet({
    req(clustdata(), clustloc(), rv$syn, input$zoom)
    cluster_map(
      cluster_locations = clustloc()$hospital,
      cluster_points = clustdata()$hospital$shapegis,
      location_boundaries = geo$counties,
      kc_boundary = geo$city,
      hospital_locations = geo$hosp,
      gp = gp_hosp,
      zoom_level = input$zoom
    )
  })

  # TABLES ------------------------------------------------------------------

  ## Data characteristics

  ### By patient location

  output$ddtblp1 <- renderReactable({
    dd_table(dd()$patient, "sex")
  })

  output$ddtblp2 <- renderReactable({
    dd_table(dd()$patient, "age_group")
  })

  output$ddtblp3 <- renderReactable({
    dd_table(dd()$patient, "patient_state")
  })

  output$ddtblp4 <- renderReactable({
    dd_table(dd()$patient, "patient_country")
  })

  output$ddtblp5 <- renderReactable({
    dd_table(dd()$patient, "hospital_name")
  })

  output$ddtblp6 <- renderReactable({
    dd_table(dd()$patient, "hospital_state")
  })

  output$ddtblp7 <- renderReactable({
    dd_table(dd()$patient, "has_been_e", "has been emergency")
  })

  ### By hospital location

  output$ddtblh1 <- renderReactable({
    dd_table(dd()$hospital, "sex")
  })

  output$ddtblh2 <- renderReactable({
    dd_table(dd()$hospital, "age_group")
  })

  output$ddtblh3 <- renderReactable({
    dd_table(dd()$hospital, "patient_state")
  })

  output$ddtblh4 <- renderReactable({
    dd_table(dd()$hospital, "patient_country")
  })

  output$ddtblh5 <- renderReactable({
    dd_table(dd()$hospital, "hospital_name")
  })

  output$ddtblh6 <- renderReactable({
    dd_table(dd()$hospital, "hospital_state")
  })

  output$ddtblh7 <- renderReactable({
    dd_table(dd()$hospital, "has_been_e", "has been emergency")
  })

  ## Clusters

  # Cluster count table
  output$clustct <- renderReactable({
    ss() |>
      significant_clusters_by_syndrome(syndromes = syn()) |>
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

  # Update analysis date when any date input is changed
  observeEvent(input$date1, {
    rv$date <- input$date1
    updateDateInput(session, "date2", value = rv$date)
    updateDateInput(session, "date3", value = rv$date)
  })

  observeEvent(input$date2, {
    rv$date <- input$date2
    updateDateInput(session, "date1", value = rv$date)
    updateDateInput(session, "date3", value = rv$date)
  })

  observeEvent(input$date3, {
    rv$date <- input$date3
    updateDateInput(session, "date1", value = rv$date)
    updateDateInput(session, "date2", value = rv$date)
  })

  # Update syndrome selections when the syndrome list is changed
  observeEvent(synselect(), {
    updateSelectInput(session, "syn1", choices = synselect(), selected = rv$syn)
    updateSelectInput(session, "syn2", choices = synselect(), selected = rv$syn)
    updateSelectInput(session, "syn3", choices = synselect(), selected = rv$syn)
  })

  # Update syndrome selections when any relevant select input is changed
  observeEvent(input$syn1, {
    rv$syn <- input$syn1
    updateSelectInput(session, "syn2", selected = rv$syn)
    updateSelectInput(session, "syn3", selected = rv$syn)
  })

  observeEvent(input$syn2, {
    rv$syn <- input$syn2
    updateSelectInput(session, "syn1", selected = rv$syn)
    updateSelectInput(session, "syn3", selected = rv$syn)
  })

  observeEvent(input$syn3, {
    rv$syn <- input$syn3
    updateSelectInput(session, "syn2", selected = rv$syn)
    updateSelectInput(session, "syn3", selected = rv$syn)
  })

  # Update date range selections when any relevant select input is changed
  observeEvent(input$dtrng1, {
    rv$dtrng <- input$dtrng1
    updateRadioButtons(session, "dtrng2", selected = rv$dtrng)
  })

  observeEvent(input$dtrng2, {
    rv$dtrng <- input$dtrng2
    updateRadioButtons(session, "dtrng1", selected = rv$dtrng)
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
