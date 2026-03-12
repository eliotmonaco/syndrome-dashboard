
"Data last updated on DATE at TIME"





url <- build_ess_url(
  syndrome = syn_api$resp,
  start = Sys.Date() - 5,
  data_source = "patient",
  output = "dd",
  dd_fields = NULL
)

df <- get_ess_dd(url)





df <- filter_ts(ts$patient$resp, date_buttons$`90 days`)

ls <- df_to_hc_list(df)

ts_plot(ls, "Title")





syndrome <- "alc"

ssresults <- get_satscan_results(ssfull, max(dt))

# Filter cluster data
clustdata <- filter_cluster_data(ssresults, syndrome, FALSE)

# Filter cluster regions
clustregion <- list(
  patient = filter_cluster_regions(
    clustdata$patient,
    geo = geo$zctas,
    var = "GEOID20"
  ),
  hospital = filter_cluster_regions(
    clustdata$hospital,
    geo = clustdata$hospital$shapeclust,
    var = "loc_id"
  )
)

# Cluster map (by patient)
cluster_map(
  clusters = clustdata$patient$shapeclust,
  cluster_regions = clustregion$patient,
)

# Cluster map (by hospital)
cluster_map(
  clusters = clustdata$hospital$shapeclust,
  cluster_regions = clustregion$hospital,
  hospital_locations = geo$hosp
)

# Cluster count table
ssresults |>
  significant_clusters_by_syndrome() |>
  clustcount_table()

# Cluster data table (by patient)
cluster_table(clustdata$patient$shapeclust)

# Location data table (by patient)
location_table(clustdata$patient$gis, id = 1, type = "patient")
location_table(clustdata$hospital$gis, id = 1, type = "hospital")







ddhosp <- lapply(dd, \(ls) {list_rbind(ls$hospital)}) |>
  list_rbind()

ddpat <- lapply(dd, \(ls) {list_rbind(ls$patient)}) |>
  list_rbind()

apply(ddpat, 2, \(x) setmeup::pct(sum(grepl("^none$", x)), nrow(ddpat), 1))

apply(ddhosp, 2, \(x) setmeup::pct(sum(grepl("^none$", x)), nrow(ddhosp), 1))

ddpat |>
  count(patient_state)

ddpat |>
  count(patient_country)

ddpat |>
  count(hospital_name)

ddpat |>
  count(hospital_state)

ddhosp |>
  count(patient_state)

ddhosp |>
  count(patient_country)

ddhosp |>
  count(hospital_name)

ddhosp |>
  count(hospital_state)




x <- "https://moessence.inductivehealth.com/ih_essence/api/dataDetails/csv?geography=64105,64108&datasource=va_er&startDate=11Dec2025&medicalGroupingSystem=essencesyndromes&userId=5809&endDate=11Mar2026&percentParam=noPercent&aqtTarget=DataDetails&geographySystem=zipcode&detector=probrepswitch&timeResolution=daily"
strsplit(x, "&")
