
"Data last updated on DATE at TIME"

ssresults |>
  significant_clusters_by_syndrome() |>
  clustcount_table()







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





syn <- "meas"

# Filter cluster data
clustdata <- lapply(
  list(
    patient = ssresults$patient[[syn]],
    hospital = ssresults$hospital[[syn]]
  ),
  config_ss_output,
  sig_pval = TRUE
)

# Cluster map (by patient)
cluster_map(clustdata$patient$shapeclust, clustdata$patient$gis)

# Cluster map (by hospital)
cluster_map(clustdata$hospital$shapeclust, clustdata$hospital$gis)

# Cluster data table (by patient)
cluster_table(clustdata$patient$shapeclust, 1)

# Location data table (by patient)
location_table(clustdata$patient$gis, 1)












