

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





syn <- "resp"

# Filter cluster data
clust <- lapply(
  list(
    patient = ssresults$patient[[syn]],
    hospital = ssresults$hospital[[syn]]
  ),
  config_ss_output,
  sig_pval = TRUE
)

# Cluster map (by patient)
cluster_map(clust$patient$shapeclust, clust$patient$gis)

# Cluster map (by hospital)
cluster_map(clust$hospital$shapeclust, clust$hospital$gis)

# Cluster data table (by patient)
tbl <- cluster_table(clust$patient$shapeclust, 1)

tbl










