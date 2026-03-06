
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

# Filter cluster data
clustdata <- filter_cluster_data(ssresults, syndrome, TRUE)

# Filter cluster ZCTAs
clustzcta <- filter_cluster_zctas(clustdata)

# Cluster map (by patient)
cluster_map(
  clusters = clustdata$patient$shapeclust,
  cluster_zctas = clustzcta$patient
)

# Import and filter hospital locations
hosploc <- readRDS("data/hospital_locations.rds")

hosploc <- hosploc |>
  select(hospital_name, long, lat)

hosploc <- hosploc |>
  st_as_sf(coords = c("long", "lat"), crs = "WGS84") |>
  st_filter(kczcta)

# Cluster map (by hospital)
cluster_map(
  clusters = clustdata$hospital$shapeclust,
  cluster_zctas = clustzcta$hospital,
  hospital_locations = hosploc
)

# Cluster count table
ssresults |>
  significant_clusters_by_syndrome() |>
  clustcount_table()

# Cluster data table (by patient)
cluster_table(clustdata$patient$shapeclust)

# Location data table (by patient)
location_table(clustdata$patient$gis, 1)







