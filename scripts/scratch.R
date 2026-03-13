
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



source("scripts/fn.R")

debugonce(capture_message)
x <- capture_message(as.numeric("word"))
x <- capture_message(mutate(starwars, x = homeworld * 2))



safe_asnumeric <- safely(as.numeric)
x <- safe_asnumeric("word")

quiet_asnumeric <- quietly(as.numeric)
x <- quiet_asnumeric("word")

x <- as.numeric() |>
  quietly()







as.list(body(rsatscan::satscan))

trace(satscan, quote(
  status <- system(paste(shQuote(ssfile), shQuote(infile)), show.output.on.console = verbose, intern = TRUE)
), at = 8)

body(rsatscan::satscan)

untrace(rsatscan::satscan)


fn <- function(x) {
  x <- c(x, 2)
  x <- c(x, 3)
  x <- c(x, 4)
  x <- c(x, 5)
  x
}

fn(0)

as.list(body(fn))

trace(fn, quote(x <- c(x, "worms")), at = 3)

body(fn)

fn(0)

untrace(fn)



trace(satscan, edit = TRUE)
untrace(satscan)
