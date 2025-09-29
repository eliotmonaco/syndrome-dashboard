# Configure Essence data

library(tidyverse)

# Pull Essence data
source("scripts/get-ess.R")

path <- "data/ess_raw_data.rds"

ess <- readRDS(path)

# Configure ess -----------------------------------------------------------

# Configure data details df
ess$dd <- lapply(ess$dd, \(x) {
  colnames(x) <- tolower(colnames(x))

  x |>
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      age = suppressWarnings(as.numeric(age)),
      zipcode = suppressWarnings(as.character(as.numeric(zipcode)))
    ) |>
    arrange(date)
})

# Configure time series df
ess$ts <- lapply(ess$ts, \(x) {
  colnames(x) <- tolower(colnames(x))

  x |>
    mutate(
      date = as.Date(date)
    ) |>
    arrange(date)
})

# # Regression/EWMA Switch
# ess$ts_sw <- lapply(ess$ts, \(x) {
#   # browser()
#   tryCatch(
#     alert_switch(x),
#     error = function(e) NULL
#   )
# })

# # Trend classification
# ts_switch <- classify_trend(ts1)

# Syndrome names
syn <- as.list(names(ess$dd))

names(syn) <- list(
  "Symptom: Abdominal pain",
  "Symptom: Diarrhea",
  "Symptom: Fever",
  "Symptom: GI",
  "Symptom: Neurological",
  "Symptom: Rash",
  "Symptom: Respiratory",
  "Symptom: Vomiting",
  "Respiratory: COVID-19",
  "Respiratory: Influenza-like illness",
  "Respiratory: RSV",
  "Bioterrorism: Anthrax",
  "Bioterrorism: Botulism",
  "Bioterrorism: Plague",
  "Bioterrorism: Smallpox",
  "Bioterrorism: Tularemia",
  "Bioterrorism: Viral Hemorrhagic Fever"
)

# Add syndrome names and stack dfs
ts <- imap(ess$ts, \(df, i) {
  df |>
    mutate(
      syndrome = i,
      syndrome_full = names(syn)[which(syn == i)],
      .before = 1
    )
})

ts <- list_rbind(ts)

# Add row ID, alert status, and alert color
lvl <- c("Normal", "Normal", "Warning", "Alert")

pal <- c("#000000", "#f2c00a", "#ff0000")
names(pal) <- c("Normal", "Warning", "Alert")

ts <- ts |>
  mutate(id = sprintf(paste0("id%0", nchar(nrow(ts)), "d"), 1:nrow(ts))) |>
  mutate(alert_status = case_when(
    colorid == 0 ~ lvl[1],
    colorid == 1 ~ lvl[2],
    colorid == 2 ~ lvl[3],
    colorid == 3 ~ lvl[4]
  )) |>
  mutate(alert_status = factor(alert_status, levels = unique(lvl)),) |>
  mutate(alert_color = pal[alert_status]) |>
  select(id, syndrome, syndrome_full, date, count, alert_status, alert_color)

# Time series dataframe from ess$ts -------------------------------

# dates <- ess$ts[[1]]$date
#
# ts1 <- lapply(ess$ts, \(x) {
#   x$count
# })
#
# ts1 <- do.call(cbind, ts1) |>
#   as.data.frame() |>
#   mutate(date = dates, .before = 1) |>
#   pivot_longer(
#     cols = !date,
#     names_to = "syndrome",
#     values_to = "count"
#   )
#
# ts1$syndrome_full <- syn_rev[ts1$syndrome]
#
# ts1$syndrome_full <- factor(ts1$syndrome_full, levels = syn_rev)

# Time series dataframe from ess$dd ------------------------------

# start_date <- as.Date("2025-01-01")
#
# ts2 <- lapply(ess$dd, \(x) {
#   x |>
#     group_by(date) |>
#     summarize(count = n()) |>
#     complete(date = seq(start_date, as.Date(Sys.Date()), by = "day")) |>
#     replace_na(list(count = 0)) |>
#     ungroup()
# })
#
# ts2 <- lapply(ts2, \(x) {
#   x$count
# })
#
# ts2 <- do.call(cbind, ts2) |>
#   as.data.frame() |>
#   mutate(date = dates, .before = 1) |>
#   pivot_longer(
#     cols = !date,
#     names_to = "syndrome",
#     values_to = "count"
#   )
#
# ts2$syndrome_full <- syn_rev[ts2$syndrome]
#
# ts2$syndrome_full <- factor(ts2$syndrome_full, levels = syn_rev)

# Compare -----------------------------------------------------------------

# all.equal(ts2, ts1)
#
# ts1 <- ts1 |>
#   rename(count_ts = count)
#
# ts2 <- ts2 |>
#   select(count_dd = count)
#
# df <- cbind(ts1, ts2) |>
#   select(date, syndrome_full, count_ts, count_dd)

# ZIP code dataset --------------------------------------------------------

# ess_zip <- lapply(dd, \(x) {
#   x |>
#     group_by(zipcode) |>
#     summarize(n = n()) |>
#     ungroup()
# })

syn <- list(
  Symptoms = syn[grepl("Symptom:", names(syn))],
  Respiratory = syn[grepl("Respiratory:", names(syn))],
  Bioterrorism = syn[grepl("Bioterrorism:", names(syn))]
)

for (i in 1:length(syn)) {
  nm <- names(syn)[i]
  names(syn[[i]]) <- str_remove(names(syn[[i]]), ".+(?=:):\\s")
  syn[[i]] <- c(
    list(all = paste(unlist(syn[[i]]), collapse = ";")),
    syn[[i]]
  )
  names(syn[[i]])[1] <- paste("All", tolower(nm))
}

saveRDS(syn, "data/syndromes.rds")
saveRDS(ts, "data/timeseries.rds")
# saveRDS(syn, "app/syndromes.rds")
# saveRDS(ts, "app/timeseries.rds")
