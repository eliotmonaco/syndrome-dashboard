# Create mock data for dashboard functions

library(tidyverse)

source("scripts/fn.R")

geo <- readRDS("data/geographic_data.rds")
ansi <- readRDS("data/ansi_state_codes.rds")

syn <- list(
  syn1 = list(
    name = "syndrome 1",
    queryname = "Syndrome 1 query",
    kr = "https://",
    apistring = "???"
  ),
  syn2 = list(
    name = "syndrome 2",
    queryname = "Syndrome 2 query",
    kr = "https://",
    apistring = "???"
  ),
  syn3 = list(
    name = "syndrome 3",
    queryname = "Syndrome 3 query",
    kr = "https://",
    apistring = "???"
  )
)

end_date <- as.Date("3001-01-02")

start_date <- get_start_date(end_date)

date_range <- seq.Date(start_date, end_date, "day")

# Essence time series -----------------------------------------------------

# Variables: `date`, `count`, `color_id`

tsraw <- lapply(list(1, 2), \(x) {
  params <- list(
    dates = list(date_range),
    lambda = list(1:10, 5:20, 50:100),
    seed = list(x)
  )

  ls <- pmap(params, \(dates, lambda, seed) {
    # Counts
    set.seed(seed)

    ct <- rpois(length(dates), lambda)

    # Color IDs
    id <- cut(
      ct,
      breaks = c(0, mean(ct) / 2, mean(ct), mean(ct) * 1.5, Inf),
      labels = c(0, 1, 2, 3),
      right = FALSE
    )

    # Data
    df <- data.frame(
      date = dates,
      count = ct,
      color_id = id
    )

    # API message
    msg <- "Success"

    list(data = df, message = msg)
  })

  names(ls) <- names(syn)

  ls
})

names(tsraw) <- c("patient", "hospital")

# Essence data details ----------------------------------------------------

# Variables: `date`, `time`, `age`, `age_group`, `sex`, `date_of_birth`,
#   `zip_code`, `patient_state`, `patient_country`, `hospital_name`,
#   `hospital_state`, `visit_number`, `has_been_e`
ddraw <- lapply(list(1, 2), \(x) {
  params <- list(
    dates = list(date_range),
    end = end_date,
    lambda = list(1:10, 5:20, 50:100),
    seed = list(x),
    zctas = list(geo$zctas$GEOID20),
    hospitals = list(geo$hosp$hospital_name_geo)
  )

  ls <- pmap(params, \(dates, end, lambda, seed, zctas, hospitals) {
    # Counts
    set.seed(seed)

    ct <- rpois(length(dates), lambda)

    # Create visit dates
    dates <- map2(dates, ct, \(x, y) {
      rep(x, y)
    })

    dates <- as.Date(unlist(dates))

    # Time
    tm <- seq.POSIXt(
      as.POSIXct("2023-01-01 00:00:00"),
      as.POSIXct("2023-01-01 23:00:00"),
      "hour"
    )

    set.seed(seed)

    tm <- sample(tm, length(dates), replace = TRUE)

    tm <- format(tm, "%H:%m %p")

    # Age
    set.seed(seed)

    age <- floor(runif(length(dates), 0, 100))

    # Age group
    agegp <- cut(
      age,
      breaks = c(0, 5, 18, 45, 65, Inf),
      labels = c("00-04", "05-17", "18-44", "45-64", "65-1000"),
      right = FALSE
    )

    # Sex
    set.seed(seed)

    sex <- sample(c("F", "M"), length(dates), replace = TRUE)

    # DOB
    dob <- end - age

    # ZIP codes
    set.seed(seed)

    zip <- sample(zctas, length(dates), replace = TRUE)

    # Hospital name
    set.seed(seed)

    hosp <- sample(hospitals, length(dates), replace = TRUE)

    # Data
    df <- data.frame(
      date = dates,
      time = tm,
      age = age,
      age_group = agegp,
      sex = sex,
      date_of_birth = dob,
      zip_code = zip,
      patient_state = "MO",
      patient_country = "USA",
      hospital_name = hosp,
      hospital_state = "MO",
      visit_number = 1:length(dates),
      has_been_e = 1
    )

    # API message
    msg <- "Success"

    list(data = df, message = msg)
  })

  names(ls) <- names(syn)

  ls
})

names(ddraw) <- c("patient", "hospital")

# Create duplicates for `ddraw$patient$syn1$data`
df <- ddraw$patient$syn1$data

df <- rbind(
  df,
  df[1,][c(1, 1, 1),],
  mutate(df[1,], date = date + 1),
  mutate(df[1,], zip_code = 64108)
)

rownames(df) <- NULL

tail(df, 5)

ddraw$patient$syn1$data <- df

# Create duplicates for `ddraw$hospital$syn2$data`
df <- ddraw$hospital$syn2$data

df <- rbind(
  df,
  df[1,][c(1, 1, 1),],
  mutate(df[1,], date = date + 1),
  mutate(df[1,], hospital_name = "Research_Medical_Center")
)

rownames(df) <- NULL

tail(df, 5)

ddraw$hospital$syn2$data <- df

# Configured Essence data -------------------------------------------------

# Separate data from API messages
dd <- lapply(ddraw, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

ts <- lapply(tsraw, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

# Configure
dd <- map2(dd, c("zip_code", "hospital_name"), \(ls, x) {
  lapply(ls, \(df) {
    tryCatch(
      config_dd(df, geo_var = x),
      error = function(e) e
    )
  })
})

ts <- lapply(ts, \(ls) {
  lapply(ls, \(df) {
    tryCatch(
      config_ts(df),
      error = function(e) e
    )
  })
})

# Separate data from error tables in data details
dderror <- lapply(dd, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$error_rate
  })
})

dd <- lapply(dd, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

# Satscan output ----------------------------------------------------------











