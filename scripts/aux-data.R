# Aux data

library(tidyverse)

# Import ANSI codes
ansi <- readxl::read_excel(
  "data/ansi-state-codes.xlsx",
  .name_repair = setmeup::fix_colnames
)

ansi <- ansi |>
  select(
    fips = fips_state_numeric_code,
    abbr = official_usps_code
  ) |>
  drop_na()

ansi <- setNames(ansi$abbr, ansi$fips)

saveRDS(ansi, "data/ansi_state_codes.rds")

