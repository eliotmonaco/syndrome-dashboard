# Syndrome list to use in `build_url()`

syn <- list(
  # Syndromic
  abd = paste0(
    "medicalGroupingSystem=chiefcomplaintsubsyndromes&",
    "medicalGrouping=abdominalpain"
  ),
  drh = paste0(
    "medicalGroupingSystem=chiefcomplaintsubsyndromes&",
    "medicalGrouping=diarrhea"
  ),
  fev = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "medicalGrouping=fever"
  ),
  gi = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "medicalGrouping=gi"
  ),
  neuro = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "medicalGrouping=neuro"
  ),
  rash = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "medicalGrouping=rash"
  ),
  resp = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "medicalGrouping=resp"
  ),
  vom = paste0(
    "medicalGroupingSystem=chiefcomplaintsubsyndromes&",
    "medicalGrouping=vomiting"
  ),
  # Respiratory
  cov = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "ccddCategory=cdc%20covid-specific%20dd%20v1"
  ),
  ili = paste0(
    "medicalGroupingSystem=chiefcomplaintsubsyndromes&",
    "medicalGrouping=ili"
  ),
  rsv = paste0( # CDC RSV query + croup
    "medicalGroupingSystem=essencesyndromes&",
    "ccddCategoryFreeText=%5E%20RSV%20%5E,or,%5Ebronchiolitis%5E,or,%5Esyncytial%20virus%5E,or,%5Ecroup%5E,or,%20%5E%5B;/%5DB97.4%5E,or,%5E%5B;/%5DB974%5E,or,%20%5E%5B;/%5DJ12.1%5E,or,%5E%5B;/%5DJ121%5E,or,%20%5E%5B;/%5DJ20.5%5E,or,%5E%5B;/%5DJ205%5E,or,%20%5E%5B;/%5DJ21.0%5E,or,%5E%5B;/%5DJ210%5E,or,%20%5E%5B;/%5D466.11%5E,or,%5E%5B;/%5D46611%5B;/%5D%5E,or,%20%5E%5B;/%5D480.1%5E,or,%5E%5B;/%5D4801%5B;/%5D%5E,or,%20%5E%5B;/%5D079.6%5E,or,%5E%5B;/%5D0796%5B;/%5D%5E,or,%20%5E%5B;/%5D55735004%5B;/%5D%5E,or,%20%5E%5B;/%5D408684006%5B;/%5D%5E,or,%20%5E%5B;/%5D195881003%5B;/%5D%5E,or,%20%5E%5B;/%5D10625551000119103%5B;/%5D%5E,or,%20%5E%5B;/%5D57089007%5B;/%5D%5E,or,%20%5E%5B;/%5D195739001%5B;/%5D%5E,or,%20%5E%5B;/%5D79479005%5B;/%5D%5E,or,%20%5E%5B;/%5D195727009%5B;/%5D%5E,or,%20%5E%5B;/%5D72204002%5B;/%5D%5E,or,%20%5E%5B;/%5D31309002%5B;/%5D%5E%09%09"
  ),
  # Bioterrorism
  anth = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "ccddCategory=anthrax%20v1"
  ),
  bot = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "medicalGrouping=bot_like"
  ),
  plg = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "ccddCategory=plague%20v1"
  ),
  smpx = paste0(
    "medicalGroupingSystem=chiefcomplaintsubsyndromes&",
    "medicalGrouping=smallpox"
  ),
  tul = paste0(
    "medicalGroupingSystem=chiefcomplaintsubsyndromes&",
    "medicalGrouping=tularemia"
  ),
  vhf = paste0(
    "medicalGroupingSystem=essencesyndromes&",
    "medicalGrouping=hemr_ill"
  )
)

