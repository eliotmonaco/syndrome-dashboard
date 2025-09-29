




df <- ts |>
  filter(
    syndrome %in% c("cov", "ili", "rsv"),
    date %in% seq.Date(as.Date("2025-09-01"), Sys.Date())
  )

df |>
  syn_highchart(alert = TRUE)

df |>
  syn_ggplot(alert = FALSE)













url <- paste0(
  # "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?",
  "https://moessence.inductivehealth.com/ih_essence/api/timeSeries?",
  "startDate=01Jan25&",
  "endDate=31Aug25&",
  "percentParam=ccddCategory&",
  "datasource=va_hosp&",
  "medicalGroupingSystem=essencesyndromes&",
  "userId=5809&",
  "aqtTarget=TimeSeries&",
  "ccddCategory=cli%20cc%20with%20cli%20dd%20and%20coronavirus%20dd%20v2&",
  # "geographySystem=hospitalstate&",
  "geographySystem=hospital",
  "geography=mochildrensmercyercc",
  "detector=probregv2&",
  "timeResolution=daily&",
  "hasBeenE=1&",
  "stratVal=&",
  "multiStratVal=geography&",
  "graphOnly=true&",
  "numSeries=0&",
  "graphOptions=multipleSmall&",
  "seriesPerYear=false&",
  "nonZeroComposite=false&",
  "removeZeroSeries=true&",
  "sigDigits=true&",
  "startMonth=January&",
  "stratVal=&",
  "multiStratVal=geography&",
  "graphOnly=true&",
  "numSeries=0&",
  "graphOptions=multipleSmall&",
  "seriesPerYear=false&",
  "startMonth=January&",
  "nonZeroComposite=false"
)

url <- gsub("\n", "", url)

df <- get_api_data(url) |>
  pluck("timeSeriesData")





