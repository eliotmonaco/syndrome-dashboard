# ts_plot_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#
#   )
# }

ts_plot_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Filter time series data
    ts <- reactive({
      req(data(), rv$syn, rv$dtrng)
      get_ts_data(data()$ts, rv$syn, rv$dtrng)
    })

    # Time series plot (patient location data)
    output$tspat <- renderHighchart({
      req(ts()$patient, synname())
      ts_plot(
        ts()$patient,
        title = synname()
      )
    })

    # Time series plot (hospital location data)
    output$tshosp <- renderHighchart({
      req(ts()$hospital, synname())
      ts_plot(
        ts()$hospital,
        title = synname()
      )
    })
  })
}

# Syndrome name
synname <- reactive({
  req(synselect())
  names(synselect())[which(synselect() == rv$syn)]
})



# # Data details to tables
# data_characteristics_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#
#   )
# }
#
# data_characteristics_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     # Data details data
#     dd <- reactive({
#       req(rv$date, rv$syn, rv$dtrng)
#
#       dbdata |>
#         get_list_data(rv$date, "dd") |>
#         get_dd_data(rv$syn, rv$dtrng)
#     })
#
#   })
# }


