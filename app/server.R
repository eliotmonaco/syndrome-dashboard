function(input, output, session) {

  ts <- reactive({
    req(input$syn, input$date)

    ts0 |>
      filter(
        syndrome == input$syn,
        date >= input$date[1],
        date <= input$date[2]
      )
  })

  output$ts <- renderHighchart({
    req(ts())

    ttl <- names(syn_names)[which(syn_names == input$syn)]

    p <- syn_highchart(ts(), title = ttl)

    p
  })

}
