mod_los_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_los_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- shiny::reactive(processed_data()$los_data)
    cond_apm_lookup <- shiny::reactive(processed_data()$cond_apm_lookup)

    output$filters_ui <- shiny::renderUI({
      shiny::req(df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter1"),
            "Point of Delivery",
            choices = pull_unique(df(), "pod_name")
          ),
          shiny::selectInput(ns("filter2"), "Measure", choices = NULL)
        )
      )
    })

    shiny::observe({
      shiny::req(df(), cond_apm_lookup(), input$filter1)

      label_lookup <- cond_apm_lookup() |>
        dplyr::mutate(
          measure_label = create_measure_label(.data[["measure"]])
        ) |>
        dplyr::filter(.data[["pod_label"]] == input$filter1)

      filter2_choices <- pull_unique(label_lookup, "measure_label")

      shiny::updateSelectInput(inputId = "filter2", choices = filter2_choices)
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(df(), input$filter1, input$filter2)
        create_los_bar_chart(df(), input$filter1, input$filter2)
      },
      res = 100,
    )
  })
}
