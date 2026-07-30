mod_p10_p90_bar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::includeMarkdown("inst/app/p10-p90-text.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_p10_p90_bar_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- shiny::reactive(processed_data()$principal_pi_data)
    full_apm_lookup <- shiny::reactive(processed_data()$full_apm_lookup)

    output$filters_ui <- shiny::renderUI({
      shiny::req(df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter1"),
            "Activity type",
            choices = c("Inpatients", "Outpatients", "A&E")
          ),
          shiny::selectInput(ns("filter2"), "Point of Delivery", choices = NULL)
        )
      )
    })

    shiny::observe({
      shiny::req(df(), full_apm_lookup(), input$filter1)

      label_lookup <- full_apm_lookup() |>
        dplyr::mutate(
          dplyr::across("activity_type_label", \(x) sub("s$", "", x))
        ) |>
        dplyr::filter(.data[["activity_type_label"]] == input$filter1)

      filter2_choices <- pull_unique(label_lookup, "pod_label")

      shiny::updateSelectInput(inputId = "filter2", choices = filter2_choices)
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(df(), input$filter1, input$filter2)
        create_principal_pi_bar_chart(df(), input$filter1, input$filter2)
      },
      res = 100,
    )
  })
}
