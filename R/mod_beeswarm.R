mod_beeswarm_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::includeMarkdown("inst/app/probabilistic-model-note.md"),
    shiny::includeMarkdown("inst/app/beeswarm-note.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::checkboxInput(
      ns("show_origin"),
      "Show Origin (zero)?",
      value = TRUE
    ),
    shiny::plotOutput(ns("plot"))
  )
}

mod_beeswarm_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # scn1 <- shiny::reactive(processed()$scenario_1_name)
    # scn2 <- shiny::reactive(processed()$scenario_2_name)
    df <- shiny::reactive(processed_data()$beeswarm_data)
    full_apm_lookup <- shiny::reactive(processed_data()$full_apm_lookup)

    label_lookup <- full_apm_lookup() |>
      dplyr::mutate(measure_label = create_measure_label(.data[["measure"]]))

    output$filters_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter1"),
            "Activity Type",
            choices = pull_unique(label_lookup, "activity_type_label")
          ),
          shiny::selectInput(ns("filter2"), "Measure", choices = NULL)
        )
      )
    })

    shiny::observe({
      shiny::req(input$filter1)

      filter2_choices <- label_lookup |>
        dplyr::filter(.data[["activity_type_label"]] == input$filter1) |>
        pull_unique("measure_label")

      shiny::updateSelectInput(inputId = "filter2", choices = filter2_choices)
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(df(), input$filter1, input$filter2)
        create_beeswarm_chart(df(), input$filter1, input$filter2)
      },
      res = 100
    )
  })
}
