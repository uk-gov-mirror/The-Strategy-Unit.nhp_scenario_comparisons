mod_p10_p90_bar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::includeMarkdown("inst/app/p10-p90-text.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_p10_p90_bar_server <- function(id, processed) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- shiny::reactive(processed()$data_distribution_summary)

    output$filters_ui <- shiny::renderUI({
      shiny::req(df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("category"),
            "Activity Type",
            choices = names(pod_categories)
          ),
          shiny::selectInput(
            ns("filter1"),
            "Point of Delivery",
            choices = NULL # will be filled dynamically
          )
        )
      )
    })

    shiny::observeEvent(input$category, {
      shiny::req(input$category, df())

      available <- pull_unique(df(), "pod_label")

      shiny::updateSelectInput(
        session,
        "filter1",
        choices = available
      )
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(df(), input$filter1)
        create_distribution_bar_chart(df(), input$filter1)
      },
      res = 100,
    )
  })
}
