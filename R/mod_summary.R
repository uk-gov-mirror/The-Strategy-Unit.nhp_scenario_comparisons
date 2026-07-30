mod_summary_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::uiOutput(ns("filters_ui")),
    shiny::plotOutput(ns("plot"))
  )
}

mod_summary_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- shiny::reactive(processed_data()$summary_data)

    output$filters_ui <- shiny::renderUI({
      shiny::req(df())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter"),
            "Activity Type",
            choices = pull_unique(df(), "activity_type_label")
          )
        )
      )
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(df(), input$filter)
        create_summary_bar_chart(df(), input$filter1)
      },
      res = 100,
    )
  })
}
