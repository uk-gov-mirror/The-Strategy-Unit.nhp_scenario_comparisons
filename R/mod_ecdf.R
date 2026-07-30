mod_ecdf_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::verbatimTextOutput(ns("debug")),
    shiny::includeMarkdown("inst/app/probabilistic-model-note.md"),
    shiny::includeMarkdown("inst/app/s-curve-note.md"),
    shiny::uiOutput(ns("filters_ui")),
    shiny::checkboxInput(
      ns("show_origin"),
      "Show Origin (zero)?",
      value = TRUE
    ),
    shiny::plotOutput(ns("plot"))
  )
}

mod_ecdf_server <- function(id, processed_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    label_lookup <- shiny::reactive({
      get_apm_lookup() |>
        dplyr::mutate(dplyr::across("measure", \(x) {
          uppercase_init(sub("dd", "d D", sub("_", "-", x)))
        }))
    })

    df <- shiny::reactive(processed_data()$principal_pi_data)
    full_apm_lookup <- shiny::reactive(processed_data()$full_apm_lookup)

    output$filters_ui <- shiny::renderUI({
      shiny::req(processed_data())

      shiny::tagList(
        shiny::tags$div(
          style = "display: flex; gap: 15px;",
          shiny::selectInput(
            ns("filter1"),
            "Activity Type",
            choices = pull_unique(label_lookup(), "activity_type_label")
          ),
          shiny::selectInput(ns("filter2"), "Measure", choices = NULL)
        )
      )
    })

    shiny::observe({
      shiny::req(processed(), input$filter1)

      filter2_choices <- label_lookup() |>
        dplyr::filter(.data[["activity_type_label"]] == input$filter1) |>
        pull_unique("measure")

      shiny::updateSelectInput(inputId = "filter2", choices = filter2_choices)
    })

    output$plot <- shiny::renderPlot(
      {
        shiny::req(processed(), input$filter1, input$filter2)

        result_1 <- if (input$filter1 == "Inpatients") {
          if (input$filter2 == "Admissions") {
            beeswarm_data1a()
          } else {
            beeswarm_data1b()
          }
        } else if (input$filter1 == "Outpatients") {
          if (input$filter2 == "Attendances") {
            beeswarm_data1c()
          } else {
            beeswarm_data1d()
          }
        } else {
          if (input$filter2 == "Walk-in") {
            beeswarm_data1e()
          } else {
            beeswarm_data1f()
          }
        }
        result_2 <- if (input$filter1 == "Inpatients") {
          if (input$filter2 == "Admissions") {
            beeswarm_data2a()
          } else {
            beeswarm_data2b()
          }
        } else if (input$filter1 == "Outpatients") {
          if (input$filter2 == "Attendances") {
            beeswarm_data2c()
          } else {
            beeswarm_data2d()
          }
        } else {
          if (input$filter2 == "Walk-in") {
            beeswarm_data2e()
          } else {
            beeswarm_data2f()
          }
        }

        # Require that at least one result has data
        shiny::req(any(c(nrow(result_1), nrow(result_2)) > 0))

        combined_dist <- dplyr::bind_rows(
          add_scenario_safe(result_1, scn1()),
          add_scenario_safe(result_2, scn2())
        )

        # Require combined_dist has rows
        shiny::req(nrow(combined_dist) > 0)

        mod_distribution_ecdf_plot(combined_dist) +
          ggplot2::ggtitle(
            glue::glue(
              "{input$filter1} {input$filter2} - S-curve ",
              "(empirical cumulative distribution function)"
            )
          )
      },
      res = 100,
    )
  })
}
