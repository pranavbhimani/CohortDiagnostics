createReport <- function(params,
                         rmdPath) {

}


#' Report module UI
#' @description
#' Create protocol document from cohort diagnostics data.
#' Select components from available data to include in report
#' @param id        Namespace id for shiny module. Must match reportModule server
reportModuleUi <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::column(
      width = 12,
      shinydashboard::box(
        width = "100%",
        shiny::tags$h2("Cohort Diagnostics Report"),
        shiny::p("Create a report including specified cohorts and included information"),
        shiny::textInput(ns("title"), "Title"),
        shiny::textInput(ns("authors"), "Authors"),
        shiny::textAreaInput(ns("reportDescription"), "Project Description"),
        shiny::h3("Cohort selection"),
        reactable::reactableOutput(outputId = ns("cohortDefinitionTable")),
        shiny::h3("Database selection"),
        reactable::reactableOutput(outputId = ns("databaseTable")),

        shiny::h3("Included Sections"),
        shiny::p("Specify sections to be included in output:"),
        reactable::reactableOutput(outputId = ns("includedSections")),

        shinyWidgets::pickerInput(
          inputId = ns("outputFormat"),
          label = "Output Format",
          choices = c("MS Word (Docx)" = "word_document",
                      "PDF" = "pdf_document",
                      "HTML" = "html_document"),
          multiple = FALSE,
          choicesOpt = list(style = rep_len("color: black;", 999)),
          options = shinyWidgets::pickerOptions(
            size = 5,
            virtualScroll = 50
          )
        ),

        shiny::p("Depending on options selected report generation may take some time.
        This page will not be usable during this period."),
        shiny::downloadButton(ns("render"), "Generate Report"),
      )
    )
  )
}

#' Report module
#' @description
#' Create protocol document from cohort diagnostics data.
#' Uses Rmarkdown to create output
#' @param id        Namespace id for shiny module. Must match reportModuleUi
reportModule <- function(id,
                         dataSource,
                         cohortTable,
                         databaseTable,
                         enabledTabs) {
  ns <- shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {

    getCohortTable <- shiny::reactive({
      cohortTable %>%
        dplyr::select(cohort = .data$shortName, .data$cohortId, .data$cohortName) %>%
        dplyr::mutate(cohortId = as.character(.data$cohortId))
    })
    output$cohortDefinitionTable <-
      reactable::renderReactable(expr = {
        data <- getCohortTable()

        validate(need(hasData(data), "There is no data for this cohort."))
        keyColumns <- c("cohort", "cohortId", "cohortName")
        dataColumns <- c()

        displayTable <- getDisplayTableSimple(
          data = data,
          keyColumns = keyColumns,
          dataColumns = dataColumns,
          selection = "multiple",
          defaultSelected = 1:nrow(data)
        )
        return(displayTable)
      })

    output$databaseTable <-
      reactable::renderReactable(expr = {
        data <- databaseTable %>%
          dplyr::select(.data$databaseId, .data$databaseName, .data$description) %>%
          unique()

        keyColumns <- c("databaseId", "databaseName", "description")

        displayTable <- getDisplayTableSimple(
          data = data,
          keyColumns = keyColumns,
          dataColumns = c(),
          selection = "multiple",
          defaultSelected = 1:nrow(data)
        )
        return(displayTable)
      })

    getAvailableSections <- shiny::reactive({
      availableSection <- tibble::tibble(
        section = c("Project Description",
                    "Cohort Definitions",
                    "Concept Set Definitions",
                    "Time Distributions",
                    "Cohort Counts",
                    "Incidence",
                    "Overlap",
                    "Characterization"),
        description = c("Above description box contents",
                        "Full, human readable definitions of cohorts",
                        "Cohort concept set tables",
                        "Distributions of ",
                        "Counts",
                        "Incidence Proportions For cohorts",
                        "Overlap between selected cohorts",
                        "Cohort Characterization table"),
        tabId = c("projectDescriptions",
                  "cohort",
                  "cohort",
                  "temporalCovariateValue",
                  "cohortCount",
                  "incidenceRate",
                  "relationship",
                  "temporalCovariateValue"),
      ) %>%
        dplyr::filter(.data$tabId %in% c(enabledTabs, "projectDescriptions"))
      return(availableSection)
    })

    output$includedSections <-
      reactable::renderReactable(expr = {
        data <- getAvailableSections()

        data <- data %>%
          dplyr::select(.data$section, .data$description)

        keyColumns <- names(data)
        displayTable <- getDisplayTableSimple(
          data = data,
          keyColumns = keyColumns,
          dataColumns = c(),
          selection = "multiple",
          defaultSelected = 1:nrow(data)
        )
        return(displayTable)
      })


    reportOptions <- shiny::reactive({
                                          #' Get selected reactable indexes
      selectedCohorts <- reactable::getReactableState("cohortDefinitionTable")$selected
      selectedDatabases <- reactable::getReactableState("databaseTable")$selected
      selectedSections <- reactable::getReactableState("includedSections")$selected

      return(
        list(
          outputType = input$outputFormat,
          author = input$authors,
          title = input$title,
          reportDescription = input$reportDescription,
          sections = getAvailableSections()[selectedSections,],
          cohortsSelected = getCohortTable()[selectedCohorts,],
          databasesSelected = databaseTable[selectedDatabases,]
        )
      )
    })

    output$render <- shiny::downloadHandler(
      filename = function() {
        paste('cohort-diagnostics-report', sep = '.', switch(
          input$outputFormat,
          pdf_document = 'pdf', html_document = 'html', word_document = 'docx'
        ))
      },

      content = function(file) {
        src <- normalizePath('markdown/main.Rmd')
        params <- reportOptions()

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        rmarkdown::render(src,
                          output_format = switch(
                            input$outputFormat,
                            pdf_document = rmarkdown::pdf_document(toc = TRUE, toc_depth = 2),
                            html_document = rmarkdown::html_document(toc = TRUE, toc_depth = 2),
                            word_document = rmarkdown::word_document(toc = TRUE, toc_depth = 2)
                          ),
                          output_file = file,
                          envir = parent.frame(),
                          params = params)
      }
    )
  })
}



