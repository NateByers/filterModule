#' A shiny module for automatically generating filter widgets for a table.
#' @description A module to be called in the server portion of a shiny app.
#' @import shiny dplyr
#' @export
#' @examples
#'# main ui
#'ui <- fixedPage(
#'
#'  filterModuleOuput("id1"),
#'
#'  DT::dataTableOutput("table")
#'
#')
#'
#'# main server
#'server <- function(input, output, session) {
#'
#'  data <- iris
#'  filter_data <- callModule(filterModule, "id1", data = data)
#'
#'  output$table <- DT::renderDataTable({
#'    filter_data()
#'  })
#'
#'}
#'
#'# run app
#'shinyApp(ui, server)
filterModule <- function(input, output, session, data) {
  ns <- session$ns

  filters <- lapply(names(data), function(name, data) {
    if(class(data[[name]]) %in% c("numeric", "integer")) {
      widget <- "slider"
      value <- c(min(data[[name]], na.rm = TRUE),
                 max(data[[name]], na.rm = TRUE))
    } else if(class(data[[name]]) %in% c("character", "factor")) {
      widget <- "text"
      value <- ""
    }
    list(widget = widget, value = value)
  }, data = data)
  names(filters) <- names(data)

  # dynamic controls
  output$dynamic_filters <- renderUI({
    tagList(
      lapply(names(filters), function(name, filters, data) {
        if(filters[[name]]$widget == "slider") {
          min <- filters[[name]]$value[1]
          max <- filters[[name]]$value[2]
          sliderInput(ns(name), name, min = min, max = max, value = c(min, max))
        } else if(filters[[name]]$widget == "text") {
          selectInput(ns(name), name,
                      choices = sort(unique(data[[name]])),
                      multiple = TRUE)
        }

      }, filters = filters, data = data)
    )
  })

  filter_data_frame <- reactive({
    data_ <- data
    for(i in names(data)) {
      value <- input[[i]]
      print(value)
      if(class(data[[i]]) %in% c("numeric", "integer")) {
        data_ <- data_[data_[[i]] >= value[1] & data_[[i]] <= value[2], ]
      } else if(class(data[[i]]) %in% c("character", "factor") & !is.null(value[1])) {
        data_ <- data_[data_[[i]] == value[1], ]
      }
    }
    data_
  })

  filter_data_frame
}

#' Filter output for a shiny app.
#' @import shiny
#' @export
#' @describeIn filterModule The output for the ui--call where you want the filter widgets.
filterModuleOuput <- function(id){

  ns <- NS(id)

  tagList(
    uiOutput(ns("dynamic_filters")
    )
  )
}
