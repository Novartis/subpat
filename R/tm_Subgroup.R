library(shiny)
library(ggplot2)
library(R6)

#' Subgroup TidyModule
#' 
#' Subgroup creates derived variables based on numeric variables. This is generally not used directly and is dynamically called from \code{\link{SubgroupManager}}
#'
#' @family tidymodules
#' @format R6 class
#' @importFrom R6 R6Class
#' @export
#' @keywords TidyModule
#' @section Input:
#' \describe{
#'   \item{\code{data}}{A data set (data frame, tibble, etc.) to create subgroups from.}
#'   \item{\code{mapping}}{A mapping (reactive list) for 'parameter', and 'parameter_value' variables \describe{
#'     \item{\code{parameter}}{The parameter values (PARAM). If this is provided then \code{parameter_value} must be provided as well. Will only perform the analysis where \code{parameter == parameter_value}}
#'     \item{\code{parameter_value}}{See the description for \code{parameter}}
#'   }}
#'   \item{\code{options}}{
#'     The options for the module of type \code{\link[shiny]{reactiveValues}} with values \describe{
#'     \item{\code{index}}{The index of the subgroup that we are creating. If given, will include as the default name for the population "New subgroup: \code{populationNumber}"}
#'     }
#'   }
#' }
#' @section Output:
#' \describe{
#' \item{\code{subgroup}}{Factor vector of the subgroup}
#' \item{\code{subgroup_name}}{name of the created subgroup}
#' }
Subgroup <- R6Class(
  "Subgroup",
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...){
      # Mandatory
      super$initialize(...)

      self$definePort({
        # At least one input
        self$addInputPort(
          name = "data",
          description = "A data set (data frame, tibble, etc.) to create subgroups from.",
          sample = head(cars))

        self$addInputPort(
          name = "mapping",
          description = "A mapping for 'parameter', and 'parameter_value' variables
          parameter - The parameter values (PARAM). If this is provided then `parameter_value` must be provided as well.
                      Will only perform the analysis where parameter == parameter_value
                      If it is not provided, then all of the data will be used.
          parameter_value - See the description for `parameter`

          If this is not provided, then will not filters the results for the histograms
          ",
          sample = list(
            parameter = 'PARAM',
            parameter_value = 'Progression free Survival'
          ))

        self$addInputPort(
          name = "options",
          description = "reactiveValues with options:
          - index The index number of the subgroup. Used to automatically name with New subgroup: <index>",
          sample = reactiveValues(
            index = 1
          ))

        self$addOutputPort(
          name = "subgroup",
          description = "The subgroup",
          sample = list())

        self$addOutputPort(
          name = "subgroup_name",
          description = "The subgroup name",
          sample = "New subgroup")
      })
    },
    ui = function() {
      tagList(
        box(
          title = textOutput(self$ns('subgroupName'), inline = TRUE),
          width = 12,
          closable = FALSE,
          fluidRow(
            column(
              width = 4,
              uiOutput(self$ns('subgroupNameUi')),
              uiOutput(self$ns('config')),
              textInput(self$ns('cutpoints'), label = "Comma separated cut-points", value = "", placeholder = ""),
              tableOutput(self$ns("partition_summary")),
              actionButton(self$ns('save_subgroup'), label = "Save subgroup"),
              textOutput(self$ns("error_msg"))
            ),
            column(
              width = 8,
              plotOutput(self$ns('variableHist'),
                         click = self$ns("variableHist_click")),
              uiOutput(self$ns('paramTextUi'))
            )
          )
        )
      )
    },
    server = function(input, output, session){
      # Mandatory
      super$server(input,output,session)

      novBlue <- "#4477AA"
      
      data_reactive <- reactive({
        d <- self$execInput("data")
        m <- self$getInput("mapping")
        # The initial value of an input is FALSE
        if(is.logical(m) && !m)
          d
        else{
          m <- m()
          if(is.null(m$parameter)) {
            d
          } else {
            d[d[[m$parameter]] == m$parameter_value, ]
            # d %>% filter(get(m$parameter) == m$parameter_value)
          }
        }
      })

      cutpoints <- reactive({
        req(input$cutpoints)
        # Split the cut-points string at commas (and remove extra spacing around them)
        # Then convert to numeric
        tryCatch(
          as.numeric(unlist(strsplit(input$cutpoints, split = "[[:space:]]*,[[:space:]]*"))),
          error = function(cond) {
            return(NULL)
          }
        )

      })

      var_partition <- reactive({
        req(cutpoints, data_reactive, input$variableSelect)
        # Partition the selected variable based on the cutpoints provided
        tryCatch({
          output$error_msg <<- renderText("")
          partition(data_reactive()[[input$variableSelect]], cutpoints())
        },
          error = function(cond) {
            if(cond$message != "") {
              output$error_msg <<- renderText(paste0("Error: ", toString(cond$message)))
            }

            return(NULL)
          }
        )
      })

      subgroup <- eventReactive(input$save_subgroup, {
        var_partition()
      })
      
      subgroup_name <- eventReactive(input$save_subgroup, {
        gsub("(\\s|:)","_", input$subgroup_name,perl = TRUE)
      })

      observeEvent(input$variableSelect, {
        req(input$variableSelect, data_reactive)

        num_var <- data_reactive()[[input$variableSelect]]
        # Update the placehold with 25, 50, and 75% quantiles
        updateTextInput(self$getShinySession(), 'cutpoints', placeholder = paste0(round(quantile(num_var, probs = c(.25, .50, .75), na.rm = TRUE)), collapse = ", "))
      })

      output$subgroupName <- renderText({
        paste("Subgroup:", input$subgroup_name)
      })

      output$subgroupNameUi <- renderUI({
        o <- self$execInput("options")

        if(!is.null(o$index)) {
          value <- paste0("New subgroup: ", o$index)
        } else {
          value <- "New subgroup"
        }

        textInput(self$ns('subgroup_name'), label = "Subgroup name", value = value)
      })

      output$partition_summary <- renderTable({
        req(var_partition())
        df <- data.frame(table(var_partition()))
        colnames(df) <- c(input$variableSelect, "Count")
        df
      })

      output$paramTextUi <- renderUI({
        m <- self$execInput("mapping")
        if(is.null(m$parameter)) {
          p("Showing entire data set")
        } else {
          p(paste0("Histogram filtering based on paramater: ", m$parameter, " == ", m$parameter_value))
        }
      })

      output$config <- renderUI({
        d <- self$execInput("data")
        numCols <- numericColNames(d)

        selectInput(self$ns('variableSelect'), label = "Select variable", choices = c("", numCols), selected = NULL)
      })

      output$variableHist <- renderPlot({
        req(input$variableSelect, data_reactive())

        g <- ggplot(data = data_reactive()) +
          # Histogram values from the selected numeric variable
          geom_histogram(aes_string(x = input$variableSelect), fill = novBlue, color = "#ffffff") +
          ggtitle(paste0("Histogram for variable: ", input$variableSelect)) +
          theme_minimal(base_size = 15, base_family = "Arial")

        if(!is.null(var_partition()) && length(cutpoints()) > 0) {
          # Lines for the cut values
          g + geom_vline(data = data.frame(xint = cutpoints()), aes(xintercept = xint), alpha = 0.7)
        } else {
          g
        }
      })

      # Ports assignment starts here
      self$assignPort({
        
        self$updateOutputPort(
          id = "subgroup",
          output = subgroup)

        self$updateOutputPort(
          id = "subgroup_name",
          output = subgroup_name)
      })

      return(subgroup)
    }
  )
)


