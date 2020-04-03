#' Time to event mapping module
#' 
#' Aids in the mapping of variables in a data set to common time-to-event (TTE) analysis variables.
#' Input is any data set (data frame, tibble, etc.)
#'
#' @family tidymodules
#' @format R6 class
#' @importFrom R6 R6Class
#' @export
#' @keywords TidyModule
#' @section Input:
#' \describe{
#'   \item{\code{data}}{A data set (data frame, tibble, etc.) for the TTE variable mapping}
#' }
#' @section Output:
#' \describe{
#'   \item{\code{mapping}}{a reactive list with values
#'   \describe{
#'     \item{population}{}
#'     \item{time}{}
#'     \item{event}{}
#'     \item{strata}{}
#'     \item{treatment}{}
#'     \item{parameter}{}
#'     \item{parameter_value}{}
#'   }}
#' }
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' library(subpat)
#' library(tidymodules)
#' 
#' tteMappingModule <- TTEMapping$new()
#' tteModule <- TTE$new()
#' 
#' ui <- fluidPage(
#'   titlePanel("TTE Analysis"),
#'   
#'   sidebarLayout(
#'     sidebarPanel(
#'       selectInput('dataset', 'survival dataset', choices = data(package = "survival")$results[, "Item"], selected = "lung"),
#'       tteMappingModule$ui()
#'     ),
#'     mainPanel(
#'       # Use the base shiny UI
#'       tteModule$standardUi()
#'     )
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   options <- reactiveValues(
#'     makePlotly = FALSE,
#'     conftype = "log-log"
#'   )
#'   
#'   optionsMapping <- reactiveValues(
#'     population = FALSE,
#'     parameter = FALSE,
#'     parameter_value = FALSE,
#'     adam = FALSE
#'   )
#'   
#'   tteMappingModule$callModule()
#'   tteModule$callModule()
#'   
#'   # Load the data set from the survival package
#'   data_reactive <- reactive({
#'     req(input$dataset)
#'     ds <- trimws(gsub("\\(.*\\)", "", input$dataset))
#'     data(list = ds, package = "survival")
#'     
#'     # Reset the modules
#'     tteMappingModule <- TTEMapping$new()
#'     tteModule <- TTE$new()
#'     tteMappingModule$callModule()
#'     tteModule$callModule()
#'     
#'     
#'     get(ds)
#'   })
#'   
#'   observe({
#'     options %>4% tteModule
#'     optionsMapping %>2% tteMappingModule
#'     data_reactive %>1% tteModule
#'     # Get the mapping and pass into the TTE module
#'     data_reactive %>1% tteMappingModule %1>2% tteModule
#'   })
#' }
#' 
#' # Run the application
#' shinyApp(ui = ui, server = server)
#' }
TTEMapping <- R6Class(
  "TTEMapping",
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...){
      # Mandatory
      super$initialize(...)
      
      self$definePort({
        # At least one input
        self$addInputPort(
          name = "data",
          description = "A data set (data frame, tibble, etc.) for the TTE variable mapping",
          sample = head(cars),
          input = NULL)
        
        self$addInputPort(
          name = "options",
          description = "list with options:
          - makePlotly (boolean) to indicate whether the graph should be made interactive via plotly
          - conftype ('log-log' [default], 'log', or 'plain') the KM confidence interval type",
          sample = list(
            population = TRUE,
            time = TRUE,
            event = TRUE,
            strata = TRUE,
            treatment = TRUE,
            parameter = TRUE,
            parameter_value = TRUE
          ),
          input = NULL)
        
        self$addOutputPort(
          name = "mapping",
          description = "The variable mapping",
          sample = list(
            time = 'AVAL',
            event = 'EVENT',
            treatment = 'TRT01P',
            strata = c('STRVAL1', 'STRVAL2'),
            parameter = 'PARAM',
            parameter_value = 'Progression free Survival',
            population = "FASFL",
            aval_conv = round(30.4368499, 3)
          ),
          output = NULL)
      })
    },
    ui = function() {
      tagList(
        checkboxInput(self$ns('showLabels'), label='Show the variable labels?', value = T),
        uiOutput(self$ns("selectPopUi")),
        uiOutput(self$ns("selectTimeUi")),
        uiOutput(self$ns("selectEventUi")),
        uiOutput(self$ns("selectTreatmentUi")),
        uiOutput(self$ns("selectParamUi")),
        uiOutput(self$ns("selectParamValUi"))
      )
    },
    server = function(input, output, session){
      # Mandatory
      super$server(input,output,session)
      
      mapping <- reactive({
        o <- self$execInput("options")
        param <- NULL
        param_value <- NULL
        if(is.null(o$parameter) || o$parameter) {
          param <- input$parameter
          param_value <- input$parameter_value
        }
        
        list(
          population = input$population,
          time = input$time,
          event = input$event,
          treatment = input$treatment,
          parameter = param,
          parameter_value = param_value,
          aval_conv = input$aval_conv
        )
      })
      
      cox_mapping <- reactive({
        list(
          strata = input$strata,
          covariates = input$covariates
        )
      })
      
      # Wait for all computations before sending data to module
      doneRendering <- reactiveVal(FALSE)
      local_rv <- reactiveValues(
        isAdam = NULL,
        choices = NULL,
        selected = NULL
      )
      
      data_reactive <- reactive({
        self$execInput("data")
      })
      
      # Configure options for choices, selected values, and whether data set is ADaM
      observe({
        d <- data_reactive()
        o <- self$execInput("options")
        
        # Default to adam data sets
        isAdam <- is.null(o$adam) || o$adam
        
        # Filter some of the variables depending on the variable
        # Place common variables at the top of the list
        local_rv$choices <- list(
          time = numericColNames(d, showLabels = input$showLabels),
          # Character or factor columns
          parameter = private$charFactorColNames(d, showLabels = input$showLabels),
          # Event columns: either 0 or 1, or logical
          # Not supporting interval status codes at this point
          event = private$eventColNames(d, showLabels = input$showLabels),
          # Put all the TRT columns first
          treatment = private$treatmentColName(d, isAdam, limit = 5, showLabels = input$showLabels),
          # Only show Population Indicator Variables
          population = private$populationIndicatorColNames(d, isAdam, showLabels = input$showLabels)
        )
        
        local_rv$selected <- list(
          time = ifelse("AVAL" %in% names(d), "AVAL", ""),
          event = ifelse("EVENT" %in% names(d), "EVENT", ""),
          treatment = ifelse("TRTP" %in% names(d), "TRTP", ""),
          parameter = ifelse("PARAM" %in% names(d), "PARAM", ""),
          population = ifelse("FASFL" %in% names(d), "FASFL", "")
        )
      })
      
      observeEvent({
        input$parameter
        self$execInput("options")$parameter
      }, {
        req(input$parameter)
        
        param_values <- unique(data_reactive()[[input$parameter]])
        
        overall_surv_params <- param_values[grepl("overall surviv(e|al)", param_values, ignore.case = TRUE)]
        # Find the survival and response parameters
        surv_params <- param_values[grepl("surviv(e|al)", param_values, ignore.case = TRUE)]
        # Time to and duration of parameters
        timeto_params <- param_values[grepl("(time to|duration of)", param_values, ignore.case = TRUE)]
        
        # Put the Survival parameters first, then the time to/duration of parameters, then the rest of the parameters
        # Another (better?) approach would be to remove all of the values where event is all NA
        param_values <- c(overall_surv_params, surv_params, timeto_params)
        
        updateSelectInput(session, inputId = 'parameter_value', choices = param_values, selected = param_values[[1]])
        
        doneRendering <- TRUE
      })
      
      output$selectPopUi <- renderUI({
        req(local_rv$choices, local_rv$selected)
        o <- self$execInput("options")
        if(is.null(o$population) || o$population) {
          selectInput(self$ns('population'), 'Population', choices = local_rv$choices$population, selected = local_rv$selected$population)
        }
      })
      
      output$selectTimeUi <- renderUI({
        req(local_rv$choices, local_rv$selected)
        o <- self$execInput("options")
        tagList(
          selectInput(self$ns('time'), 'Time Variable', choices = local_rv$choices$time, selected = local_rv$selected$time),
          numericInput(self$ns('aval_conv'), 'Time conversion factor (e.g. 30.4 for months, 1= dataset value)', value = round(30.4368499, 1), min = 1)
        )
      })
      
      output$selectEventUi <- renderUI({
        req(local_rv$choices, local_rv$selected)
        selectInput(self$ns('event'), 'Event Variable', choices = local_rv$choices$event, selected = local_rv$selected$event)
      })
      
      output$selectTreatmentUi <- renderUI({
        req(local_rv$choices, local_rv$selected)
        o <- self$execInput("options")
        if(is.null(o$treatment) || o$treatment) {
          selectInput(self$ns('treatment'), 'Treatment Variable', choices = local_rv$choices$treatment, selected = local_rv$selected$treatment)
        }
      })
      
      output$selectParamUi <- renderUI({
        req(local_rv$choices, local_rv$selected)
        o <- self$execInput("options")
        if(is.null(o$parameter) || o$parameter) {
          selectInput(self$ns('parameter'), 'Parameter Variable', choices = local_rv$choices$parameter, selected = local_rv$selected$parameter)
        }
      })
      
      output$selectParamValUi <- renderUI({
        req(local_rv$choices, local_rv$selected, input$parameter)
        o <- self$execInput("options")
        
        if((is.null(o$parameter) || o$parameter) && (is.null(o$parameter_value) || o$parameter_value)) {
          param_values <- unique(data_reactive()[[input$parameter]])
          
          overall_surv_params <- param_values[grepl("overall surviv(e|al)", param_values, ignore.case = TRUE)]
          # Find the survival and response parameters
          surv_params <- param_values[grepl("surviv(e|al)", param_values, ignore.case = TRUE)]
          # Time to and duration of parameters
          timeto_params <- param_values[grepl("(time to|duration of)", param_values, ignore.case = TRUE)]
          
          # Put the Survival parameters first, then the time to/duration of parameters, then the rest of the parameters
          # Another (better?) approach would be to remove all of the values where event is all NA
          param_values <- c(overall_surv_params, surv_params, timeto_params)
          
          # Will update choices once parameter is selected
          selectInput(self$ns('parameter_value'), 'Select Parameter', choices = param_values, selected = param_values[[1]])
        }
      })
      
      # Outputs
      self$assignPort({
        
        self$updateOutputPort(
          id = "mapping",
          output = mapping)
      })
    }
),
private = list(
  # Returns factors or character column names
  charFactorColNames = function(d, showLabels = TRUE) {
    if(showLabels) {
      res <- columnLabelsInvert(d)
    } else {
      res <- colnames(d)
    }
    
    cond <- unlist(lapply(d, function(x) {
      is.character(x) || is.factor(x)
    }))
    
    res[cond]
  },
  
  # Finds the columns where all the values are 0 or 1 (or it is logical)
  eventColNames = function(d, showLabels = TRUE) {
    if(showLabels) {
      res <- columnLabelsInvert(d)
    } else {
      res <- colnames(d)
    }
    
    cond <- unlist(lapply(d, function(x) {
      # Allow logical, 0/1 with NAs but not columns that only contain NAs
      is.logical(x) || (all(x  == 0 | x == 1 | x == 2, na.rm = TRUE) && !all(is.na(x)))
    }))
    
    res[cond]
  },
  # Finds the population indicator variables
  # From the ADaM documentation (pg 23 Table 3.2.3 of ADaMIG_v1.1.pdf)
  # "This list of flags is not meant to be all-inclusive. Additional population flags may be added."
  populationIndicatorColNames = function(d, is_adam = TRUE, showLabels = TRUE) {
    if(showLabels) {
      res <- columnLabelsInvert(d)
    } else {
      res <- colnames(d)
    }
    
    if(! is_adam) {
      return(res)
    }
    
    popVars <- c("FASFL",
                 "SAFFL",
                 "ITTFL",
                 "PPROTFL",
                 "COMPLFL",
                 "RANDFL",
                 "ENRLFL")
    
    cond <- names(d) %in% popVars
    if(all(!cond)) {
      warning("No population indicator variables found in data set.")
      res
    } else {
      res[cond]
    }
  },
  
  treatmentColName = function(d, is_adam = TRUE, limit = 5, showLabels = TRUE) {
    if(showLabels) {
      res <- columnLabelsInvert(d)
    } else {
      res <- colnames(d)
    }
    
    if(is_adam) {
      cond <- grepl("^TRT", res)
    } else {
      cond <- unlist(lapply(d, function(x) {
        length(unique(x)) <= limit
      }))
    }
    Filter(Negate(is.na), res[cond])
  }
)
)
