#' Time to event (TTE) module 
#'
#' Shiny \code{\link[tidymodules]{TidyModule}} to perform exploratory survival analysis.
#' Features Kaplan Meier survival function estimation and event summaries as well as Cox Proportional Hazard models.
#'
#' @family tidymodules
#' @format R6 class
#' @importFrom R6 R6Class
#' @import dplyr tidymodules survival
#' @importFrom scales percent
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
TTE <- R6Class(
  "TTE",
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...){
      # Mandatory
      super$initialize(...)
      
      # Ports definition starts here
      self$definePort({
        self$addInputPort(
          name = "data",
          description = "Any data frame with 'time', 'event' and 'strata' variables",
          sample = data.frame(a = 1:4, b = 1:4),
          input = NULL)
        
        self$addInputPort(
          name = "mapping",
          description = "A mapping for 'time', 'event', 'treatment', 'strata', 'parameter', and 'parameter_value' variables
          time - The time variable name (AVAL)
          population - The population flag (Y/N): FASFL, SAFFL, ITTFL, etc. See the ADaM specs for full list.
          event - The event variable (EVENT)
          treatment - The treatment variable name (TRT01P)
          strata - Additional variables c('STRVAL1', 'STRVAL2') to stratify on (not required.)
          parameter - The parameter values (PARAM). If this is provided then `parameter_value` must be provided as well.
          Will only perform the analysis where parameter == parameter_value
          If it is not provided, then all of the data will be used.
          parameter_value - See the description for `parameter`
          ",
          sample = list(
            time = 'AVAL',
            event = 'EVENT',
            treatment = 'TRT01P',
            strata = c('STRVAL1', 'STRVAL2'),
            parameter = 'PARAM',
            parameter_value = 'Progression free Survival',
            population = "FASFL"
          ),
          input = NULL)
        
        self$addInputPort(
          name = "subgroups",
          description = "Any data frame with subgroups",
          sample = data.frame(a = 1:4, b = 1:4),
          input = NULL)
        
        self$addInputPort(
          name = "options",
          description = "list with options:
          - makePlotly (boolean) to indicate whether the graph should be made interactive via plotly
          - conftype ('log-log' [default], 'log', or 'plain') the KM confidence interval type",
          sample = list(),
          input = NULL)
      })
      
      
    },
    coxUi = function() {
      tagList(
        self$coxVariablesUi(),
        uiOutput(self$ns("paramType")),
        box(
          title = "Forest Plot",
          closable = FALSE,
          self$coxForestUi()
        ),
        tableOutput(self$ns("coxCoefficients"))
      )
    },
    coxForestUi = function() {
      plotOutput(self$ns("coxForestPlot"))
    },
    coxVariablesUi = function() {
      fluidRow(
        column(
          width = 4,
          uiOutput(self$ns("selectStrataUi"))
        ),
        column(
          width = 4,
          uiOutput(self$ns("selectCovariatesUi")),
          uiOutput(self$ns("selectSubgroupsUi"))
        )
      )
    },
    plotUi = function() {
      uiOutput(self$ns("kmPlot"))
    },
    eventTableUi = function() {
      tagList(
        h2("Summary of Events and KM Quartiles"),
        tableOutput(self$ns('kmEventSummary')),
        h2("KM Events at selected timepoints"),
        tableOutput(self$ns('kmEventTables'))
      )
    },
    # UI without bs4Dash or AVA
    standardUi = function() {
      tagList(
        div(
          self$plotUi(),
          style = "max-width: 850px;"
        ),
        self$eventTableUi() 
      )
    },
    ui = function() {
      tagList(
        box(
          title = "Kaplan-Meier plot",
          width = 10,
          closable = FALSE,
          self$plotUi()
        ),
        verbatimTextOutput(self$ns("survFormula")),
        self$eventTableUi()
      )
    },
    server = function(input, output, session){
      super$server(input,output,session)
      
      data_react <- reactive({
        d <- self$execInput("data")
        
        m <- mapping()
        
        # The population value should be Y/N according to ADaM specs
        if(!is.null(m$population)) {
          d <- d %>% dplyr::filter(get(m$population) == 'Y')
        }
        
        if(! is.null(m$parameter)) {
          req(m$parameter_value)
          res <- d %>% filter(get(m$parameter) == m$parameter_value)
          # If we did not select any rows, return NULL
          if(nrow(res) == 0) NULL
          # Otherwise return the results
          else res
        } else {
          # No parameter value, return the entire data set
          d
        }
      })
      
      data_with_subgroups <- reactive({
        d <- self$execInput("data")
        subg <- self$execInput("subgroups")
        subg <- as.data.frame(subg)
        
        if(!is.null(subg) && nrow(subg) == nrow(d)) {
          d <- cbind(d, subg)
        }
        
        m <- mapping()
        
        # The population value should be Y/N according to ADaM specs
        if(!is.null(m$population)) {
          d <- d %>% dplyr::filter(get(m$population) == 'Y')
        }
        
        if(! is.null(m$parameter)) {
          req(m$parameter_value)
          res <- d %>% filter(get(m$parameter) == m$parameter_value)
          # If we did not select any rows, return NULL
          if(nrow(res) == 0) NULL
          # Otherwise return the results
          else res
        } else {
          # No parameter value, return the entire data set
          d
        }
      })
      
      mapping <- reactive({
        self$execInput("mapping")
      })
      
      covariateFormulaStr <- reactive({
        # Combine subgroups and covariates
        covar_subgroups <- c(input$covariates, input$subgroups)
        if(!is.null(covar_subgroups)) paste0(covar_subgroups, collapse = " + ")
      })
      
      # Create the strata portion of the formula string
      strataFormulaStr <- reactive({
        if(!is.null(input$strata)) paste0(paste0("strata(", input$strata, ")", sep = ""), collapse = " + ")
      })
      
      # Gets the formula string without strata variables
      getFormulaStr <- reactive({
        req(data_react)
        m <- mapping()
        req(m$time, m$event, m$treatment)
        
        if(!is.null(m$aval_conv)) {
          time_str <- paste0(m$time, "/", m$aval_conv)
        } else {
          time_str <- m$time
        }
        
        paste0("Surv(", time_str,  ",", m$event, ")~",  m$treatment)
      })
      
      # Get the full formula including strata variables
      coxFormulaStr <- reactive({
        rtn <- getFormulaStr()
        if(!is.null(covariateFormulaStr())) rtn <- paste(rtn, covariateFormulaStr(), sep = " + ")
        if(!is.null(strataFormulaStr())) rtn <- paste(rtn, strataFormulaStr(), sep = " + ")
        rtn
      })
      
      # confidence interval type
      # options are (default) 'log-log' (default in SAS)
      # 'log' (default in survival package)
      # 'plain'
      conftype <- reactive({
        o <- self$execInput("options")
        o$conftype %||% 'log-log'
      })
      
      survModel <- reactive({
        req(data_react())
        m <- mapping()
        model <- survival::survfit(as.formula(getFormulaStr()), data=data_react(), conf.type = conftype())
        model
      })
      
      survDiffModel <- reactive({
        survival::survdiff(as.formula(getFormulaStr()), data = data_react())
      })
      
      coxphModel <- reactive({
        survival::coxph(as.formula(coxFormulaStr()), data = data_with_subgroups())
      })
      
      plotTitle <- reactive({
        if(!is.null(mapping()$parameter_value)) {
          paste0("Kaplan-Meier Curve: ", mapping()$parameter_value)
        } else {
          "Kaplan-Meier Curve"
        }
      })
      
      p <- reactive({
        GGally::ggsurv(survModel(), main = plotTitle(), back.white = TRUE) +
          ggplot2::scale_y_continuous(labels = scales::percent, limits=c(0,1))
      })
      
      output$kmPlot1 <- renderPlot({
        req(p)
        p() 
      })
      
      output$kmPlot2 <- plotly::renderPlotly({
        req(p)
        # If we have plotly, use it
        plotly::ggplotly(p())
      })
      
      output$survFormula <- renderPrint({
        req(getFormulaStr)
        cat("Model:", getFormulaStr())
      })
      
      output$kmPlot <- renderUI({
        o <- self$execInput("options")
        ns <- session$ns
        # Show loading spinner
        shinycssloaders::withSpinner(
          if(requireNamespace("plotly", quietly = TRUE) && (!is.null(o$makePlotly) && o$makePlotly)) {
            tryCatch({
              plotly::plotlyOutput(self$ns("kmPlot2"))
            }, error = function(cond) {
              print("Error while creating plotly graph:")
              print(cond$message)
              print("Defaulting to non-interactive KM plot")
              # Default to non-plotly version if error
              plotOutput(self$ns("kmPlot1"))
            })
            
          } else {
            plotOutput(self$ns("kmPlot1"))
          }
        )
        
      })
      
      output$kmEventSummary <- function() {
        req(survModel())
        
        joinedTables <- rbind(
          kmEventSummary(survModel()),
          kmQuartiles(survModel())
        )
        
        htmlTable::htmlTable(joinedTables,
                             css.cell = "padding-left: 1.5em; padding-right: 1.5em;",
                             rgroup = c('Events', 'KM Quartiles'),
                             n.rgroup= c(3,3),
                             col.rgroup = c("#F7F7F7", "none"),
                             css.table = "margin-top: 1em; margin-bottom: 1em; width: 850px; ")
      }
      
      
      output$kmEventTables <- function() {
        req(survModel())
        
        kmEventTables(survModel())
      }
      
      # Select the strata variable
      output$selectStrataUi <- renderUI({
        d <- data_react()
        m <- mapping()
        # Only show columns that have <= 10 unique values
        choices <- strataColNames(d, limit = 10, showLabels = m$showLabels %||% TRUE)
        # Select the ADaM strata values
        selected <- colnames(d)[grepl("^STRVAL", colnames(d))]
        selectInput(self$ns('strata'), 'Strata Variable', choices = choices, selected = selected, selectize = TRUE, multiple = TRUE)
      })
      
      # Select the covariates
      output$selectCovariatesUi <- renderUI({
        d <- data_react()
        m <- mapping()
        if(is.null(m$showLabels) || m$showLabels) {
          choices <- columnLabelsInvert(d)
        } else {
          choices <- colnames(d)
        }
        
        selectInput(self$ns('covariates'), 'Covariates', choices = choices, selectize = TRUE, multiple = TRUE)
      })
      
      output$selectSubgroupsUi <- renderUI({
        s <- self$execInput("subgroups")
        selectInput(self$ns('subgroups'), 'Subgroups', choices = names(s), selectize = TRUE, multiple = TRUE)
      })
      
      output$paramType <- renderUI({
        m <- mapping()
        h3(
          m$parameter_value
        )
      })
      
      output$coxForestPlot <- renderPlot({
        req(coxphModel)
        coefs <- broom::tidy(coxphModel(), exponentiate = TRUE)
        
        m <- max(coefs$conf.high)
        
        ggplot(coefs, aes(x = estimate, y=term)) +
          geom_point() +
          geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3) +
          geom_vline(xintercept = 1) +
          ggtitle(paste0('Cox Model Hazard Ratio Estimates and 95% CI')) +
          theme_bw() +
          xlim(0, 1 + m) +
          ylab(NULL)
      })
      
      output$coxCoefficients <- function() {
        req(coxphModel, data_with_subgroups)
        # req(length(input$covariates) > 0)
        coxphHazardTable(coxphModel(), data_with_subgroups())
      }
    }
  ),
  private = list()
  )
