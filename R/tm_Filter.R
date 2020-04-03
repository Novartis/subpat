#' Filter Module
#' 
#' The filter module provides an interface to select a data set, variable, and a query based on the value.
#' 
#' @family tidymodules
#' @format R6 class
#' @importFrom R6 R6Class
#' @export
#' @keywords TidyModule
#' @section Input:
#' \describe{
#'   \item{\code{datalist}}{A list of data sets. Can be tibbles, data frames, etc. Expects them to have a common ID field (default is \code{USUBJID})}
#'   \item{\code{options}}{
#'     The options for the module of type \code{\link[shiny]{reactiveValues}} with values \describe{
#'   \item{\code{expanded}}{Should the accordion be expanded when created? Default: \code{TRUE}}
#'   \item{\code{subjectDs}}{If one of the data sets in \code{datalist} contains subject level data then provide it here. Default: \code{ADSL}}
#'   \item{\code{idvar}}{ID field that is common across all datasets in \code{input:datalist}. Default \code{USUBJID}}
#'     }
#'   }
#' }
#' @section Output:
#' \describe{
#' \item{\code{filter}}{S3 object \code{\link{DatasetFilter}}}
#' }
#' @examples
#' \dontrun{
#' library(tidymodules)
#' library(subpat)
#' library(bs4Dash)
#' 
#' addFilter1 <- Filter$new()
#' 
#' ui <- tagList(
#'   shinyjs::useShinyjs(),
#'   bs4DashPage(
#'     sidebar = bs4DashSidebar(disable = TRUE),
#'     
#'     body = bs4DashBody(
#'       bs4Accordion(id = 'filterAccordion',
#'                    addFilter1$ui()),
#'       
#'       verbatimTextOutput('filter_results'),
#'       verbatimTextOutput('showPatients')
#'     )
#'   )
#' )
#' 
#' # Define server logic
#' server <- function(input, output) {
#'   datalist <- reactive({
#'     example_datasets <- list(
#'       "ADSL" = data.frame(
#'        a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
#'        b = c(29L, 4L, 15L, 28L, 41L, 25L, 41L, 15L, 6L, 17L),
#'        USUBJID = as.character(seq_len(10)),
#'        stringsAsFactors = FALSE
#'      ),
#'      "ADAE" = data.frame(
#'        a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
#'        c = c(1, NA, NA, 1, 2, NA, NA, 4, 3, 1),
#'         dates = seq(as.Date("2019-07-10"), as.Date("2019-08-12"), length.out = 10),
#'        USUBJID = as.character(seq_len(10)),
#'        stringsAsFactors = FALSE
#'      ) 
#'     )
#'   })
#'   
#'   opts <- reactiveValues(
#'     subjectDs = "ADSL",
#'     idvar = "USUBJID"
#'   )
#'   
#'   addFilter1$callModule()
#'   
#'   observe({
#'     # Add the options to the filter module
#'     opts %>2% addFilter1
#'     # Add the data to the filter module
#'     datalist %>1% addFilter1
#'   })
#'   
#'   output$filter_results <- renderPrint({
#'     filter_reactive <- addFilter1$getOutput("filter")
#'     str(filter_reactive())
#'   })
#' }
# 
#' # Run the application 
#' shinyApp(ui = ui, server = server)
#' }
Filter <- R6Class(
  "Filter", 
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...){
      super$initialize(...)
      
      self$definePort({
        self$addInputPort(
          name = "datalist",
          description = "A list of data sets",
          sample = example_datasets)
        
        self$addInputPort(
          name = "options",
          description = "The options include `expanded` which is a booleaning indicating the expanded status of accordion",
          sample = list(
            expanded = TRUE,
            subjectDs = "ADSL",
            idvar = c("USUBJID", "STYSID1A")
          )
        )
        
        self$addOutputPort(
          name = "filter",
          description = "The output filter",
          sample = filter_sample)
      })
      
      
    },
    ui = function() {
      tagList(
        includeCSS(system.file("resources/population_mod.css", package = "subpat")),
        bs4AccordionItemExpanded(id = self$ns('accordItem'),
                                 expanded = TRUE,
                                 status = 'default',
                                 title = div(
                                   class = "d-flex justify-content-between filterAccordHeader",
                                   div(class = "p-2", 
                                          actionButtonStatus(
                                            self$ns("deleteButton"),
                                            label = NULL,
                                            status = "danger",
                                            icon = icon('trash-alt'))),
                                   div(class = "p-2",
                                          actionButtonStatus(
                                            self$ns("showPatientsButton"),
                                            label = NULL,
                                            status = "primary",
                                            icon = icon('users'),
                                            onclick = "(function(e) { e.preventDefault(); e.stopPropagation(); })(event)")),
                                   # The dataset and variable
                                   div(class = "p-2", uiOutput(self$ns('accordTitleCol2'))),
                                   # The filter
                                   div(class = "p-2",  uiOutput(self$ns('accordTitleCol3'))),
                                   # The badge
                                   div(class = "p-2",  uiOutput(self$ns('accordTitleCol4'))),
                                   div(class = "p-2", 
                                          div(
                                            class = "collapse-icons",
                                            icon('angle-double-down', class = "collapsed fa-2x"),
                                            icon('angle-double-up', class = "expanded fa-2x")
                                          )
                                   )
                                 ),
                                 # Close all the other accordion items when we press new item
                                 onheaderclick = '$(".accordion .panel-collapse").collapse("hide");',
                                 fluidRow(
                                   column(
                                     width = 4,
                                     uiOutput(self$ns('selectDsUi'))
                                   ),
                                   column(
                                     width = 4,
                                     uiOutput(self$ns('selectVarUi'))
                                   ),
                                   column(
                                     width = 4,
                                     uiOutput(self$ns('varValuesUI'))
                                   )
                                 ),
                                 div(
                                   class = "d-flex flex-row justify-content-between",
                                   div(
                                     class = "p-2",
                                     radioButtons(self$ns('missingOptions'), label="Missing value options",
                                                  choiceNames = c("Remove Missing", "Only missing", "Include missing"),
                                                  choiceValues = c("removeMissing", "onlyMissing", "includeMissing"), inline = TRUE)
                                   ))
      )
      
      )
    },
    server = function(input, output, session, 
                      data = NULL, 
                      options = NULL){
      
      super$server(input,output,session)
      
      self$assignPort({
        
        self$updateInputPort(
          id = "datalist",
          input = data)
        
        if(!is.null(options)) {
          self$updateInputPort(
            id = "options",
            input = options
          )
        }
      })
      
      local_rv <- reactiveValues(
        commonColumns = character(0),
        currentDataset = NULL,
        datasetsOrdered = NULL,
        indexADSL = integer(0),
        colnameADSL = character(0),
        # The labels for the current dataset
        varlabels = list(),
        currentFilter = NULL,
        queriedPatients = NULL
      )
      
      
      # When the data sets change, re-compute the variable names and ADSL
      observe({
        dl <- self$execInput("datalist")
        o <- self$execInput("options")
        
        if(!is.null(o$subjectDs)) {
          local_rv$indexADSL <- private$findADSL()
          local_rv$ADSL <- dl[[local_rv$indexADSL]]
          local_rv$colnameADSL <- names(dl)[[local_rv$indexADSL]]
          withoutADSL <- setdiff(seq_along(dl), local_rv$indexADSL)
          # Place ADSL at the top of the list
          local_rv$datasetsOrdered <- names(dl)[c(local_rv$indexADSL, withoutADSL)]
          local_rv$commonColumns <- colnames(dl[[local_rv$indexADSL]])
        } else {
          local_rv$datasetsOrdered <- names(dl)
          local_rv$commonColumns <- colnames(dl)
        }
      })
      
      # When the data set changes update the variable names available
      observeEvent(input$selectDs, {
        dl <- self$execInput("datalist")
        o <- self$execInput("options")
        
        # Clear the current filter
        local_rv$currentFilter <- NULL
        
        local_rv$currentDatasetName <- input$selectDs
        local_rv$currentDataset <- dl[[local_rv$currentDatasetName]]
        # Get the unique subject count for the current dataset
        if(is.null(o$idvar)) {
          local_rv$currentDatasetSubjectCount <- unique(local_rv$currentDataset[, "USUBJID", drop = TRUE])
        } else {
          local_rv$currentDatasetSubjectCount <- unique(local_rv$currentDataset[, o$idvar, drop = TRUE])
        }
        
        colLabs <- columnLabels(local_rv$currentDataset)
        # Not sure why we need both as.vector and unlist..
        labs <- as.vector(unlist(colLabs))

        # If we are not using ADSL, then hide the ADSL columns
        if(!is.null(o$subjectDs) && !is.null(local_rv$colnameADSL) && input$selectDs != local_rv$colnameADSL) {
          choices <- setdiff(labs, colLabs[local_rv$commonColumns])
        } else {
          # Show the variable labels
          choices <- labs
        }
        
        local_rv$varlabels <- choices
      })
      
      observeEvent(input$missingOptions, {
        req(local_rv$currentFilter)
        local_rv$currentFilter$onlyMissing <- input$missingOptions == "onlyMissing"
        local_rv$currentFilter$includeMissing <- input$missingOptions == "includeMissing"
      })
      
      observeEvent(input$onlyMissing, {
        local_rv$currentFilter$onlyMissing = input$missingOptions == "onlyMissing"
        local_rv$currentFilter$includeMissing = input$missingOptions == "includeMissing"
      })
      
      observeEvent(local_rv$currentFilter, {
        o <- self$execInput("options")
        # Query the patients when the filter changes
        local_rv$queriedPatients <- querySubjects(local_rv$currentFilter, dataset = local_rv$currentDataset, idkey = o$idvar)
      })
      
      # Update the currentFilter as categorical
      observeEvent({
        #input$selectVar
        input$varPicker
      }, {
        req(input$selectVar)
        
        local_rv$currentFilter$catValues <-  input$varPicker
      }, ignoreNULL = FALSE)
      
      # Update currentFilter as numeric
      observeEvent({
        input$varSlider
      }, {
        local_rv$currentFilter$minVal <-input$varSlider[1]
        local_rv$currentFilter$maxVal <-input$varSlider[2]
      }, ignoreNULL = FALSE)
      
      # Update currentFilter as date
      observeEvent({
        input$startDate
        input$endDate
      }, {
        req(isolate(input$selectVar))
        local_rv$currentFilter$minVal <- input$startDate
        local_rv$currentFilter$maxVal <- input$endDate
      }, ignoreNULL = FALSE)
      
          
      # Hide the accordion when delete button is pressed
      # Triggered when delete button is pressed
      observeEvent(input$deleteButton, {
        local_rv$currentFilter <- NULL
        shinyjs::hide(id = "accordItem_outer")
      })
      
      observeEvent(input$showPatientsButton, {
        showModal(modalDialog(
          title = "Patient Listing",
          DT::dataTableOutput(self$ns('patientModalTable'))
        ), session = self$getShinySession())
      })
      
      output$accordTitleCol2 <- renderText({
        req(input$selectVar, input$selectDs)
        paste(input$selectVar, input$selectDs, sep = "@")
      })
      
      output$accordTitleCol3 <- renderText({
        req(local_rv$currentFilter)
        
        if(! is.null(local_rv$currentFilter)) {
          toString(local_rv$currentFilter)
        } else {
          ""
        }
      })
      
      output$accordTitleCol4 <- renderText({
        # req(local_rv$currentFilter, local_rv$currentDatasetSubjectCount, local_rv$currentDataset)
        if(!is.null(local_rv$currentFilter) && ! is.null(local_rv$currentDataset) && ! is.null(local_rv$currentDatasetSubjectCount)) {
          toString(bs4Badge(
            id = self$ns("patientBadge"),
            paste(length(local_rv$queriedPatients), "/", length(local_rv$currentDatasetSubjectCount), "patients"),
            status = "warning"))
        }
      })
      
      output$selectDsUi <- renderUI({
        req(local_rv$datasetsOrdered)
        selectInput(self$ns('selectDs'), label = "Select Dataset", choices = local_rv$datasetsOrdered)
      })
      
      output$selectVarUi <- renderUI({
        req(local_rv$varlabels)
        selectInput(self$ns('selectVar'), label="Select Variable", choices = local_rv$varlabels)
      })
      
      # Render the variable value selector
      # Generate a multiple shinyWidgets::pickerInput for categorical variables
      # dateInput for start and end date
      # sliderInput for numeric variables
      output$varValuesUI <- renderUI({
        dl <- self$execInput("datalist")
        
        if (!is.null(input$selectDs) && !is.null(input$selectVar)) {
          dataset <- dl[[input$selectDs]]
          
          # The variable is the label
          selCol <- labelToColumn(dataset, input$selectVar)
          
          # Show a selectInput if we have a numeric (categorical) column
          if (is.character(selCol) || is.factor(selCol)) {
            choices <- unique(selCol)
            
            # Create the new filter
            local_rv$currentFilter <- DatasetFilter(dataset = local_rv$currentDatasetName,
                                                    variable = labelToName(local_rv$currentDataset, input$selectVar),
                                                    filterType = "categorical")
            
            shinyWidgets::pickerInput(self$ns('varPicker'), label="Select Values", choices = choices, multiple = TRUE)
          } else if (is.date(selCol)) {
            minCol <- min(selCol, na.rm = TRUE)
            maxCol <- max(selCol, na.rm = TRUE)
            
            # Create the new filter
            local_rv$currentFilter <- DatasetFilter(dataset = local_rv$currentDatasetName,
                                                    variable = labelToName(local_rv$currentDataset, isolate(input$selectVar)),
                                                    filterType = "date")
            
            # Two date selectors for start and end date
            tagList(
              dateInput(self$ns('startDate'), "Start Date", value = minCol, min = minCol, max = maxCol, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en"),
              dateInput(self$ns('endDate'), "End Date", value = maxCol, min = minCol, max = maxCol, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en")
            )
          } else if (is.numeric(selCol)) {
            # Show a sliderInput if numeric input
            minCol <- floor(min(selCol, na.rm = TRUE))
            maxCol <- ceiling(max(selCol, na.rm = TRUE))
            
            # Create the new filter
            local_rv$currentFilter <- DatasetFilter(dataset = local_rv$currentDatasetName,
                                                    variable = labelToName(local_rv$currentDataset, input$selectVar),
                                                    filterType = "numeric")
            
            sliderInput(self$ns('varSlider'), label="Variable range", min = minCol, max = maxCol, value = c(minCol, maxCol))
          }
        } else {
          # No variable selected, show an empty selectInput
          shinyWidgets::pickerInput(self$ns('varPicker'), label="Select Values", choices = "")
        }
      })
      
      output$patientModalTable <- DT::renderDataTable({
        o <- self$execInput("options")
        
        d <- data.frame(
          USUBJID = local_rv$queriedPatients
        )
        
        if(!is.null(o$idvar)) {
          colnames(d) <- o$idvar
        }
        
        DT::datatable(
          d,
          options = list(
            # Other options
          ),
          rownames = FALSE
        )
      })
      
      self$assignPort({
        
        self$updateOutputPort(
          id = "filter",
          output = reactive({local_rv$currentFilter}))
      })
      
      return(reactive({local_rv$currentFilter}))
    }
  ),
  private = list(
    # Private methods
    findADSL = function() {
      o <- self$execInput("options")
      dl <- self$execInput("datalist")
      subjectDs <- ifelse(is.null(o$subjectDs), "ADSL", o$subjectDs)
      
      res <- grep(subjectDs, names(dl), ignore.case = TRUE)
      if (length(res) == 0) {
        warning(paste0("Could not find common data set `", subjectDs, "` in the sasdatalist"))
      }
      res
    }
  )
)
