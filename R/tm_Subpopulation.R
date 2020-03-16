
#' Population Module
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
#'   \item{\code{populationNumber}}{The index of the population that we are creating. If given, will include as the default name for the population "New population: \code{populationNumber}"}
#'   \item{\code{hideOnSave}}{Boolean. Should the population card be hidden when we save? Default: \code{TRUE}}
#'   \item{\code{idvar}}{ID field that is common across all datasets in \code{datalist}. Default \code{USUBJID}}
#'     }
#'   }
#' }
#' @section Output:
#' \describe{
#' \item{\code{savePopulation}}{S3 object \code{\link{PopulationFilter}} returned when \code{Save Population} button is pressed}
#' \item{\code{editPopulation}}{S3 object \code{\link{PopulationFilter}} which is updated after any change to the population.}
#' }
#' @examples
#' \dontrun{
#' library(tidymodules)
#' library(subpat)
#' library(bs4Dash)
#' 
#' # Create a new population edit module
# popedit <- Subpopulation$new(id = "popedit")
#' 
#' ui <- tagList(
#'   shinyjs::useShinyjs(),
#'   bs4DashPage(
#'     sidebar = bs4DashSidebar(disable = TRUE),
#'     body = bs4DashBody(
#'       popedit$ui(),
#'       
#'       fluidRow(
#'         column(width = 6, 
#'                p("Editing population"),
#'                verbatimTextOutput('editing_population')),
#'         column(width = 6,
#'                p("Saved object"),
#'                verbatimTextOutput('saved_population'))
#'       )
#'     )
#'   )
#' )
#' 
#' # Define server logic required to draw a histogram
#' server <- function(input, output) {
#'   datalist <- reactive({
#'     list(
#'       "ADSL" = data.frame(
#'         a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
#'         b = c(29L, 4L, 15L, 28L, 41L, 25L, 41L, 15L, 6L, 17L),
#'         USUBJID = as.character(seq_len(10)),
#'         stringsAsFactors = FALSE
#'       ),
#'       "ADAE" = data.frame(
#'         a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
#'         c = c(1, NA, NA, 1, 2, NA, NA, 4, 3, 1),
#'         dates = seq(as.Date("2019-07-10"), as.Date("2019-08-12"), length.out = 10),
#'         USUBJID = as.character(seq_len(10)),
#'         stringsAsFactors = FALSE
#'       )
#'     )
#'   })
#'   
#'   popedit$callModule()
#'   
#'   opts <- reactiveValues(
#'     hideOnSave = FALSE,
#'     populationNumber = 1,
#'     subjectDs = "ADSL",
#'     idvar = "USUBJID"
#'   )
#'   
#'   observe({
#'     # Add the data to the module
#'     datalist %>1% popedit
#'     
#'     # Add the options to the population editing module
#'     opts %>2% popedit
#'   })
#'   
#'   output$saved_population <- renderPrint({
#'     req(popedit)
#'     str(popedit$getOutput(1)())
#'   })
#'   
#'   output$editing_population <- renderPrint({
#'     req(popedit)
#'     str(popedit$getOutput(2)())
#'   })
#' }
#' 
#' # Run the application 
#' shinyApp(ui = ui, server = server)
#' }
Subpopulation <- R6Class(
  "Subpopulation", 
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
          description = "populationNumber: current number for the population, hideOnSave: should this ui be hidden on save?",
          sample = list(
            populationNumber = 1,
            hideOnSave = TRUE,
            idvar = c("USUBJID", "STYSID1A")))
        
        
        self$addOutputPort(
          name = "savePopulation",
          description = "Event called when save population is pressed. Returns the saved population.",
          sample = PopulationFilter(list(), list()))
        
        self$addOutputPort(
          name = "editingPopulation",
          description = "The current population that is being edited. Updated before saved.",
          sample = PopulationFilter(list(), list())
        )
      })
      
      
    },
    ui = function() {
      div(id = self$ns("editSubpopulationModule"),
          box(height = "25%",
                width = 12,
                title = fluidRow(
                  column(3, 
                         span("Edit Population")),
                  column(8,               
                         # Number of filters
                         uiOutput(self$ns('filterCountUi'), inline = TRUE),
                         # Number of patients
                         uiOutput(self$ns('patientCountUi'), inline = TRUE))
                ),
                closable = FALSE,
                # Edit population name
                div(
                  class = "d-flex flex-row",
                  div(
                    class = "p-2 align-self-center flex-fill",
                    uiOutput(self$ns('populationNameUi'))
                  ),
                  div(
                    class = "p-2 align-self-center flex-fill",
                    actionButton(self$ns('savePopulation'), label = 'Save Population')
                  )
                ),
                # Show accordion
                bs4Accordion(id = self$ns('filterAccordion')),
                # Add new filter
                actionButton(
                  self$ns('addFilter'),
                  label = "",
                  icon = icon('plus-circle'),
                  # Collapse all the accordion items when we press new item
                  onclick = '$(".accordion .panel-collapse").collapse("hide");'
                )
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
        
        self$updateInputPort(
          id = "options",
          input = options
        )
      })
      
      local_rv <- reactiveValues(
        currentPopulation = NULL,
        delete_filter = NULL,
        totalSubjects = 0,
        currentFilters = list(),
        filterModules = list(),
        deleteEvents = list()
      )
      
      observeEvent(self$execInput("datalist"), {
        dl <- self$execInput("datalist")
        o <- self$execInput("options")
        
        local_rv$totalSubjects <- length(totalSubjects(dl, idkey = o$idvar))
        # Initialize the current population
        local_rv$currentPopulation <- PopulationFilter(names(dl))
      })
      
      observeEvent(input$addFilter, {
        selector <- paste0("#", self$ns('filterAccordion'), "accordion")

        # Dynamically create filter module
        new_filter <- Filter$new()
        local_rv$filterModules[[new_filter$module_ns]] <- new_filter
        new_filter$callModule()
        
        insertUI(
          selector = selector,
          where = "beforeEnd",
          ui = new_filter$ui(),
          session = self$getShinySession()
        )
      })
      
      # When save population is pressed, update the savePopulation reactive with the current population
      savePopulation <- eventReactive(input$savePopulation, {
        o <- self$execInput("options")
        
        # Hide this module, default is true
        if(is.null(o$hideOnSave) || o$hideOnSave) {
          shinyjs::hide(id = 'editSubpopulationModule')
        }
        
        # Update the last edit date
        local_rv$currentPopulation$editDate <- date() 
        isolate({local_rv$currentPopulation})
      })
      
      # When the filter modules change, update the current population's filters to reflect new outputs
      observe({
        # Remove the null filters from the list
        local_rv$currentPopulation$filters <- Filter(Negate(is.null), lapply(local_rv$filterModules, function(fm) {
          r <- fm$getOutput(1)
          if(is.logical(r) && !r)
            NULL
          else
            r()
        }))
      })
      
      # Remove the filter when the delete button in the accordion item is pressed
      observeEvent(local_rv$delete_filter, {
        removeUI(
          selector = paste0("#", local_rv$delete_filter)
        )
      })
      
      observeEvent(input$populationName, {
        # Keep name up to date in current population
        local_rv$currentPopulation$name <- input$populationName
      })
      
      output$populationNameUi <- renderUI({
        o <- self$execInput("options")
        initPopulationName <- paste0("New population: ", o$populationNumber)
        textInput(self$ns('populationName'), label = 'Population Name', value = initPopulationName)
      })
      
      output$filterCountUi <- renderUI({
        filter_length <- length(local_rv$currentPopulation$filters)
        if(filter_length == 0) {
          badge_text <- "No filters"
        } else {
          badge_text <- paste0(filter_length, " filter", ifelse(filter_length == 1, "", "s"))
        }
        bs4Badge(badge_text, status = "primary")
      })
      
      output$patientCountUi <- renderUI({
        dl <- self$getInput("datalist")
        o <- self$execInput("options")
        
        query_count <- length(querySubjects.PopulationFilter(local_rv$currentPopulation, dl(), commonTable = o$subjectDs, idkey = o$idvar))
        
        patientCountBadge(query_count, local_rv$totalSubjects)
      })
      
      self$assignPort({
        
        self$updateOutputPort(
          id = "savePopulation",
          output = savePopulation
        )
        
        self$updateOutputPort(
          id = "editingPopulation",
          output = reactive({local_rv$currentPopulation})
        )
      })
    }
  ),
  private = list(
    # Private methods
  )
)