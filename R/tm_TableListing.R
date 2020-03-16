#' Table Listing Module
#' 
#' Shiny \code{\link[tidymodules]{TidyModule}} create ad-hoc (non-validated) PDF table listings by data set and by subject.
#' 
#' @family tidymodules
#' @format R6 class
#' @importFrom R6 R6Class
#' @importFrom shinycssloaders withSpinner
#' @export
#' @section Input:
#' \describe{
#'   \item{\code{datalist}}{A list of data sets. Can be tibbles, data frames, etc. Expects them to have a common ID field (default is \code{USUBJID})}
#'   \item{\code{populations}}{A list of \code{\link{PopulationFilter}}}
#'   \item{\code{options}}{
#'     The options for the module of type \code{\link[shiny]{reactiveValues}} with values \describe{
#'   \item{\code{subjectDs}}{If one of the data sets in \code{datalist} contains subject level data then provide it here. Default: \code{ADSL}}
#'   \item{\code{idvar}}{ID field that is common across all datasets in \code{datalist}. Default \code{USUBJID}}
#'     }
#'   }
#' }
#' @section Output:
#' No output is returned for this module.
#' @examples 
#' \dontrun{
#' library(bs4Dash)
#' library(subpat)
#' library(tidymodules)
#' 
#' tableListMod <- TableListing$new()
#' 
#' example_datasets <- list(
#'   "ADSL" = data.frame(
#'     a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
#'     b = c(29L, 4L, 15L, 28L, 41L, 25L, 41L, 15L, 6L, 17L),
#'     USUBJID = as.character(seq_len(10)),
#'     stringsAsFactors = FALSE
#'   ),
#'   "ADAE" = data.frame(
#'     a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
#'     c = c(1, NA, NA, 1, 2, NA, NA, 4, 3, 1),
#'     dates = seq(as.Date("2019-07-10"), as.Date("2019-08-12"), length.out = 10),
#'     USUBJID = as.character(seq_len(10)),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' 
#' # This is the output from dput on a PopulationFilter
#' # Use the Subpopulation to create them in a shiny application
#' example_populations <- list(
#'   # Population 1
#'   structure(list(dataset_names = c("ADSL", "ADAE"), filters = structure(list(
#'     filter_1 = structure(
#'       list(
#'         dataset_name = "ADAE", variable = "c", 
#'         filterType = "numeric", includeMissing = FALSE, onlyMissing = FALSE, 
#'         createDate = "Fri Jul 26 18:44:11 2019", minVal = 1L, maxVal = 4L
#'       ), 
#'       .Names = c("dataset_name", "variable", 
#'                  "filterType", "includeMissing", "onlyMissing", "createDate", 
#'                  "minVal", "maxVal"),
#'       class = "DatasetFilter")
#'       ),
#'       .Names = "filter_1"
#'     ), 
#'     name = "New population: 1", createDate = "Fri Jul 26 18:44:05 2019", 
#'     editDate = "Fri Jul 26 18:44:13 2019"),
#'     .Names = c("dataset_names", "filters", "name", "createDate", "editDate"),
#'     class = "PopulationFilter")
#' )
#' 
#' ui <- tagList(
#'   shinyjs::useShinyjs(),
#'   bs4DashPage(
#'     sidebar = bs4DashSidebar(disable = TRUE),
#'     
#'     body = bs4DashBody(
#'       tableListMod$ui()
#'     )
#'   )
#' )
#' 
#' # Define server logic
#' server <- function(input, output) {
#'   opts <- reactiveValues(
#'     idvar = "USUBJID"
#'   )
#'   
#'   tableListMod$callModule()
#'   
#'   observe({
#'     reactive(example_datasets) %>1% tableListMod
#'     reactive(example_populations) %>2% tableListMod
#'     opts %>3% tableListMod
#'   })
#' }
#' 
#' # Run the application 
#' shinyApp(ui = ui, server = server)
#' 
#' }
TableListing <- R6Class(
  "TableListing", 
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...){
      super$initialize(...)
      
      self$definePort({
        self$addInputPort(
          name = "datalist",
          description = "A list of data sets",
          sample = example_datasets,
          input = NULL)
        
        self$addInputPort(
          name = "populations",
          description = "The populations created",
          sample = list(
            `Population 1` = PopulationFilter(list(), list(), name = "Population 1"),
            `Population 2` = PopulationFilter(list(), list(), name = "Population 2")
          ),
          input = NULL)
        
        self$addInputPort(
          name = "options",
          description = "`subjectDs` to specify a subject level data set. `idvar` is a variable that is common to all data sets to join on.",
          sample = reactiveValues(
            subjectDs = "ADSL",
            idvar = c("USUBJID", "STYSID1A")
          ),
          input = NULL)
      })
    },
    ui = function() {
      tagList(
        includeCSS(system.file("resources/table_listing_mod.css", package = "subpat")),
        
        fluidRow(
          column(
            width = 6,
            uiOutput(self$ns("populationSelectUI")),
            uiOutput(self$ns("datasetSelectUI"))
          ),
          column(
            width = 4,
            offset = 1,
            style = "margin-top: 28px;",
            fluidRow(
              downloadButton(self$ns('listingDatasets'), label = "Listing by Data set"),
              downloadButton(self$ns('subjectListing'), label = "Listing by Subjects")
            )
          )
        ),
        shinycssloaders::withSpinner(uiOutput(self$ns("datasetCardsUI"), inline = TRUE))
      )
    },
    server = function(input, output, session, 
                      datalist = NULL,
                      populations = NULL,
                      options = NULL){
      
      super$server(input,output,session)
      
      self$assignPort({
        self$updateInputPort(
          id = "datalist",
          input = datalist)
        
        self$updateInputPort(
          id = "populations",
          input = populations)
        
        self$updateInputPort(
          id = "options",
          input = options
        )
      })
      
      local_rv <- reactiveValues(
        # The datasets
        datalist = NULL,
        # The populations
        populations = NULL,
        # The queried datasets
        queriedDatasets = NULL,
        # The variable selection modules
        varSelModules = list()
      )
      
      observe({
        local_rv$datalist <- self$execInput("datalist")
      })
      
      observe({
        local_rv$populations <- self$execInput("populations")
      })
      
      observeEvent(local_rv$populations, {
        names(local_rv$populations) <- unlist(lapply(local_rv$populations, function(x) x$name))
      })
      
      observeEvent({
        input$populationSelect
        local_rv$populations
        input$datasetSelect
      }, {
         req(input$populationSelect)
        
        o <- self$execInput("options")
        
        
        if(input$populationSelect == "Full Population") {
          local_rv$queriedDatasets <- local_rv$datalist
        } else {
          selectedPop <- local_rv$populations[[input$populationSelect]]
          local_rv$queriedDatasets <- query(selectedPop, local_rv$datalist, idkey = o$idvar)
        }
      })
      
      # When the queried datasets update, create new modules 
      observeEvent(local_rv$queriedDatasets, {
        req(local_rv$queriedDatasets)
        # Initialize the modules
        # Need to do this in a for-loop because of the way the module system is set up
        # It looks at the parent environment but will fail if called in lapply (or a function)
        displayDatasets <- local_rv$queriedDatasets[input$datasetSelect]
        for(i in seq_along(displayDatasets)) {
          dataset <- displayDatasets[[i]]
          dataset_name <- names(displayDatasets)[[i]]
          
          # Call the module
          local_rv$varSelModules[[dataset_name]] <- VariableSelection$new(inherit = FALSE)
          local_rv$varSelModules[[dataset_name]]$callModule()
        }
      })
      
      # When the modules or queried dataset update, pass in the new data to each of the modules
      # This workaround is because it does not work to pass the data in the same for loop 
      observeEvent(local_rv$varSelModules, {
        o <- self$execInput("options")
        
        req(local_rv$varSelModules, local_rv$queriedDatasets)
        lapply(names(local_rv$varSelModules), function(dataset_name) {
          dataset <- local_rv$queriedDatasets[[dataset_name]]
          
          # Use the passed in options as well as dataset name
          opts <- reactive(list(
            subjectDs = o$subjectDs,
            idvar = o$idvar,
            datasetname = dataset_name
          ))
          
          opts %>2% local_rv$varSelModules[[dataset_name]]
          reactive(dataset) %>1% local_rv$varSelModules[[dataset_name]]
        })
      })
      
      output$datasetCardsUI <- renderUI({
        req(local_rv$queriedDatasets, local_rv$varSelModules)
        req(length(local_rv$queriedDatasets) > 0)
        req(input$datasetSelect)
        isolate({
          displayDatasets <- local_rv$queriedDatasets[input$datasetSelect]
          
          # Manually create an accordion
          # Doing it with avaAccordion caused ID to show as the last item
          div(
            id = self$ns('datasetsaccordion'),
            lapply(names(displayDatasets), function(dataset_name) {
              childUi <- local_rv$varSelModules[[dataset_name]]$ui()
              # Manually add accordion id
              childUi$children[[1]]$children[[2]]$attribs[["data-parent"]] <- paste0("#", self$ns('datasetsaccordion')) 
              childUi
            })
          )
        })
      })
      
      output$populationSelectUI <- renderUI({
        selectInput(inputId = self$ns('populationSelect'),
                    label = "Select subpopulation",
                    selectize = TRUE,
                    choices = c("Full Population", names(local_rv$populations)))
      })
      
      output$datasetSelectUI <- renderUI({
        selectInput(inputId = self$ns('datasetSelect'),
                    label = "Select Data sets",
                    selectize = TRUE,
                    multiple = TRUE,
                    choices = names(local_rv$datalist),
                    selected = names(local_rv$datalist))
      })
      
      output$listingDatasets <- downloadHandler(
        filename = "listing.pdf",
        content = function(f) {
          o <- self$execInput("options")
          
          shinyjs::runjs(
            paste0("$('#",self$ns('loadingModal'),"').modal('show')")
          )

          # Create an environment to run the R-markdown template in
          # This allows us to pass variables into the template
          e <- new.env()
          # Get the output from the variable selection modules
          # Only the ones that are selected
          e$datasets <- lapply(local_rv$varSelModules[input$datasetSelect], function(m) {
            m$getOutput("filtered_data")()
          })
          e$population <- local_rv$populations[[input$populationSelect]]
          e$idvar <- o$idvar
          
          template <- system.file('resources/listing_template.Rmd', package = 'subpat')
          template_tmpdir  <- tempfile(fileext = ".Rmd")
          file.copy(template, template_tmpdir)
          
          rmarkdown::render(template_tmpdir, output_format = rmarkdown::pdf_document(),
                            output_file=f,
                            envir = e)
          
          shinyjs::runjs(
            paste0("$('#",self$ns('loadingModal'),"').modal('hide')")
          )
        }
      )
      
      output$subjectListing <- downloadHandler(
        filename = "listing.pdf",
        content = function(f) {
          o <- self$execInput("options")
          
          shinyjs::runjs(
            paste0("$('#",self$ns('loadingModal'),"').modal('show')")
          )
          
          # Create an environment to run the R-markdown template in
          # This allows us to pass variables into the template
          e <- new.env()
          # Get the output from the variable selection modules
          # Only the ones that are selected
          
          e$datasets <- lapply(local_rv$varSelModules[input$datasetSelect], function(m) {
            m$getOutput("filtered_data")()
          })
          
          e$subjects <- querySubjects.PopulationFilter(local_rv$populations[[input$populationSelect]], local_rv$datalist, idkey = o$idvar)
          e$idvar <- o$idvar
          
          # Need to include subject id
          e$population <- local_rv$populations[[input$populationSelect]]
          template <- system.file('resources/subject_listing_template.Rmd', package = 'subpat')
          
          template_tmpdir  <- tempfile(fileext = ".Rmd")
          file.copy(template, template_tmpdir)
          
          rmarkdown::render(template_tmpdir, output_format = rmarkdown::pdf_document(),
                            output_file=f,
                            envir = e)
          
          shinyjs::runjs(
            paste0("$('#",self$ns('loadingModal'),"').modal('hide')")
          )
        }
      )
    }
  ),
  private = list(
    # Private methods
  )
)