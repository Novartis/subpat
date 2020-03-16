#' Subpopulation Manager TidyModule
#' 
#' Provides module for creating and editing subpopulations.
#'
#' @family tidymodules
#' @format R6 class
#' @importFrom R6 R6Class
#' @export
#' 
#' @keywords TidyModule
#' @section Input:
#' \describe{
#'   \item{\code{datalist}}{A list of data sets. Can be tibbles, data frames, etc. Expects them to have a common ID field (default is \code{USUBJID})}
#'   \item{\code{options}}{
#'     The options for the module of type \code{\link[shiny]{reactiveValues}} with values \describe{
#'   \item{\code{subjectDs}}{If one of the data sets in \code{datalist} contains subject level data then provide it here. Default: \code{ADSL}}
#'   \item{\code{idvar}}{ID field that is common across all datasets in \code{datalist}. Default \code{USUBJID}}
#'     }
#'   }
#' }
#' @section Output:
#' \describe{
#' \item{\code{populations}}{List of \code{\link{PopulationFilter}} S3 objects.}
#' }
#' @examples
#' \dontrun{
#' library(tidymodules)
#' library(subpat)
#' library(bs4Dash)
#' 
#' subpopmod <- SubpopulationManager$new(id = "subpopmodule")
#' 
#' ui <- tagList(
#'   shinyjs::useShinyjs(),
#'   bs4DashPage(
#'     sidebar = bs4DashSidebar(disable = TRUE),
#'     body = bs4DashBody(
#'       subpopmod$ui()
#'     )
#'   )
#' )
#' 
#' # Define server logic
#' server <- function(input, output) {
#'   datalist <- reactive({
#'     example_datasets
#'   })
#'   
#'   opts <- reactiveValues(
#'     subjectDs = "ADSL",
#'     idvar = "USUBJID"
#'   )
#'   
#'   subpopmod$callModule()
#'   
#'   observe({
#'     # Pass the data into the module
#'     datalist %>1% subpopmod
#'     
#'     # Add the options to the subpopulation manager
#'     opts %>2% subpopmod
#'   })
#' }
#' 
#' # Run the application 
#' shinyApp(ui = ui, server = server)
#' }
SubpopulationManager <- R6Class(
  "SubpopulationManager",
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
          inherit = FALSE,
          sample =  reactiveValues(
            subjectDs = "ADSL",
            idvar = c("USUBJID", "STYSID1A")
          ))
        
        self$addOutputPort(
          name = "populations",
          description = "The populations created",
          sample = list(
            `Population 1` = PopulationFilter(list(), list(), name = "Population 1"),
            `Population 2` = PopulationFilter(list(), list(), name = "Population 2")
          ))
      })
    },
    ui = function() {
      tagList(
        # Adds pulse effect to show user where to expand accordion
        shinyEffects::setPulse(class = "expand-pulse", duration = 1, iteration = 5),
        
        # Should only show if we have data sets available
        shinyjs::hidden(
          div(
            id = self$ns('populations'),
            tagList(box(
              height = "25%",
              width = 12,
              title = fluidRow(column(3,
                                      span("Populations")),
                               column(2,
                                      uiOutput(
                                        self$ns('populationBadgeUi')
                                      ))),
              closable = FALSE,
              # Shows populationTable if we have populations
              # Otherwise shows help screen
              uiOutput(self$ns('populationTableUi')),
              
              div(
                class = "d-flex flex-row",
                div(class = "p-2",
                    actionButton(
                      self$ns('createPopulation'),
                      label = "",
                      icon = icon('plus-circle')
                    )),
                # Button to delete multpiple should only be shown when
                # checkboxes are selected
                
                div(class = "p-2",
                    shinyjs::hidden(
                      actionButtonStatus(
                        self$ns('delete_multiple'),
                        label = "Delete selected",
                        icon = icon('trash-alt'),
                        status = "danger"
                      )
                    ))
              )
            ))
          )
        ),
        div(
          id = self$ns('no_data_found'),
          p("Please load some data first.")
        ),
        div(
          id = self$ns("edit_population")
        )
      )
    },
    server = function(input, output, session, 
                      datalist = NULL,
                      options = NULL){
      
      super$server(input,output,session)
      
      self$assignPort({
        self$updateInputPort(
          id = "datalist",
          input = datalist)
        
        self$updateInputPort(
          id = "options",
          input = options %||% reactive({
            list(
                subjectDs = "ADSL",
                idvar = "USUBJID")
          })
        )
      })
      
      local_rv <- reactiveValues(
        # Index of the population we are editing
        editingPopulationIndex = NULL,
        # Contains the row ids of all the selected populations
        selectedRows = NULL,
        # The data frame to render based on the populations
        renderDf = NULL,
        # Total subjects in the datasets
        totalSubjects = 0,
        # Initial population id
        populationId = 1,
        # The subjects
        datasetSubjects = NULL,
        # The list of EditPopulation Modules
        populationModules = list(),
        # The current populations
        populations = list()
      )
      
      observe({
        local_rv$datalist <- self$execInput("datalist")
      })
      
      observe({
        o <- self$execInput("options")
        
        local_rv$renderDf <- data.frame(
          population_names = unlist(purrr::map(local_rv$populations, "name")),
          filters = unlist(purrr::map(local_rv$populations, getFilterVarNames)),
          # TODO: Should not compute this each time
          patient_count = unlist(purrr::map(local_rv$populations, ~ patientCountBadge(length(querySubjects(.x, local_rv$datalist, idkey = o$idvar)), local_rv$totalSubjects, as.string = TRUE)))
        )
      })
      
      observeEvent(local_rv$datalist, {
        o <- self$execInput("options")
        
        local_rv$totalSubjects <- length(totalSubjects(local_rv$datalist, idkey = o$idvar))
        
        if(!is.null(local_rv$datalist)) {
          shinyjs::show('populations')
          shinyjs::hide('no_data_found')
        } else {
          shinyjs::hide('populations')
          shinyjs::show('no_data_found')
        }
      })
      
      # Dynamically add the new population module
      observeEvent(input$createPopulation, {
        o <- self$execInput("options")

        spm <- Subpopulation$new()
        local_rv$populationModules[[spm$module_ns]] <- spm 
        spm$callModule()
        
        # Pass the population number as an option into the edit population module
        o$populationNumber = isolate(local_rv$populationId)
        
        # pass the options into the module
        reactive(o) %>2% spm
        
        insertUI(
          selector = paste0("#", self$ns("edit_population")),
          ui = spm$ui(),
          where = "beforeEnd",
          session = self$getShinySession()
        )
        
        local_rv$populationId <- local_rv$populationId + 1
        
        # Disable create population
        shinyjs::disable("createPopulation")
      })
      
      # When the populations modules change, update the current populations to reflect new outputs
      observe({
        local_rv$populations <- lapply(local_rv$populationModules, function(pm) {
          pm$execOutput(1)
        })
        
        # This only gets called when one module is saved. Enable the create population button
        shinyjs::enable("createPopulation")
      })
      
      # Hide the delete button when no rows are selected
      observeEvent(local_rv$selectedRows, {
        if(is.null(local_rv$selectedRows) || length(local_rv$selectedRows) == 0) {
          shinyjs::hide("delete_multiple")
        } else {
          shinyjs::show("delete_multiple")
        }
        
      }, ignoreNULL = FALSE)
      
      observeEvent(input$checkboxClicked, {
        print(input$checkboxClicked$id)
        clickedRow <- extractRowNumber(input$checkboxClicked$id)
        
        isChecked <- hasClass("checked", input$checkboxClicked$classes)
        
        # Update the selected rows with the new checked/unchecked information
        if(isChecked) {
          local_rv$selectedRows <- union(local_rv$selectedRows, clickedRow)
        } else {
          local_rv$selectedRows <- setdiff(local_rv$selectedRows, clickedRow)
        }
      })
      
      observeEvent(input$delete_multiple, {
        req(local_rv$populations)
        
        # Delete all the selected rows
        local_rv$populationModules[local_rv$selectedRows] <- NULL
        local_rv$selectedRows <- NULL
      })
      
      # TODO: Fix bug if delete a row, then create a population and try to delete the same row
      observeEvent(input$deletePopulationClicked, {
        clickedRow <- extractRowNumber(input$deletePopulationClicked)
        
        local_rv$populationModules[[clickedRow]] <- NULL
      })
      
      observeEvent(input$editPopulationClicked, {
        clickedRow <- extractRowNumber(input$editPopulationClicked)
        
        # Somewhat ugly way to get the div id from the population to show
        editModuleId <- paste0(
          local_rv$populationModules[[clickedRow]]$id,
          "-editSubpopulationModule")
        
        shinyjs::show(id = editModuleId)
      })
      
      output$populationTableUi <- renderUI({
        dl <- self$execInput("datalist")
        
        if(length(local_rv$populations) > 0) {
          shinyjs::removeClass('createPopulation', "expand-pulse")
          
          DT::dataTableOutput(self$ns('populationTable'))
        } else {
          # Make the create population button pulse
          shinyjs::addClass('createPopulation', "expand-pulse")
          p("No populations found. Press the button in the lower left to create one.")
        }
      })
      
      output$populationTable <- DT::renderDataTable({
        # Example table
        datatableControls(local_rv$renderDf, 'controls', self$ns,
                          colnames = c(
                            "Population Name", "Variables", "Patient Count"),
                          selection = 'none',
                          rownames = FALSE,
                          options = list(
                            ordering=F,
                            dom = 't'
                          )
        )
      })
      
      output$populationBadgeUi <- renderUI({
        pops_length <- length(local_rv$populations)
        if(pops_length == 0) {
          badge_text <- "No populations"
        } else {
          badge_text <- paste0(pops_length, " population", ifelse(pops_length == 1, "", "s"))
        }
        
        bs4Badge(badge_text, status = "primary")
      })
      
      self$assignPort({
        
        self$updateOutputPort(
          id = "populations",
          output = reactive({local_rv$populations}))
      })
      
      return(reactive({local_rv$populations}))
    }
  ),
  private = list(
    # Private methods
  )
)