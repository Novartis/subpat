#' Subgroup module creates derived variables based on numeric variables.
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
#'     \item{\code{showLabels}}{Boolean: Do you want the variable labels to show? Generally from \code{haven::read_sas} or \code{Hmisc::labels}}
#'     \item{\code{isvar}}{Boolean: should we show the id variable selector?}
#'     }
#'   }
#' }
#' @section Output:
#' \describe{
#' \item{\code{subgroups}}{List of each of the subgroups. Can call as.data.frame on the returned subgroups to create a data frame}
#' }
#' @examples 
#' \dontrun{
#' library(shiny)
#' library(subpat)
#' library(bs4Dash)
#' library(tidymodules)
#' 
#' subgroupManagerModule <- SubgroupManager$new()
#' 
#' ui <- tagList(
#'   shinyjs::useShinyjs(),
#'   bs4DashPage(
#'     sidebar = bs4DashSidebar(disable = TRUE),
#'     
#'     body = bs4DashBody(
#'       subgroupManagerModule$ui(),
#'       verbatimTextOutput('subgroupSummary')
#'     )
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   
#'   subgroupManagerModule$callModule()
#'   
#'   observe({
#'     reactive(mtcars) %>1% subgroupManagerModule
#'   })
#'   
#'   output$subgroupSummary <- renderPrint({
#'     print("Subgroups:")
#'     print(summary(subgroupManagerModule$getOutput("subgroups")()))
#'   })
#' }
#' 
# # Run the application
# shinyApp(ui = ui, server = server)
#' }
SubgroupManager <- R6::R6Class(
  "SubgroupManager",
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...){
      # Mandatory
      super$initialize(...)

      self$definePort({
        # At least one input
        self$addInputPort(
          name = "data",
          description = "A data set (data frame, tibble, etc.) for the subgroup creation",
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
          description = "reactiveValues with values
          showLabel - Boolean to indicate whether to show variable labels
          idvar - Boolean: Should we show an ID variable selector?",
          inherit = FALSE,
          sample = list(
            showLabels = TRUE,
            idvar = NULL
          ))

        self$addOutputPort(
          name = "subgroups",
          description = "The subgroups",
          sample = list())
      })

    },
    ui = function() {
      tagList(
        h3("Create subgroups"),
        fluidRow(
          uiOutput(self$ns('subgroupList'))
        ),
        fluidRow(
          uiOutput(self$ns("selectIdUi")),
          column(
            width = 3,
            actionButton(self$ns("newSubgroup"), label = "Create new subgroup")
          )
        ),
        div(id = self$ns("subgroupModules"))
      )
    },
    server = function(input, output, session){
      # Mandatory
      super$server(input,output,session)

      local_rv <- reactiveValues(
        subgroupModules = list(),
        subgroupIndex = 1
      )

      data_reactive <- reactive({
        self$execInput("data")
      })

      observeEvent(input$newSubgroup, {
        local_rv$subgroupModules[[local_rv$subgroupIndex]] <- Subgroup$new()
        # Pass the index into the subgroup module to set the name to: New subgroup 1,2,3, etc
        reactive(
          list(
            index = local_rv$subgroupIndex,
            subjectid = input$idvar
          )
        ) %>3% local_rv$subgroupModules[[local_rv$subgroupIndex]]
        
        local_rv$subgroupModules[[local_rv$subgroupIndex]]$callModule()
        
        insertUI(
          selector = paste0("#", self$ns("subgroupModules")),
          ui = local_rv$subgroupModules[[local_rv$subgroupIndex]]$ui(),
          session = self$getShinySession())

        local_rv$subgroupIndex <- local_rv$subgroupIndex + 1
      })

      subgroups <- reactive({
        sgroups <- lapply(local_rv$subgroupModules, function(sm) {
          sm$execOutput("subgroup")
        })
        names(sgroups) <- unlist(
          lapply(local_rv$subgroupModules, function(sm) {
            sm$execOutput("subgroup_name")
          })
        )

        sgroups
      })
      
      subgroups_df <- reactive({
        as.data.frame(sgroups())
      })
      
      output$subgroupList <- renderUI({
        sgroups <- subgroups()
        if(length(sgroups) > 0) {
          div(id = "subgroup_list",
                  lapply(names(sgroups), function(x) {
                    sg_summary <-  summary(sgroups[[x]])
                    
                    div(id = self$ns(x),
                        x,
                        lapply(names(sg_summary), function(v) {
                          div(paste0(v, " Count: ", sg_summary[[v]]))
                        })
                    )
                  }))
        } else {
          p("No subgroups found")
        }
      })

      output$selectIdUi <- renderUI({
        o <- self$execInput("options")
        req(o$idvar)
        d <- data_reactive()
        if(is.null(o$showLabels) || o$showLabels) {
          res <- columnLabelsInvert(d)
        } else {
          res <- colnames(d)
        }

        if("USUBJID" %in% res) {
          choices <- res[grepl("USUBJID", res)]
        } else {
          choices <- res[grepl("ID", res, ignore.case = TRUE)]
        }
        
        column(
          width = 3,
          selectInput(self$ns('idvar'), 'Subject ID', choices = choices)
        )
      })

      # Ports assignment starts here
      self$assignPort({
        self$updateOutputPort(
          id = "subgroups",
          output = subgroups)

      })

      return(subgroups)
    }
  ),
  private = list(
    # any private functions?
  )
)
