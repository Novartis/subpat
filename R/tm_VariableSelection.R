#' Variable Selection Module
#' 
#' Shiny \code{\link[tidymodules]{TidyModule}} to select variables from a data set. Includes exporting functionality (to HTML). Interface uses bs4Dash.
#' 
#' @family tidymodules
#' @format R6 class
#' @importFrom R6 R6Class
#' @export
#' @keywords TidyModule
#' @section Input:
#' \describe{
#'   \item{\code{dataset}}{A data sets. Can be tibble, data frame, etc.}
#'   \item{\code{options }}{
#'     The options for the module of type \code{\link[shiny]{reactiveValues}} with values
#'     \describe{
#'     \item{\code{datasetname}}{The default dataset name that will appear in the header. Default: \code{""}}
#'     \item{\code{idvar}}{ID field that is common across all datasets in \code{input:datalist}. Default \code{USUBJID}}
#'     }
#'   }
#' }
#' @section Output:
#' \describe{
#'   \item{\code{filtered_indices}}{The output data set indices after filtering, selecting  and reordering columns.}
#'   \item{\code{filtered_data}}{The output data set after filtering, selecting and reordering columns.}
#'   \item{\code{filtered_columns}}{The names of the columns that have been selected.}
#'   \item{\code{search_columns}}{The searches applied to the data table.}
#' }
#' @examples
#' \dontrun{
#' library(tidymodules)
#' library(bs4Dash)
#' library(subpat)
#' 
#' varSelMod <- VariableSelection$new()
#' 
#' ui <- tagList(
#'   shinyjs::useShinyjs(),
#'   bs4DashPage(
#'     sidebar = bs4DashSidebar(disable = TRUE),
#'     
#'     body = bs4DashBody(
#'       fileInput("file1", "Choose SAS File",
#'                 multiple = FALSE,
#'                 accept = c(".sas7bdat")),
#'       
#'       conditionalPanel(condition = 'output.file_uploaded',
#'                        varSelMod$ui(),
#'                        verbatimTextOutput('tableIndices'),
#'                        verbatimTextOutput('filteredTable'),
#'                        verbatimTextOutput('searchColumns'))
#'     )
#'   )
#' )
#' 
#' # Define server logic
#' server <- function(input, output) {
#'   opts <- reactiveValues(
#'     datasetname = NULL,
#'     idvar = "USUBJID"
#'   )
#'   
#'   varSelMod$callModule()
#'   
#'   output$file_uploaded <- reactive({
#'     return(!is.null(input$file1))
#'   })
#'   
#'   outputOptions(output, 'file_uploaded', suspendWhenHidden=FALSE)
#'   
#'   observe({
#'     req(input$file1)
#'     dat <- haven::read_sas(input$file1$datapath)
#'     opts$datasetname <- input$file1$name
#'     
#'     # Pass the uploaded data into the module
#'     reactive(dat) %>1% varSelMod
#'     # Pass the options into the module
#'     opts %>2% varSelMod
#'     
#'   })
#'   
#'   # Show the output indices from the module
#'   output$tableIndices <- renderPrint({
#'     print(varSelMod$getOutput("filtered_indices")())
#'   })
#'   
#'   # Show the filtered data from the table
#'   output$filteredTable <- renderPrint({
#'     print(varSelMod$getOutput("filtered_data")())
#'   })
#'   
#'   # Show the strings entered in the search columns
#'   output$searchColumns <- renderPrint({
#'     print(varSelMod$getOutput("search_columns")())
#'   })
#' }
#' 
#' # Run the application 
#' shinyApp(ui = ui, server = server)
#' 
#' }
VariableSelection <- R6Class(
  "VariableSelection", 
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...){
      super$initialize(...)
      
      self$definePort({
        self$addInputPort(
          name = "dataset",
          description = "A data set (data frame, tibble)",
          sample = example_datasets[[1]],
          input = NULL)
        
        self$addInputPort(
          name = "options",
          description = "Any reactive options",
          sample = list(
            datasetname = "My dataset name. Will appear in header",
            idvar = c("USUBJID", "STYSID1A")
          ),
          input = list(
            datasetname = "",
            idvar = c("USUBJID", "STYSID1A")
          )
        )
        
        self$addOutputPort(
          name = "filtered_indices",
          description = "The output data set indices after filtering, selecting  and reordering columns.",
          sample = c(1,3, 5)
        )
        
        self$addOutputPort(
          name = "filtered_data",
          description = "The output data set after filtering, selecting and reordering columns.
          Always includes USUBJID",
          sample = example_datasets[[1]][c(1,3, 5), c(1,2)]
        )
        
        self$addOutputPort(
          name = "filtered_columns",
          description = "The names of the columns that have been selected",
          sample = c("USUBJID", "AGE")
        )
        
        self$addOutputPort(
          name = "search_columns",
          description = "The searches applied to the data table",
          sample = list()
        )
      })
    },
    ui = function() {
     
      bs4AccordionItemExpanded(
        id = self$ns("varselAccordItem"),
        status = 'default',
        expanded = TRUE,
        title = uiOutput(self$ns('cardTitle')),
        fluidRow(
          column(
            width = 6,
            uiOutput(self$ns('variableSelectUI'))),
          column(
            width = 3,
            style = "margin-top:25px",
            downloadButton(self$ns('downloadData')),
            shinyjs::hidden(div(id = self$ns('progBar'),
                                myLoading())))
        ),
        
        shinycssloaders::withSpinner(DT::dataTableOutput(self$ns('datatablePreview')))
      )
    },
    server = function(input, output, session, 
                      dataset = NULL,
                      options = NULL){
      
      super$server(input,output,session)
      
      self$assignPort({
        self$updateInputPort(
          id = "dataset",
          input = dataset)
        
        if(!is.null(options)) {
          self$updateInputPort(
            id = "options",
            input = options
          )
        }
      })
      
      local_rv <- reactiveValues(
        # The dataset
        dataset = NULL,
        # The data table to display
        datatable = NULL,
        # The data that is used for the data table
        datatable_df = NULL,
        datasetname = NULL,
        selectedVariables = NULL,
        # If we need different behavior for initialization
        # Here we want USUBJID to be the default first variable selected
        init = TRUE
      )
      
      observe({
        o <- self$execInput("options")
        local_rv$dataset<- self$execInput("dataset")
        
        if(o$idvar %in% colnames(local_rv$dataset)) {
          # Start with the id variable as default variable selection
          local_rv$selectedVariables <- o$idvar
        }
      })
      
      observeEvent(input$variableSelect, {
        o <- self$execInput("options")
        
        if(local_rv$init) {
          local_rv$init <- FALSE
        } else {
          local_rv$selectedVariables <- union(o$idvar, input$variableSelect)
        }
      }, ignoreNULL = FALSE)
      
      observeEvent({
        local_rv$dataset
        local_rv$selectedVariables
      }, {
        o <- self$execInput("options")
        
        if(is.null(local_rv$selectedVariables)) {
          # Start with only showing USUBJID
          if(o$idvar %in% colnames(local_rv$dataset)) {
            # Only select the id variable
            dat <- local_rv$dataset[, o$idvar, drop = FALSE]
          } else {
            # Show the entire data set if no variables are selected
            # and we do not have USUBJID in the dataset
            dat <- local_rv$dataset
          }
        } else {
          dat <- local_rv$dataset[local_rv$selectedVariables]
        }
        
        # Limit the results?
        #  dat <- head(dat, n = 250)
        
        req(dat)
        
        if(o$idvar %in% local_rv$selectedVariables) {
          local_rv$datatable_df <- dat
        } else {
          local_rv$datatable_df <- cbind(local_rv$dataset[, o$idvar, drop = FALSE], dat)
        }
        
        local_rv$datatable <- DT::datatable(
          dat,
          rownames = FALSE,
          # extension = 'ColReorder',
          selection = 'none',
          filter = 'top',
          options = list(
            # colReorder = TRUE,
            scrollX = TRUE
          )
        )
      }, ignoreNULL = FALSE)
      
      observe({
        o <- self$execInput("options")
        local_rv$datasetname <- o$datasetname %||% "Variable selection"
      })
      
      # Reactive for the searches applied to each of the columns
      # list(col1 = "10..20", col2 = "", ...)
      search_columns <- reactive({
        req(input$datatablePreview_search_columns)
        res <- input$datatablePreview_search_columns
        names(res) <- colnames(local_rv$datatable_df)
        res
      })
      
      output$downloadData <- downloadHandler(
        filename = "listing.html",
        content = function(file) {
          shinyjs::show(self$ns('progBar'))
          tempReport <- file.path(tempdir(), "listing.html")
          print(tempReport)
          if(is.null(local_rv$selectedVariables)) {
            dat <- local_rv$dataset
          } else {
            dat <- dplyr::select(local_rv$dataset, !!local_rv$selectedVariables)
          }
          
          # Only select the shown rows
          dat_sub <- dat[input$datatablePreview_rows_all, ]
          
          # Summary of the filters
          # Show dataset name, along with the data table filters
          # TODO: Add the population name and meta data
          caption <- paste0("Data set name: ", local_rv$datasetname)
          if(!all(input$datatablePreview_search_columns == "")) {
            # Connects the variable name, along with any filter that are applied to the columns
            dt_info <- paste(local_rv$selectedVariables, input$datatablePreview_search_columns, sep = " = ")
            # Remove the variable names not filtered
            dt_info <- dt_info[input$datatablePreview_search_columns != ""]
            
            caption <- paste(caption, dt_info, sep = ", ")
          }
          
          kb <- knitr::kable(dat_sub, format = "html", caption = caption) %>%
            kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T) %>%
            kableExtra::save_kable(file = tempReport, self_contained = T)
          
          file.copy(tempReport, file, overwrite = TRUE)
          shinyjs::hide(self$ns('progBar'))
          
        }
      )
      
      output$cardTitle <- renderUI({
        req(local_rv$datasetname)
        o <- self$execInput("options")
        
        fluidRow(
          column(
            width = 3,
            local_rv$datasetname
          ),
          column(
            width = 6,
            if(!is.null(local_rv$selectedVariables)) {
              # Show the column labels in the header
              
              colLabs <- columnLabels(local_rv$dataset, removeId = o$idvar)
              if(! all(is.na(colLabs[local_rv$selectedVariables]))) {
                res <- Filter(Negate(is.na), colLabs[local_rv$selectedVariables])
                lapply(res, bs4Badge, position = "left", status = "primary")
              }
            }
          )
        )
      })
      
      output$variableSelectUI <- renderUI({
        o <- self$execInput("options")
        
        colLabs <- columnLabelsInvert(local_rv$dataset, removeId = o$idvar)
        
        selectizeInput(inputId = self$ns('variableSelect'),
                       label = "Select variables",
                       multiple = T,
                       width = "100%",
                       choices = colLabs,
                       options = list(
                         plugins = list('drag_drop', 'remove_button')
                       ))
      })
      
      output$datatablePreview <- DT::renderDataTable({
        # TODO: Look into if DT::replaceData could help
        # https://github.com/rstudio/DT/issues/260
        local_rv$datatable
      })
      
      
      self$assignPort({
        self$updateOutputPort(
          id = "filtered_indices",
          output = reactive(input$datatablePreview_rows_all))
        
        self$updateOutputPort(
          id = "filtered_data",
          output = reactive({
            o <- self$execInput("options")
            
            if(is.null(local_rv$datatable_df) || is.null(input$datatablePreview_rows_all)) {
              # Always add back in USUBJID
              varSel <- union(o$idvar, local_rv$selectedVariables)
              local_rv$dataset[varSel]
            } else {
              local_rv$datatable_df[input$datatablePreview_rows_all, ]
            }
          })
        )
        
        self$updateOutputPort(
          id = "filtered_columns",
          output = reactive(local_rv$selectedVariables)
        )
        
        self$updateOutputPort(
          id = "search_columns",
          output = search_columns
        )
      })
    }
  ),
  private = list(
    # Private methods
  )
)