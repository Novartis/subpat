library(subpat)
library(tidymodules)
library(bs4Dash)

subpopulationModule <- SubpopulationManager$new()
tableListingModule <- TableListing$new()

store <- Store$new()

ui <- tagList(
    shinyjs::useShinyjs(),
    bs4DashPage(
      navbar = bs4DashNavbar(),
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        opacity = 1,
        
        bs4SidebarMenu(id="patlist",
                       bs4SidebarHeader("Patient Listing Generator"),
                       bs4SidebarMenuItem(
                         "Load Data",
                         tabName = "loadData",
                         icon = "database"
                       ),
                       bs4SidebarMenuItem(
                         "Patient Populations",
                         tabName = "query",
                         icon = "users"
                       ),
                       bs4SidebarMenuItem(
                         "Table Listings",
                         tabName = "listing",
                         icon = "id-card"
                       ),
                       bs4SidebarMenuItem(
                         "ModStore",
                         tabName = "modStore",
                         icon = "project-diagram"
                       )
        )
      ),
      body = bs4DashBody(
          bs4TabItems(
            bs4TabItem(
              tabName = "loadData",
              actionButton("examplebutton", label = "Use example data"),
              fileInput("fileupload", "Choose .xpt Files",
                        accept = c(".xpt"),
                        multiple = TRUE
              ),
              bs4Alert(
                title = "Disclaimer",
                status = "danger",
                closable = TRUE,
                "This package is NOT VALIDATED, and should be reserved for exploratory analysis only."
              )
            ),
            bs4TabItem(
              tabName = "query",
              fluidPage(
                subpopulationModule$ui()
              )
            ),
            bs4TabItem(
              tabName = "listing",
              fluidPage(
                tableListingModule$ui()
              )
            )
            ,
            bs4TabItem(
              tabName = "modStore",
              store$ui()
            )
          )
        )
      )
    )

server <- function(input, output, session){
  
  # Patient subpopulation module
  callModules()
  
  opts <- reactive(
    list(
      subjectDs = "ADSL",
      idvar = "USUBJID"
    )
  )
  
  v <- reactiveValues(datalist = NULL)
  
  data_reactive <- reactive({
    req(input$fileupload)
   
    print(names(datalist))
    datalist
  })
  
  observeEvent(input$fileupload, {
    datalist <- lapply(input$fileupload$datapath, haven::read_xpt)
    # Make the names of the data sets the name of the file without extension
    names(datalist) <- toupper(tools::file_path_sans_ext(basename(input$fileupload$name)))
    v$datalist <- datalist
  })
  
  observeEvent(input$examplebutton, {
    path <- "sample_data/cdisc"
    
    ex_files <- list.files(system.file(path, package = "subpat"), full.names = TRUE)
    v$datalist <- lapply(ex_files, haven::read_xpt)
    names(v$datalist) <- toupper(tools::file_path_sans_ext(basename(ex_files)))
    
    v$message <- "Example files loaded"
  })
  
    
  observe({
    # Pass in the options
    opts %>2% subpopulationModule
    opts %>3% tableListingModule
    
    # Pass the data set into the subpopulation module
    # Then pass the populations into the variable selection module
    reactive(v$datalist) %>1% subpopulationModule %1>2% tableListingModule
    
    # Pass the data sets into the variable selection module
    reactive(v$datalist) %>1% tableListingModule
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

