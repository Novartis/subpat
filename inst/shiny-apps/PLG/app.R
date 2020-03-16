library(tidymodules)
library(subpat)
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
              fileInput("fileupload", "Choose SAS Files",
                        accept = c(".sas7bdat"),
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
  
  data_reactive <- reactive({
    req(input$fileupload)
    datalist <- lapply(input$fileupload$datapath, haven::read_sas)
    # Make the names of the data sets the name of the file without extension
    names(datalist) <- toupper(tools::file_path_sans_ext(basename(input$fileupload$name)))
    print(names(datalist))
    datalist
  })
    
  observe({
    # Pass in the options
    opts %>2% subpopulationModule
    opts %>3% tableListingModule
    
    # Pass the data set into the subpopulation module
    # Then pass the populations into the variable selection module
    data_reactive %>1% subpopulationModule %1>2% tableListingModule
    
    # Pass the data sets into the variable selection module
    data_reactive %>1% tableListingModule
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

