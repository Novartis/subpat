library(tidymodules)
library(subpat)
library(bs4Dash)

subpopmod <- SubpopulationManager$new(id = "subpopmodule")

ui <- tagList(
  shinyjs::useShinyjs(),
  bs4DashPage(
    sidebar = bs4DashSidebar(disable = TRUE),
    body = bs4DashBody(
      subpopmod$ui()
    )
  )
)

# Define server logic
server <- function(input, output) {
  datalist <- reactive({
    example_datasets
  })
  
  opts <- reactive(list(
    subjectDs = "ADSL",
    idvar = "USUBJID"
  ))
  
  subpopmod$callModule()
  
  observe({
    # Pass the data into the module
    datalist %>1% subpopmod
    # Add the options to the subpopulation manager
    opts %>2% subpopmod
  })
}

# Run the application 
shinyApp(ui = ui, server = server)