library(bs4Dash)
library(subpat)
library(tidymodules)

tableListMod <- TableListing$new()

ui <- tagList(
  shinyjs::useShinyjs(),
  bs4DashPage(
    sidebar = bs4DashSidebar(disable = TRUE),
    
    body = bs4DashBody(
      tableListMod$ui()
    )
  )
)

# Define server logic
server <- function(input, output) {
  opts <- reactive(list(
    idvar = "USUBJID"
  ))
  
  tableListMod$callModule()
  
  observe({
    reactive(example_datasets) %>1% tableListMod
    reactive(example_populations) %>2% tableListMod
    opts %>3% tableListMod
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

