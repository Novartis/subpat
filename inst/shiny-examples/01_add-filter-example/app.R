library(subpat)


addFilter1 <- Filter$new()

ui <- tagList(
  shinyjs::useShinyjs(),
  bs4DashPage(
    sidebar = bs4DashSidebar(disable = TRUE),
    
    body = bs4DashBody(
      bs4Accordion(id = 'filterAccordion',
                   addFilter1$ui()),
      
      verbatimTextOutput('filter_results'),
      verbatimTextOutput('showPatients')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  datalist <- reactive({
    example_datasets
  })
  
  opts <- reactive(list(
    subjectDs = "ADSL",
    idvar = "USUBJID"
  ))
  
  addFilter1$callModule()
  
  observe({
    # Add the options to the filter module
    opts %>2% addFilter1
    # Add the data to the filter module
    datalist %>1% addFilter1
  })
  
  output$filter_results <- renderPrint({
    str(mod(1)$execOutput("filter"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

