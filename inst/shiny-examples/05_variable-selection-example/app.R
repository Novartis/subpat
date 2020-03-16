library(tidymodules)
library(bs4Dash)
library(subpat)

varSelMod <- VariableSelection$new()

ui <- tagList(
  shinyjs::useShinyjs(),
  bs4DashPage(
    sidebar = bs4DashSidebar(disable = TRUE),
    
    body = bs4DashBody(
      fileInput("file1", "Choose SAS File",
                multiple = FALSE,
                accept = c(".sas7bdat")),
      
      conditionalPanel(condition = 'output.file_uploaded',
                       varSelMod$ui(),
                       verbatimTextOutput('tableIndices'),
                       verbatimTextOutput('filteredTable'),
                       verbatimTextOutput('searchColumns'))
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  varSelMod$callModule()
  
  opts <- reactive(list(
    datasetname = input$file1$name,
    idvar = "USUBJID"
  ))
  
  dat <- reactive({
    req(input$file1)
    haven::read_sas(input$file1$datapath)
  })
  
  output$file_uploaded <- reactive({
    return(!is.null(input$file1))
  })
  
  outputOptions(output, 'file_uploaded', suspendWhenHidden=FALSE)
  
  observe({
    # Pass the uploaded data into the module
    dat %>1% varSelMod
    # Pass the options into the module
    opts %>2% varSelMod
  })
  
  # Show the output indices from the module
  output$tableIndices <- renderPrint({
    print(mod(1)$execOutput("filtered_indices"))
  })
  
  # Show the filtered data from the table
  output$filteredTable <- renderPrint({
    print(mod(1)$execOutput("filtered_data"))
  })
  
  # Show the strings entered in the search columns
  output$searchColumns <- renderPrint({
    print(mod(1)$execOutput("search_columns"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

