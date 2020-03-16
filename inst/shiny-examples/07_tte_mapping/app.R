library(shiny)
library(subpat)
library(tidymodules)

tteMappingModule <- TTEMapping$new()
tteModule <- TTE$new()

ui <- fluidPage(
  titlePanel("TTE Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('dataset', 'survival dataset', choices = data(package = "survival")$results[, "Item"], selected = "lung"),
      tteMappingModule$ui()
    ),
    mainPanel(
      # Use the base shiny UI
      tteModule$standardUi()
    )
  )
)

server <- function(input, output, session) {
  
  callModules()
  
  options <- reactive(list(
    makePlotly = FALSE,
    conftype = "log-log"
  ))
  
  optionsMapping <- reactive(list(
    population = FALSE,
    parameter = FALSE,
    parameter_value = FALSE,
    adam = FALSE
  ))
  
  # Load the data set from the survival package
  data_reactive <- reactive({
    req(input$dataset)
    ds <- trimws(gsub("\\(.*\\)", "", input$dataset))
    data(list = ds, package = "survival")
    get(ds)
  })
  
  observe({
    data_reactive %>1% tteModule
    options %>4% tteModule
    # Get the mapping and pass into the TTE module
    data_reactive %>1% tteMappingModule %1>2% tteModule
    optionsMapping %>2% tteMappingModule
  })
}

# Run the application
shinyApp(ui = ui, server = server)