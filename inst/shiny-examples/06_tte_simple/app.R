library(shiny)
library(subpat)
library(tidymodules)

tteModule <- TTE$new()

ui <- fluidPage(
  h2("Time-to-event Analysis of NCCTG Lung Cancer Data"),
  tteModule$standardUi()
)

server <- function(input, output, session) {

  # Set the variable names to use in the model
  mapping <- reactive({
    list(
      time = 'time',
      event = 'status',
      treatment = 'sex',
      # Conversion factor: makes the time number of months.
      aval_conv = 30.34,
      # Included are some other parameters that we can set
      # For this example, we set them to null
      # The comments show some common values ADaM data sets
      strata = NULL, # c('STRVAL1', 'STRVAL2'),
      parameter = NULL, # 'PARAM',
      parameter_value = NULL, # 'Progression free Survival',
      population = NULL# "FASFL"
    )
  })

  # Here are some general options for the module
  options <- reactive(list(
    # Do you want the graph interactive?
    makePlotly = FALSE,
    # Confidence interval type
    # Options: log-log (SAS default), "log", "none", "plain"
    # See help on survival::survfit.formula for more information
    conftype = "log-log"
  ))

  # We need to call the callModule() method of the tidymodule
  tteModule$callModule()

  # Here you want to load your data in some way
  # In this example, we just wrap the `lung` dataset from the survival package
  data_reactive <- reactive({
    survival::lung
  })

  observe({
    # Pass the data into the TTE module
    # The pipe %>1% means pass the reactive into the first input of TTE module (the data)
    data_reactive %>1% tteModule
    # Pass the mapping into the TTE module
    # The pipe %>2% means pass the reactive into the second input of the module
    mapping %>2% tteModule
    # Pass our options into the TTE module
    options %>4% tteModule
  })
}

# Run the application
shinyApp(ui = ui, server = server)

