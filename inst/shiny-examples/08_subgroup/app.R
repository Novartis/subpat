library(shiny)
library(subpat)
library(bs4Dash)
library(tidymodules)

subgroupManagerModule <- SubgroupManager$new()

ui <- tagList(
  shinyjs::useShinyjs(),
  bs4DashPage(
    sidebar = bs4DashSidebar(disable = TRUE),
    
    body = bs4DashBody(
      subgroupManagerModule$ui(),
      verbatimTextOutput('subgroupSummary')
    )
  )
)

server <- function(input, output, session) {
  
  subgroupManagerModule$callModule()
  
  observe({
    reactive(mtcars) %>1% subgroupManagerModule
  })
  
  output$subgroupSummary <- renderPrint({
    print("Subgroups:")
    print(summary(mod(1)$execOutput("subgroups")))
  })
}

# Run the application
shinyApp(ui = ui, server = server)