library(tidymodules)
library(subpat)
library(bs4Dash)

# Create a new population edit module
populationMod <- Subpopulation$new(id = "populationMod")

ui <- tagList(
  shinyjs::useShinyjs(),
    bs4DashPage(
    sidebar = bs4DashSidebar(disable = TRUE),
    body = bs4DashBody(
      populationMod$ui(),
      
      fluidRow(
        column(width = 6, 
               p("Editing population"),
               verbatimTextOutput('editing_population')),
        column(width = 6,
               p("Saved object"),
               verbatimTextOutput('saved_population'))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  datalist <- reactive({
    list(
      "ADSL" = data.frame(
        a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
        b = c(29L, 4L, 15L, 28L, 41L, 25L, 41L, 15L, 6L, 17L),
        USUBJID = as.character(seq_len(10)),
        stringsAsFactors = FALSE
      ),
      "ADAE" = data.frame(
        a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
        c = c(1, NA, NA, 1, 2, NA, NA, 4, 3, 1),
        dates = seq(as.Date("2019-07-10"), as.Date("2019-08-12"), length.out = 10),
        USUBJID = as.character(seq_len(10)),
        stringsAsFactors = FALSE
      )
    )
  })
  
  populationMod$callModule()
  
  opts <- reactive(list(
    hideOnSave = FALSE,
    populationNumber = 1,
    subjectDs = "ADSL",
    idvar = "USUBJID"
  ))
  
  observe({
    # Add the data to the module
    datalist %>1% populationMod
    # Add the options to the population editing module
    opts %>2% populationMod
  })
  
  output$saved_population <- renderPrint({
    str(mod("populationMod")$execOutput("savePopulation"))
  })
  
  output$editing_population <- renderPrint({
    str(mod("populationMod")$execOutput("editingPopulation"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)