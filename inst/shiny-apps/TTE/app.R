library(tidymodules)
library(bs4Dash)
library(dplyr)

library(subpat)

tteMappingModule <- TTEMapping$new()
tteModule <- TTE$new()
# From PLG
subpopulationModule <- SubpopulationManager$new()
subgroupManagerModule <- SubgroupManager$new()

store <- Store$new()

ui <- tagList(
  shinyjs::useShinyjs(),
  bs4DashPage(
    sidebar_collapsed = TRUE,
    controlbar_collapsed = FALSE,
    navbar = bs4DashNavbar(
      
    ),
    controlbar = bs4DashControlbar(
      skin = "light",
      title = "KM Configuration",
      width = 450,
  
      conditionalPanel("output.dataloaded",
         bs4TabSetPanel(
          id = "controlbarTabset",
          side = "left",
          # This NEEDS to be first since the mapping module won't be displayed otherwise
          # The variable values come from the select inputs
          bs4TabPanel(
            tabName = "Variable Mapping",
            active = TRUE,
            tteMappingModule$ui(),
            checkboxInput('useParam', 'Use parameter filtering?', value = T)
          ),
          bs4TabPanel(
            tabName = "Other",
            h3("Model options"),
            # One-sided test support.
            # radioButtons('logrankSided', label = 'Hypothesis Tails',choices = c("one-sided", "two-sided"), inline = TRUE),
            selectInput('conftype', 'Confidence Interval Type', choices = list("log-log (SAS default)" = "log-log", "log" = "log", "plain" = "plain"), selected = "log-log"),
            h3("Plot options"),
            checkboxInput('makePlotly', 'Enable interactivity through Plotly?', value = F)
          )
        )
      ),
      conditionalPanel("! output.dataloaded", p("Please load data first."))
    ),
    sidebar = bs4DashSidebar(
      skin = "light",
      bs4SidebarMenu(
        id = "sidebar",
        bs4SidebarHeader("TTE Analysis"),
        bs4SidebarMenuItem(
          "Load data",
          tabName = "loaddata",
          icon = "table"
        ),
        bs4SidebarMenuItem(
          "Subpopulations",
          tabName = "subpop",
          icon = "users"
        ),
        bs4SidebarMenuItem(
          "Subgroups",
          tabName = "subgroups",
          icon = "table"
        ),
        bs4SidebarMenuItem(
          "KM Summary",
          tabName = "km",
          icon = "chart-bar"
        ),
        bs4SidebarMenuItem(
          "Cox PH Model",
          tabName = "coxph",
          icon = "chart-area"
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
          tabName = "loaddata",
          fluidRow(
            column(
              width = 6,
              p("Using example data from: https://github.com/phuse-org/phuse-scripts/blob/master/data/adam/cdisc/adtte.xpt")
            )#,
            #column(
            #  width = 2,
            #  style = "margin-top: 28px",
            #  actionButton('load', 'Load path')
            #)
          ),
          # fluidRow(
          #   textOutput('fileloaded')
          # )
        ),
        bs4TabItem(
          tabName = "subpop",
          subpopulationModule$ui()
        ),
        bs4TabItem(
          tabName = "subgroups",
          subgroupManagerModule$ui()
        ),
        bs4TabItem(
          tabName = "km",
          # Load the TTE mapping module after data is loaded
  
          conditionalPanel("output.dataloaded",
                           uiOutput('selectPopulationUI'),
                           tteModule$ui()),
          conditionalPanel("! output.dataloaded", p("Please load data first."))
        ),
        bs4TabItem(
          tabName = "coxph",
          conditionalPanel("output.dataloaded",
                           tteModule$coxUi()),
          conditionalPanel("! output.dataloaded", p("Please load data first."))
        ),
        bs4TabItem(
          tabName = "modStore",
          # store$ui()
        )
      )
    )
  )
)

server <- function(input, output, session) {

  options <- reactive(list(
    makePlotly = input$makePlotly,
    conftype = input$conftype,
    parameter = input$useParam,
    parameter_value = input$useParam
  ))

  optionsPlg <- reactive(list(
    subjectDs = NULL,
    idvar = "USUBJID"
  ))

  callModules()

  data_basename <- reactive({
    # Only the filename without path and extension
    # toupper(tools::file_path_sans_ext(basename(input$datapath)))
    "ADTTE"
  })

  data_reactive <- reactive({
    adtte_path <- system.file("sample_data/cdisc_tte/adtte.xpt", package = "subpat")
    haven::read_xpt(adtte_path)
  })#eventReactive(input$load, {
    #haven::read_xpt(input$datapath)
  #})

  data_reactive_pop <- reactive({
    d <- data_reactive()
    if(is.null(input$selectPopulation) || input$selectPopulation == "Full Population") {
      d
    } else {
      plgDataList <- list(x = d)
      # Name the data set with the basename of the file
      names(plgDataList) <- data_basename()
      # Query for the sub-population
      subpat::query(populations()[[input$selectPopulation]], plgDataList)[[1]]
    }
  })

  observe({
    print(summary(mod(4)$execOutput("subgroups")))
  })
  
  plgDataList <- reactive({
    # PLG subpopulation expects a list
    l <- list(x = data_reactive())
    # Name the data set with the basename of the file
    names(l) <- data_basename()
    
    l
  })

  defineEdges({
    
    data_reactive %>1% tteMappingModule
    options %>2% tteMappingModule
    
    data_reactive %>1% subgroupManagerModule
    # Pass the mapping into the Subgroup Module
    tteMappingModule %1>2% subgroupManagerModule
    
    data_reactive_pop %>1% tteModule
    tteMappingModule %1>2% tteModule
    subgroupManagerModule %1>3% tteModule
    options %>4% tteModule
    
    plgDataList %>1% subpopulationModule
    optionsPlg %>2% subpopulationModule
  })

  populations <- reactive({
    mod(3)$execOutput("populations")
  })

  output$selectPopulationUI <- renderUI({
    pop <- populations()
    req(pop, length(pop) > 0)
    # Get the name from each population
    pop_names <- lapply(pop, function(x) x$name)
    choices <- c("Full Population", names(pop_names))
    names(choices) <- c("Full Population", pop_names)
    selectInput('selectPopulation', label = "Select population", choices =choices)
  })

  output$fileloaded <- renderText({
    req(input$datapath, input$load)
    paste0("Loaded file: ", basename(input$datapath))
  })

  output$dataloaded <- reactive({
    dataloaded <- !is.null(data_reactive_pop())

    #if(dataloaded) {
      # Go to the KM Plot tab
    #  shinyjs::runjs("$('#tab-km').click()")
    #}

    return(dataloaded)
  })

  outputOptions(output, 'dataloaded', suspendWhenHidden=FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)

