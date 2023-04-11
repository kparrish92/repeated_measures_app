## app.R ##
library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    actionButton(inputId = "btn_data", label = "Download"),
    conditionalPanel(condition = "output.setupComplete",
                     box( title = "box1" ),
                     box( title = "box2" ),
                     box( title = "boc3" )
    ),
    conditionalPanel(condition = "!output.setupComplete",
                     box( title = "loading"))
  )
)

server <- function(input, output) { 
  
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  
  ## simulate data load
  observe({
    
    if(input$btn_data){
      
      df <- data.frame(id = seq(1,200),
                       val = rnorm(200, 0, 1))
      
      ## Simulate the data load
      Sys.sleep(5)
      ## set my condition to TRUE
      rv$setupComplete <- TRUE
    }
    
    ## the conditional panel reads this output
    output$setupComplete <- reactive({
      return(rv$setupComplete)
    })
    outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
    
  })
}

shinyApp(ui, server)