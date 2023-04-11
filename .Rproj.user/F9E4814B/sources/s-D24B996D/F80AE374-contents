library(shiny)
library(tidyverse)
library(here)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sampling Error Visualization: within subjects"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Choose n",
                  min = 1,
                  max = 100,
                  value = 20)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
      data = read.csv(here("data", "data.csv"))
      k = 100
      
      mean_df <- matrix(nrow = k, ncol = 2)
      
      for(thisRun in 1:k){
        sam <- data[sample(nrow(data), input$bins), ]
        mean_df[thisRun, 1] = mean(sam$vot)
        mean_df[thisRun, 2] = sd(sam$vot)
      }
      
      mean_df_df = mean_df%>% 
        as.data.frame() %>% 
        rename(mean = V1) %>% 
        rename(sd = V2)
      
      df = data.frame(mean = 60, sd = 20) 
      
      
      mean_df_df %>% 
        ggplot(aes(x = mean, y = sd)) + 
        geom_point(fill = "darkturquoise", colour="black",pch=21, size=3) + 
        xlim(50, 70) + 
        ylim(15, 25) + geom_point(data = df, 
                                  fill = "red", 
                                  colour="black",pch=21, size=3)
      
      
      
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
