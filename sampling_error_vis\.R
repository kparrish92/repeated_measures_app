library(shiny)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sampling Error Visualization"),
  
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
    pop_dist = data.frame("vot" = rnorm(n = 100, mean = 60, sd = 20)) %>% 
      mutate("n" = "100")
    dat = data.frame("vot" = rnorm(n = input$bins, mean = 60, sd = 20)) %>% 
      mutate("n" = input$bins)
    full = rbind(pop_dist, dat)
    full %>% 
      ggplot(aes(x = as.factor(n), y = vot)) + 
      geom_point(alpha = .05) + 
      geom_boxplot() + 
      annotate("text", 
               x = .8, 
               y = 25, 
               label = mean(pop_dist$vot)) + 
      annotate("text", 
               x = .8, 
               y = 22, 
               label = sd(pop_dist$vot)) +
      annotate("text", 
               x = 1.6, 
               y = 25, 
               label = mean(dat$vot)) +
      annotate("text", 
               x = 1.6, 
               y = 22, 
               label = sd(dat$vot))
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
