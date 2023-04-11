library(shiny)
library(tidyverse)

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
    
    sample_1 = data[sample(nrow(data), input$bins), ] %>% 
      mutate(group = "g1") %>% 
      mutate(sample = "Language 1")
    
    mean1 = round(mean(sample_1$vot), digits = 1)
    sd1 = round(sd(sample_1$vot), digits = 1)  
    
    sample_2 = data[sample(nrow(data), input$bins), ] %>% 
      mutate(group = "g2") %>% 
      mutate(sample = "Language 2")
    
    mean2 = round(mean(sample_2$vot), digits = 1)
    sd2 = round(sd(sample_2$vot), digits = 1) 
    
    full = rbind(sample_1, sample_2) 
    full %>% 
      ggplot(aes(x = vot, y = sample)) + 
      geom_boxplot() + 
      annotate("text", 
               x = 20, 
               y = .9, 
               label = mean1) + xlim(15, 100) +
      annotate("text", 
               x = 20, 
               y = 2.1, 
               label = mean2) +
      annotate("text", 
               x = 20, 
               y = .7, 
               label = sd1) + xlim(15, 100) +
      annotate("text", 
               x = 20, 
               y = 1.9, 
               label = sd2)
  })
  output$stats <- renderPrint({
    head(sample_2)
  })  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
