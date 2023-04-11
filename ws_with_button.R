library(shiny)
library(here)
library(tidyverse)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  actionButton(inputId = "go", 
               label = "Update"),
  plotOutput("hist")
)

server <- function(input, output) {
  data <- eventReactive(input$go, {
  })
  
  output$hist <- renderPlot({
    data = read.csv(here("data", "data.csv"))
    
    sample_1 = data[sample(nrow(data), input$go), ] %>% 
      mutate(group = "g1") %>% 
      mutate(sample = "Language 1")
    
    mean1 = round(mean(sample_1$vot), digits = 1)
    sd1 = round(sd(sample_1$vot), digits = 1)  
    
    sample_2 = data[sample(nrow(data), input$go), ] %>% 
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
}

shinyApp(ui = ui, server = server)