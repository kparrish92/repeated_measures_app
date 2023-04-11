
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
#

# Load packages
library("shiny")
library("ggplot2")
library("tidyr")
library("dplyr")
library("broom")
library("patchwork")

# Set theme for ggplot
param_theme <- function(...) {
  list(
    theme_bw(...), 
    theme(
      panel.grid.major = element_line(size = 0.2),
      panel.grid.minor = element_line(size = 0.2)
    )
  )
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(""),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 withMathJax(),
                 p("$$y_i \\sim Normal(\\mu_i, \\sigma)$$"), 
                 p("$$\\mu_i = \\alpha + \\beta x_i$$"), 
                 br(), 
                 sliderInput(
                   inputId = "b_0",
                   label = "Intercept",
                   min = -1.5, max = 1.5, value = 0, step = 0.1, ticks = F), 
                 sliderInput(
                   inputId = "b_1",
                   label = "Slope",
                   min = -1.5, max = 1.5, value = 0.5, step = 0.1, ticks = F),
                 #sliderInput(
                 #  inputId = "sigma",
                 #  label = "Sigma",
                 #  min = 0.1, max = 2, value = 1, step = 0.1, ticks = F),
                 #sliderInput(
                 #  inputId = "n",
                 #  label = "N",
                 #  min = 25, max = 500, value = 25, step = 1, ticks = F), 
                 br(), 
                 p(strong("Created by:"), 
                   tags$a("Joseph V. Casillas", href="https://www.jvcasillas.com"),
                   br(), 
                   strong("Source code:"), 
                   tags$a("Github", href="https://github.com/jvcasillas/shiny_parameters/"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9, br(), 
              plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate data
    #dat <- tibble(
    #  x = rnorm(input$n, 0, 1), 
    #  y = input$b_0 + x * input$b_1 + rnorm(input$n, 0, input$sigma)
    #)
    
    
    g1_100 = data.frame(vot = rnorm(100, 60, 25), participant = 1:100) %>% 
      mutate(group = "g1") %>% 
      mutate(sample = "n = 100")
    
    g2_100 = data.frame(vot = rnorm(100, 60, 25), participant = 1:100) %>% 
      mutate(group = "g2") %>% 
      mutate(sample = "n = 100")
    
    g1_10 = data.frame(vot = rnorm(20, 60, 25), participant = 1:100) %>% 
      mutate(group = "g1") %>% 
      mutate(sample = "n = 20")
    g2_10 = data.frame(vot = rnorm(20, 60, 25), participant = 1:100) %>% 
      mutate(group = "g2") %>% 
      mutate(sample = "n = 20")
    
    full_df = rbind(g1_10, g2_10, g1_100, g2_100) 
    
    # Data space plot
    p1 = full_df %>% 
      ggplot(aes(x = vot, y = group)) + geom_boxplot() + facet_grid(~ sample)
    p1
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
