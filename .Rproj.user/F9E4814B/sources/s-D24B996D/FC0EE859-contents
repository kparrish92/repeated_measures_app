
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
library("shinyjs")

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

ui <- fluidPage(
  useShinyjs(),
  div(
    id = "loading_page",
    h1("Loading...")
  ),
  hidden(
    div(
      id = "main_content",
      "Data loaded, content goes here"
    )
  )
)


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
                 textInput(
                   inputId = "mean_u",
                   label = "Mean"), 
                 textInput(
                   inputId = "sd_u",
                   label = "SD"), 
                 textInput(
                   inputId = "es",
                   label = "Effect Size (Cohen's D)"),
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
                   tags$a("Kyle Parrish", href="https://www.kparrish92.github.io"),
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
    
    
    
    sample_function = function(sampling, number_stim)
    {
      power_df = matrix(nrow = 1000, ncol= 9)
      
      
      for (i in 1:1000) {
        
        sample_df = sampling %>%
          sample_n(number_stim) %>%
          mutate(df = "sample")
        
        data_tost = rbind(sampling, sample_df)
        
        t_e = TOSTER::dataTOSTtwo(
          data = data_tost,
          deps = "k",
          group = "df",
          low_eqbound_d = -.4,
          high_eqbound_d = .4,
          desc = TRUE,
          plots = FALSE)
        
        tost = t_e$tost$asDF
        desc = t_e$desc$asDF
        
        power_df[i, 1] = min(tost$`p[1]`, tost$`p[2]`) # p value tost
        power_df[i, 2] = tost$`p[0]`# p value t-test
        power_df[i, 3] = desc$`m[1]`# mean 1 (actual)
        power_df[i, 4] = desc$`m[2]`# mean 2 (sample)
        power_df[i, 5] = desc$`sd[1]`# sd 1 (actual)
        power_df[i, 6] = desc$`sd[2]`# sd 2 (sample)
        power_df[i, 7] = desc$`n[1]`# n1 (actual)
        power_df[i, 8] = desc$`n[2]`# n2 (sample)
        power_df[i, 9] = i # n2 (sample)
        
        
      }
      return(power_df)
    }
    
    
    run_samples = function(sampling_df)
    {
      five = sample_function(sampling_df, 6) %>%
        as.data.frame()
      
      eight = sample_function(sampling_df, 10) %>%
        as.data.frame()
      
      ten = sample_function(sampling_df ,12) %>%
        as.data.frame()
      
      twelve = sample_function(sampling_df, 15) %>%
        as.data.frame()
      
      fifteen = sample_function(sampling_df, 18) %>%
        as.data.frame()
      
      final_df = rbind(five, eight, ten, twelve, fifteen)
      
      return(final_df)
    }
    
    
    sampling_df = data.frame(k = rnorm(n = 1000, mean = mean_u, sd = sd_u)) %>%
      mutate(df = "sampling")
    
    test_data = run_samples(sampling_df)
    
    # Data space plot
    test_data %>%
      group_by(participant, segment, n_actual) %>%
      summarize(n = sum(sig_ttest)) %>%
      ggplot(aes(x = as.factor(n_actual), y = as.numeric(n), fill = segment)) +
      geom_boxplot(outlier.size = 0) +
      ylab("No. false positive") + xlab("Number of stimuli") +
      geom_hline(yintercept = 5, linetype = "dashed", alpha = .4) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      theme(text = element_text(size = 20))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
