#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  

    # Application title
    titlePanel("How many tokens do I need?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          p("This purpose of this app is to test whether a given mean, standard deviation, and number of tokens are sufficient (at a power level of .8)
            to produce a sample that is within a small effect size (d = +/- .4) of an assumed underlying distrubution.
            In this case, 'token' refers to a single repetition of a given condition in a repeated measures experiment.
            While this app was created with phonetic research in mind, it can be used to any type of research in which repeated measures are used!"),
            numericInput('mean_input', 'Mean', 10, min = 0, max = Inf),
            numericInput('sd_input', 'Standard Deviation', 2, min = 0, max = Inf),
            sliderInput("n_input", "Number of tokens", min = 1, max = 200, value = 15),
            submitButton("Submit"),
            br(),
            p(strong("Created by:"), 
              tags$a("Kyle Parrish", href="https://kparrish92.github.io"),
              br(), strong("Source code:"), 
              tags$a("Github", href="https://github.com/kparrish92/repeated_measures_app"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          withSpinner(plotOutput("distPlot")),
          p("NOTE: Each time SUBMIT is pressed, the new plot takes up to 2 minutes to load. 
            Input a MEAN, STANDARD DEVIATION and NUMBER OF TOKENS.
            Essentially, this app runs a power analysis 10 times.
            The app simulates an underlying distribution and randomly samples 
            the NUMBER OF TOKENS from it 100 times. In each iteration, the random 
            sample is tested against the underlying distribution using a Test of 
            Equivelance (bounds +/- d = .4). Then, the quantity of significant 
            equivalence tests are added. This process was repeated 10 times and plotted.
            If the power was at least .8 in all 10 power analyses, the boxplot will fall
            to the right of the dashed line. If it does not, you need more tokens!"),
          p(strong("This application was created in tandem with a paper from ICPhs 2023 in Prague"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({

        ul_dist = data.frame(k = rnorm(n = 1000, mean = input$mean_input, 
                                       sd = input$sd_input)) %>%
          mutate(df = "sampling")
        
  
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
              low_eqbound_d = -0.4,
              high_eqbound_d = 0.4,
              desc = TRUE,
              plots = FALSE)
            
            tost = t_e$tost$asDF
            desc = t_e$desc$asDF
            
            power_df[i, 1] = max(tost$`p[1]`, tost$`p[2]`) # p value tost
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
        
        
        df = sample_function(ul_dist, input$n_input) %>% 
          as.data.frame() %>% 
          rename("tost" = V1,
                 "t_test" = V2,
                 "mean_actual" = V3,
                 "mean_sample" = V4,
                 "sd_actual" = V5,
                 "sd_sample" = V6,
                 "n_actual" = V7,
                 "n_sample" = V8,
                 "iteration" = V9) %>%
          mutate(sig_tost = case_when(
            tost < .05 ~ 1,
            tost > .05 ~ 0,
          )) %>%
          mutate(sig_ttest = case_when(
            t_test < .05 ~ 1,
            t_test > .05 ~ 0,
          )) %>%
          mutate(tost = round(as.numeric(tost), digits = 3)) %>%
          mutate(t_test = round(as.numeric(tost), digits = 3)) %>%
          mutate(mean_difference = round(as.numeric(mean_actual) -
                                           as.numeric(mean_sample), digits = 3)) %>%
          mutate(test_no = rep(1:10, each = 100))
        
        
         
          

        # draw the histogram with the specified number of bins
        df %>%
          group_by(test_no, n_actual) %>%
          summarize(n = sum(sig_tost)) %>%
          ggplot(aes(y = as.factor(n_actual), x = as.numeric(n)), 
                 fill = n_actual) +
          geom_boxplot(outlier.size = 0, fill = "orange") +
          ylab("tokens") + xlab("Statistical Power") +
          xlim(0,100) +
          geom_vline(xintercept = 80, linetype = "dashed", alpha = .4) +
          theme_minimal() +
          ggtitle("Box plot of the results of 10 power analyses") +
          theme(text = element_text(size = 20))
    })
  }




# Run the application 
shinyApp(ui = ui, server = server)
