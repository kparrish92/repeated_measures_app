# Global variables can go here
n <- 200


# Define the UI
ui <- bootstrapPage(
  numericInput('n_u', 'Number of obs', n),
  numericInput('mean_u', 'mean', 0),
  numericInput('mean_u', 'sd', 0),
  plotOutput('plot')
)


# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    df = rnorm(n = n_u, mean = mean_u, sd = sd_u)
    plot(df)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)