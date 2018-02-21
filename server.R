# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderTable({
    ggplot(data = movies, aes_string(x = x, y = y,
                                     color = z)) +
      geom_point()
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)