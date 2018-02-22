library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

cdata <- read.csv("C:/ttu/spring18/kaggle/uniRanking/data/cwurData.csv")
time <- read.csv("C:/ttu/spring18/kaggle/uniRanking/data/timesData.csv")
# Define UI for application that plots features of movies
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                titlePanel("World University Ranking", windowTitle = "University Ranking"),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    h3("Plotting"),      # Third level header: Plotting
                    
                    # Select variable for year
                    selectInput(inputId = "year", 
                                label = "Year: ",
                                choices = c("2015", "2014", "2013", "2012"), 
                                selected = "2015"),
                    
                    hr(),
                    
                    
                    # Select variable for world rank
                    sliderInput(inputId = "worldRank", 
                                label = "World Rank: ",
                                min = 1, max = 40, 
                                value = "20"),
              
                    
                    # Set alpha level
                    #sliderInput(inputId = "alpha", 
                    #            label = "Alpha:", 
                     #           min = 0, max = 1, 
                      #          value = 0.5),
                  
                    
                    # Enter text for plot 1 title
                    textInput(inputId = "plot_title1", 
                              label = "Plot-1 title: ", 
                              placeholder = "Enter text to be used as plot-1 title"),
                    
                    # Enter text for plot 2 title
                    textInput(inputId = "plot_title2", 
                              label = "Plot-2 title: ", 
                              placeholder = "Enter text to be used as plot-2 title"),
                    
                    hr(),
                    
                    # Show data table
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table",
                                  value = TRUE),
                    
                    br(),
                    
                    # Built with Shiny by RStudio
                    br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                       ".")
                    
                  ),
                  
                  # Output:
                  mainPanel(
                    
                    tabsetPanel(id = "tabspanel", type = "tabs",
                                tabPanel(title = "Plot", 
                                         plotOutput(outputId = "barplot"),
                                         br(),
                                         plotOutput(outputId = "linechart")),
                                tabPanel(title = "Data", 
                                         br(),
                                         DT::dataTableOutput(outputId = "moviestable"))
                    )
                  )
                )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create barplot object the plotOutput function is expecting 
 output$barplot <- renderPlot({
    
    options(repr.plot.width=5, repr.plot.height=5)
    theme_b<-theme(axis.text.x = element_blank(),legend.position = "none")
    
    cdata %>% select(year, institution, world_rank) %>% 
      group_by(institution, world_rank) %>% 
      filter(world_rank <= as.numeric(input$worldRank) & year == input$year) %>%
      ggplot(aes(x = institution, y = world_rank)) + geom_bar(stat = 'identity', fill = 'orchid4') + 
      #geom_point(alpha = input$alpha) +
      labs(title = toTitleCase(input$plot_title1)) + 
      theme_b + geom_label(aes(label = institution), size = 3)
  })
 
 output$linechart <- renderPlot({
   
   options(repr.plot.width=8, repr.plot.height=5)
   theme_a <- theme(axis.text.x = element_text(angle=90))
   
   time$world_rank <- as.numeric(time$world_rank)
   time$num_students <- as.numeric(time$num_students)
   #time$year <- as.numeric(time$year)
   
   time %>% select(world_rank, university_name, num_students) %>% 
     group_by(university_name) %>% 
     filter(world_rank <= as.numeric(input$worldRank)) %>% 
     arrange(desc(num_students)) %>% 
     ggplot(aes(x = factor(university_name, levels = university_name), 
                y = num_students, fill = world_rank)) + 
     #geom_density(alpha = input$alpha) + 
     labs(title = toTitleCase(input$plot_title2)) + 
     geom_bar(stat = 'identity') + theme_a + scale_fill_continuous(low="hotpink3", high="black")
 })
  
  
  # Update code below to render data table regardless of current state of input$show_data
  output$moviestable <- DT::renderDataTable({
    DT::datatable(data = cdata[, c(1:4,6:8, 13:14)], 
                  options = list(pageLength = 15), 
                  rownames = FALSE)
  })
  
  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabspanel", target = "Data", select = TRUE)
    } else {
      hideTab(inputId = "tabspanel", target = "Data")
    }
  })
}

# Create Shiny app object
shinyApp(ui = ui, server = server)