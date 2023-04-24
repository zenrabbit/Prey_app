# Prey_species_tool
# A simple tool to query reported prey species for federally listed vertebrates within Central California
library(shiny)
library(tidyverse)


prey <- read_csv("tidy_prey.csv") %>% 
  select(listed_species, prey_diet_binomial) %>% 
  group_by(listed_species, prey_diet_binomial) %>% 
  summarize(n = n())


ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Prey items for Central California listed vertebrates"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("listed", label = "listed species:", 
                  choices = prey$listed_species, selected = 1),
      hr(),
      tags$a(
        "Data from KNB",
        target = "_blank",
        href = "https://knb.ecoinformatics.org/view/doi%3A10.5063%2FF1NG4P39"
      ),
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("preyplot")  
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({
    req(input$listed)
    prey_sel <- prey %>% filter(listed_species %in% input$listed)
  })
  output$preyplot <- renderPlot({
    ggplot(data(), aes(reorder(prey_diet_binomial, n), n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(
        labels = scales::number_format(accuracy = 1)) +
      labs(x = "", y = "reported frequency of study") +
      theme_linedraw()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
