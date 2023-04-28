# Prey_species_tool
# A simple tool to query reported prey species for federally listed vertebrates 
# within Central California

# Set Up -----------------------------------------------------------------------

## load data and packages ------------------------------------------------------
# load packages
library(shiny)        # for running app
library(shinyWidgets) # extra widgets
library(tidyverse)    # data tidying
library(rgbif)        # retrieve taxonomy
library(shinythemes)  # access theme for app

# read in data
prey <- read_csv("tidy_prey_full_names.csv") 

## Prep data -------------------------------------------------------------------

# data for prey plot
prey_plot <- prey %>% 
  group_by(listed_species, prey_diet_binomial_updates) %>% 
  summarize(n = n())

# data for prey table
prey_table <- prey %>% 
  distinct() %>%
  select(listed_species, order_listed, class_listed, family_listed, 
         "prey_binomial" = prey_diet_binomial_updates, order_prey,
         class_prey, family_prey)

# special container for data table 
table_container <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 4, 'Listed Species'),
      th(colspan = 4, 'Prey Species')
    ),
    tr(
      lapply(rep(c('Species', 'Order', 'Class', 'Family'), 2), th)
    )
  )
))

# User Interface ---------------------------------------------------------------

ui <- navbarPage(
  title = "Prey item query tool for Central California listed vertebrate species",
  theme = shinytheme("flatly"), 
  
  # tab for plot
  tabPanel(
    title = "Plot",
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
        column(12,(div(style='height:auto;',
                      plotOutput("preyplot"))) )
      )
    )
  ),
  
  # tab for table
  tabPanel(
    title = "Table",
    tags$a(
      "Data from KNB",
      target = "_blank",
      href = "https://knb.ecoinformatics.org/view/doi%3A10.5063%2FF1NG4P39"
      ),
    fluidRow(
      column(12,
             DT::dataTableOutput('table')
             )
      )
    )
)


# Server -----------------------------------------------------------------------

server <- function(input, output) {
  
  # prey plot 
  data <- reactive({
    req(input$listed)
    prey_sel <- prey_plot %>% filter(listed_species %in% input$listed)
  })
  
  output$preyplot <- renderPlot({
    ggplot(data(), aes(reorder(prey_diet_binomial_updates, n), n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(
        labels = scales::number_format(accuracy = 1)) +
      labs(x = "", y = "reported frequency of study") +
      theme_linedraw() +
      theme(axis.text.y = element_text(face = "italic"))
    }, height = function() {200 + (nrow(data())*5.5)})
  
  # prey table 
  output$table <- DT::renderDataTable({
    prey_table}, container = table_container, rownames = FALSE)
    
}

# Run app ----------------------------------------------------------------------
shinyApp(ui = ui, server = server)

