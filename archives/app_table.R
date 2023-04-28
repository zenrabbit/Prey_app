# Prey_species_tool
# A simple tool to query reported prey species for federally listed vertebrates within Central California
library(shiny)

library(tidyverse)
prey <- read_csv("tidy_prey.csv") %>% 
  select(listed_species, prey_diet_binomial) %>% 
  distinct()

shinyApp(
  ui = fluidPage(
    h3("Prey item query tool for Central California listed vertebrate species"),
    tags$a(
      "Data from KNB",
      target = "_blank",
      href = "https://knb.ecoinformatics.org/view/doi%3A10.5063%2FF1NG4P39"
    ),
    fluidRow(
      column(12,
             dataTableOutput('table')
      )
    )
  ),
  server = function(input, output) {
    output$table <- renderDataTable(prey)
    
    
  }
)
