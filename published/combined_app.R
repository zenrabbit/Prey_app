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
  group_by(listed_species, prey_diet_binomial_updates, class_prey) %>% 
  summarize(n = n())

# data for prey table
prey_table <- prey %>% 
  distinct() %>%
  select(listed_species,  class_listed, order_listed, family_listed, 
         "prey_binomial" = prey_diet_binomial_updates,class_prey, order_prey,
          family_prey)

# Plot colors 
cols <- RColorBrewer::brewer.pal(5, "Pastel1")
colorRampPalette(cols)(23)

# pastel option
# class_cols <- c("#FBB4AE", "#EDB8B7", "#E0BDC1", "#D3C1CA", "#C6C6D4", "#B9CADE", 
#   "#B5CFE0", "#B9D5DA", "#BEDAD5", "#C2E0CF", "#C7E5CA", "#CCEBC5",   "#CFE5CA", 
#   "#D2DFD0", "#D5D9D5", "#D9D3DB", "#DCCDE1", "#E0CCDE","#E6CED3",  "#ECD1C7", 
#   "#F2D3BC", "#F8D6B1", "#FED9A6")

# viridis 
class_cols <- viridis::viridis(23)
names(class_cols) <- unique(prey$class_prey)

# User Interface ---------------------------------------------------------------

ui <- navbarPage(
  title = "Prey item query tool for Central California listed vertebrate species",
  theme = shinytheme("flatly"), 
  
  tags$head(tags$style(HTML('.navbar-static-top {background-color: #1E9B8AFF;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: #1E9B8AFF;}'))),
  
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
  
  # the height function allows for dynamic rescaling of plot based on the 
  # number of rows in the dataset - it modifies the number of pixels allocated 
  # to the height of the plot
  output$preyplot <- renderPlot({
    ggplot(data(), aes(y = reorder(prey_diet_binomial_updates, n), x = n)) +
      geom_bar(stat = "identity", aes(fill = class_prey), color = "black", linewidth = 0.25) +
      scale_x_continuous(
        labels = scales::number_format(accuracy = 1), 
        breaks = function(x) unique(floor(pretty(x))),
        expand = c(0,0),
        limits = function(x) c(0, max(x) + 0.1)
      ) +
      scale_fill_manual("Prey Class", values = class_cols) +
      labs(y = "", x = "reported frequency of study") +
      theme_linedraw() +
      theme(axis.text.y = element_text(face = "italic"),
            legend.justification = "top")
    }, height = function() {200 + (nrow(data())*5.5)})
  
  # prey table 
  output$table <- DT::renderDataTable({
    prey_table}, 
    colnames = c("Listed Species", "Class", "Order", "Family", 
                 "Prey Species", "Class", "Order", "Family"),
   # container = table_container, 
   rownames = FALSE)
    
}

# Run app ----------------------------------------------------------------------
shinyApp(ui = ui, server = server)

