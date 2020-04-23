#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)

# ui code

h3("Red Meat Production")

ui <- navbarPage(
    
    title = "TITLE",
    
    theme = shinytheme("simplex"),
    
    tabPanel(
        title = "About",
        fluidPage (
            fluidRow(
                h1("Welcome!")
            )
        )
    ),
    
    tabPanel(
        title = "Meat",
        fluidPage(
            fluidRow(
                h1("Welcome!")
            )
        )
    )
)
    
# Server code

server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)
