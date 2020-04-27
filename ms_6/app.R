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
library(usmap)

# ui code

h3("Red Meat Production")

ui <- navbarPage(
    
    title = "Obesity and Food Access in the US",
    
    theme = shinytheme("simplex"),
    
    tabPanel(
        title = "Findings",
        fluidPage (
            
            imageOutput("obesity_image", width = "100%", height = "100%"),
            
            fluidRow(column(2)
                h1(tags$b("Obesity and Food Access in The United States"), align = "center"),
                p(tags$em("Insights on America's Alarming Obesity Rates \n and Analysis on Food Access"), align = "center"),
                
                br(),
                br(),
                
                p("Although one of the wealthiest countries in the world, The United States has one of the highest obesity rates
                  of any developed country. In 2016, according to the Central Intelligence Agency, the US saw 36.20% of its 
                  adult population as obese. Granted, the US is an incredibly large and diverse country. People's diets and
                  amount of physical movement vary region to region, state to state. It is important to look at the
                  variation in the percentage of population that is obese by state."),
                p("Obesity is defined as excessive or abnormal fat accumulation that may impair health, according to the World
                  Health Organization. Body Mass Index is one way to determine obesity status, and a BMI of equal to or 
                  greater than 30 is classified as obese. BMI is calculated by a person's weight in kilograms divided by the 
                  square of the heigh in meters.")
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
    
    # Load in image for top of app for Findings page
        
    output$obesity_image <- renderImage({
        
        list(src = 'images/obesity.image.jpg', 
             height = 444,
             width = 800, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
