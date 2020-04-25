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
library(shinyWidgets)

# read in data from RDS

obesity_region <- readRDS("data/obesity_region.RDS")


# ui code

ui <- navbarPage(
    
    title = "Obesity and Food Access in the US",
    
    theme = shinytheme("cosmo"),
    
    tabPanel("An Introduction",
        fluidPage(
            
            br(),
            br(),
            
            imageOutput("obesity_image", width = "100%", height = "100%"),
            
            br(),
            
            fluidRow(column(2), column(8,
                     h1(tags$b("Obesity and Food Insecurity in the United States"), align = "center", style = "color: darkred"),
                     p(tags$b("Insights on America's Alarming Obesity Rates and an Analysis on Food Insecurity"), align = "center"),
                     
                     br(),
                     
                     p("Although one of the wealthiest countries in the world, The United States has one of the highest obesity rates
                  of any developed country. In 2016, according to the Central Intelligence Agency, the US saw 36.20% of its 
                  adult population classified as obese. Granted, the US is an incredibly large and diverse country. People's diets and
                  amount of physical movement vary region to region, state to state. Thus, It is important to look at the
                  variation in the percentage of population that is obese across different state."),
                     
                     br(),
                     
                     h3("Obesity", style = "color: darkred"),
                     
                     p("Obesity is defined as excessive or abnormal fat accumulation that may impair health, according to the World
                  Health Organization. Body Mass Index is one way to determine obesity status; it is calculated by dividng a person's 
                  weight in kilograms by the square of their height in meters. A BMI equal to or greater than 30 is classified as obese."),
                     
                     br(),
                     
                     p("Because obesity can make its population more susceptible to severe illnesses, I wanted to explore different 
                       factors that could affect the percentage of obesity in a population. Some of these variables include, of
                       course, age, gender, and the region in which you live. However, I was also inspired by the unfortunate situation
                       that COVID19 has presented us with: food shortages. I wondered if having easy way access to healthy and reliable
                       food supplies had any correlation with obesity in the US. After all, if you cannot easily reach healthy food options, it 
                       is possible that you would resort to cheap fast food or unhealthy diets. I was also curious to see if food security was different
                       across different states and regions of the country, and how much it could be impacting obesity."),
                     
                     br(),
                     
                     h3("Food Insecurity", style = "color: darkred"),
                     
                     p("Currently, about 1 in 9 people in the US are suffering from food insecurity. According to Feeding America, food insecurity 
                        is a household's inability to provide enough food for every person to live an active, healthy life. It is a method of measuring 
                        hunger in America, but there are often serious health consequences that come with food insecurity, especially for children and for seniors, 
                        who are often stuck between choosing to feed themselves and paying for medical care. Food insecurity can be caused by economic reasons, 
                        such as a lack of money to buy groceries. However, there are infrastructural causes as well, like living in a food 
                        desert, where there is either a lack in supplies of or difficulty in reaching fresh and healthy foods. This version of
                        food insecurity will be one of the main variables in this analysis. Because poverty seems to be associated with food
                        insecurity, I looked at this variable as well."),
                     
                     br(),
                     
                     h3("Analysis", style = "color: darkred"),
                     
                     p("There are two main variables that I examined, as aforementioned: obesity and food access. Under the Variables 
                       in Question tab, you will find my analysis on both. The obesity tab explores obesity across the US over time. It also looks at 
                       how obesity differs across regions in the country. Within this tab, you can find a comparison between obesity and poverty as well.
                       The food access tab looks at food access across different states in the US."),
                     
                     br(),
                     br(),
                     br()
)))),
    
    navbarMenu("Variables in Question",
               tabPanel("Obesity",
                    tabsetPanel(
                        tabPanel("The United States",
                            
                        fluidRow(
                            
                                h3("Obesity Across the United States Over Time", style = "color: darkred; text-align: center"),
                                
                                 p("How does the percentage of the population classified as obese or overweight change over time?
                                    The graphic to the right suggests that the percentage increases over time, looking at years from
                                    2011-2018. Where does most of the change occur?"),

                                ),
                        
                        fluidRow(
                            
                             imageOutput("obesity_usmap")
                        )),
                        
               tabPanel("Regions",
                        
                        br(),
                        br(),
                        
                        sidebarLayout(
                            sidebarPanel(column(4),
                                         
                                h3("Obesity Across Individual Regions", style = "color: darkred"),          
                        
                                p("How does the percentage of the population classified as obese or overweight change over time?
                                The graphic to the right suggests that the percentage increases over time, looking at years from
                                2011-2018. Where does most of the change occur?"),
                                
                                br(),
                                br(),
                                
                                pickerInput("region", tags$b("Choose a region:"),
                                            choices = c("West", "South", "Northeast", "Midwest"),
                                            multiple = FALSE)
                               
                               ),
                            
                            mainPanel( 
                                imageOutput("obesity_region")
                                )
                        ),
               
                        br(),
                        br(),
                        
                        sidebarLayout(
                          sidebarPanel(column(4),
                                       
                                h3("Obesity Across Regions Comparatively", style = "color: darkred"),          
                                       
                                p("TEXT HERE")),
                          
                          mainPanel(
                              imageOutput("obesity_region_facet")
                          )
                        )
                ),
               
               tabPanel("Poverty",
                        
                br(),
                br(),
                
                sidebarLayout(
                    sidebarPanel(
                        
                        h3("Poverty in the US and Obesity", style = "color: purple")
                        
                    ),
                    
                    mainPanel(
                        plotOutput("poverty_obesity_plot")
                    )
                ),
                
                       sidebarPanel(
                           
                           h3("Poverty in the US", style = "color: purple")
                           
                       ),
                       
                       mainPanel(
                           imageOutput("poverty_plot")
                   )
               )
               )),
               
                tabPanel("Food Access"
                        )
),
    
    tabPanel("Findings",
             
             
             
             ),
    
    navbarMenu("About",
               tabPanel("The Data",
                        
                        p("Mention how the obesity data is just for 2011.")
               ),
               
               tabPanel("Me :)"
               )
    )
)
    

# Server code

server <- function(input, output) {
    
    # Load in image for top of app for Findings page
    
    output$obesity_image <- renderImage({
        list(src = './images/obesity.image.jpg', 
             height = 300,
             width = 600,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
    
    output$obesity_usmap <- renderImage({
        list(src = "./plots/obesity_mean_usmap.gif",
             contentType = 'image/gif',
             height = 600,
             width = 600,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
    
    output$obesity_region <- renderPlot({
        obesity_region <- obesity_region %>% 
        filter(!is.na(region)) %>% 
        filter(region == input$region)
        
        ggplot(data = obesity_region, aes(x = year, y = mean_perc)) +
            geom_jitter(aes(alpha = 0.1), show.legend = FALSE) +
            theme_minimal() +
            labs(title = "Percent of Population Classified as Obese or Overweight \n in US Over Time by Region",
                 caption = "Data from Center for Disease Control",
                 y = "Mean Percent of Population",
                 x = "Year") +
            geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "dark red") +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank())
    })
    
    output$obesity_region_facet <- renderImage({
        list(src = './plots/obesity_region_plot.png', 
             height = 700,
             width = 600,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
    
    output$poverty_obesity_plot <- renderPlot({
        ggplot(data = obesity_region, aes(x = year, y = mean_perc)) +
            geom_jitter()
    })
    
    output$poverty_plot <- renderImage({
        list(src = './plots/food_poverty_plot.png', 
             height = 700,
             width = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
