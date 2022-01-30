#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if (!require(openintro)) install.packages('openintro')
if (!require("rnaturalearth")) install.packages('rnaturalearth')
if (!require("rnaturalearthdata")) install.packages('rnaturalearthdata')
if (!require(plotly)) install.packages('plotly')
if (!require(shiny)) install.packages('shiny')
if (!require(tidyverse)) install.packages('tidyverse')
library(shiny)
library(tidyverse)
library(openintro)
library("rnaturalearth")
library("rnaturalearthdata")
library(plotly)

coviddata <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}

choosevar <- function(varname, choosedate, data = coviddata){
    varname1 <- enquo(varname)
    data %>%
        filter(date == choosedate) %>%
        select(iso_code, location, count = !!varname1)
}

mapplot <- function(data){
    worldcoviddata <- world %>%
        left_join(data, by=c("iso_a3" = "iso_code"))
    p <- ggplot(worldcoviddata) +
        geom_sf(aes(text = location, fill = count), color = "black")+
        scale_fill_continuous(low = "#efedf5", high = "#3f007d") +
        my_map_theme()
    ggplotly(p)
}
    
# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 World Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("choosedate",
                        "Date:",
                        min = as.Date("2020-01-01","%Y-%m-%d"),
                        max = as.Date(Sys.Date()-1,"%Y-%m-%d"),
                        value=as.Date("2021-11-18"),
                        timeFormat="%Y-%m-%d"),
        
            selectInput("varname",
                        "Variable to Map:",
                        choices = list("Total Cases Per Million" = "total_cases_per_million",
                                       "Total Deaths Per Million" = "total_deaths_per_million",
                                       "New Cases Per Million" = "new_cases_per_million", 
                                       "New Deaths Per Million" = "new_deaths_per_million"), 
                        selected = "total_cases_per_Million"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("TitleText")),
            h5(textOutput("SubtitleText")),
            plotlyOutput("mapPlot"),
            h5("Data source:", 
               tags$a(href="https://github.com/owid/covid-19-data/tree/master/public/data", "Our World in Data"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$TitleText <- renderText(paste("Covid-19 World Data"))
    
    output$SubtitleText <- renderText(paste("Map shows", input$varname, 
                                            "on", input$choosedate))
    
    output$mapPlot <- renderPlotly({
       mapplot(choosevar(varname = input$varname, choosedate = input$choosedate, 
                                 data = coviddata))
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
