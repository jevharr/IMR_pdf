
library(shiny)
library(tabulizer)
library(tidyverse)
library(rworldmap)
library(ggplot2)
library(fiftystater)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Drill into US data"),
  
  # Sidebar with a selector for country
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "country",
                  label = "Country",
                  world$Country,
                  multiple = F)
    ),
    
    # Show a plot of the IMR data
    mainPanel(
      plotOutput("map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  location <- 'https://fas.org/sgp/crs/misc/R41378.pdf'
  out <- extract_tables(location)
  
  worldData <- countryExData %>%
    select("ISO3V10", "Country")
  world <- data.frame(out[1]) %>%
    select(1:2) %>%
    setNames(c("Country", "IMR")) %>%
    as.tibble()
  world <- world[-1,]
  world$Country <- sub('Czech Republic', 'Czech Rep.', world$Country)
  world$Country <- sub('Slovak Republic', 'Slovakia', world$Country)
  world$Country <- sub('Korea', 'South Korea', world$Country)
  world$IMR <- as.numeric(world$IMR)
  world <- filter(world, Country != 'OECD Average') %>%
    left_join(worldData, by = "Country")
  sPDF_world <- joinCountryData2Map(world, joinCode = "NAME", verbose = T)
  
  stateCodes <- read.csv("stateCodes.csv")
  stateShape <- map_data("usa")
  us <- as.tibble(do.call(rbind, out[3:4])) %>%
    select(V1, V2) %>%
    setNames(c("State", "IMR"))
  us$State <- tolower(us$State)
  us[2, "State"] <- paste(us[1, "State"], us[2, "State"])
  us[54, "State"] <- paste(us[53, "State"], us[54, "State"])
  us <- us[-c(1, 26, 53),]
  us$IMR <- as.numeric(us$IMR)
  # data("fifty_states")
  
  output$map <- renderPlot({
    if(input$country != "United States"){
      par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
      mapCountryData(sPDF_world, nameColumnToPlot="IMR", addLegend = F, mapTitle = 'Infant Mortality per 1000 Live Births')
    }
    else{
      ggplot(us, aes(map_id = State)) + 
        # map points to the fifty_states shape data
        geom_map(aes(fill = IMR), map = fifty_states) + 
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        scale_fill_gradient(low = '#FFFF80', high = '#FF0000') +
        labs(x = "", y = "", title = "Infant Mortality per 1000 Live Births") +
        theme(legend.position = "bottom", 
              panel.background = element_blank())
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

