
#
#import data
drugs <- read.csv("drugs.csv")
weather <- read.csv("weather.csv")
drimpair <- read.csv("drimpair.csv")
distract <- read.csv("distract.csv")
accident <- read.csv("accident.csv")

#install.packages("leaflet")
# libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)

# Define UI for application with multiple plots
# Join datasets
distract_drimpair <- inner_join(distract, drimpair, by = c("STATE", "STATENAME", "ST_CASE", "VEH_NO"), relationship = "many-to-many")

# Make factors
distract_drimpair$STATE <- as.factor(distract_drimpair$STATENAME)
weather$STATE <- as.factor(weather$STATENAME)
drugs$STATE <- as.factor(drugs$STATENAME)
distract_drimpair$DRDISTRACTNAME <- as.factor(distract_drimpair$DRDISTRACTNAME)
distract_drimpair$DRIMPAIRNAME <- as.factor(distract_drimpair$DRIMPAIRNAME)
weather$WEATHERNAME <- as.factor(weather$WEATHERNAME)
drugs$DRUGRESNAME <- as.factor(drugs$DRUGRESNAME)

# Start building app
traffic_data <- distract_drimpair

ui <- fluidPage(
  
  # app title
  titlePanel("Traffic Accident Risk Factors"),
  
  # create tabs
  navbarPage("Navigation",
# Map Tab
    tabPanel("Map",
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput("State", "Select State", unique(accident$STATENAME)),
        selectInput("County", "Select County", "All"),
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map")
      )
    )
  ),

# second tab
  tabPanel("Distractions",
           sidebarLayout(
             sidebarPanel(
               selectInput("selectStates", "Select State(s)", 
                           choices = unique(accident$STATENAME), 
                           multiple = TRUE, # choose multiple states
                           selected = unique(accident$STATENAME)[1])
               ),
           
    mainPanel(plotOutput("distractionsPlot")
        )
      )
    )
  )
)

  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Update county choices based on state
  observe({
    if (input$State != "All") {
      counties <- unique(accident$COUNTYNAME[accident$STATENAME == input$State])
      updateSelectInput(session, "County", choices = c("All", counties))
    } else {
      updateSelectInput(session, "County", choices = "All")
    }
  })
  
  # Filter data based on selections
  filtered_accident <- reactive({
    filtered <- accident
    
    if (input$State != "All") {
      filtered <- filter(filtered, STATENAME == input$State)
    }
    
    if (input$County != "All") {
      filtered <- filter(filtered, COUNTYNAME == input$County)
    }
    
    filtered
  })
  
  # Render map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = filtered_accident(),
        lat = ~ LATITUDE,
        lng = ~ LONGITUD,
        popup = ~ paste(
          "Accident ID:", 
          ST_CASE, 
          "<br>", "Description:", 
          HARM_EVNAME
        )
      )
  })
  # Render lollipop plot for selected states
  output$distractionsPlot <- renderPlot({
    # Filter data for selected states and distractions
    distract_data <- accident %>%
      filter(STATENAME %in% input$selectStates) %>%
      group_by(STATENAME) %>%
      summarize(DistractionCount = n_distinct(ST_CASE)) # Count rows with distraction data
    
    # Create lollipop plot
    ggplot(distract_data, aes(x = STATENAME, y = DistractionCount)) +
      geom_segment(aes(x = STATENAME, xend = STATENAME, y = 0, yend = DistractionCount), color = "skyblue") +
      geom_point(color = "blue", size = 4) +
      labs(title = "Number of Distraction-Related Accidents",
           x = "State", y = "Number of Accidents") +
      theme_minimal() +
      coord_flip() # flip coordinates for horizontal lollipop plot
  })
}

#     traffic_data %>%
#       mutate(
#         DRDISTRACTNAME = case_when(
#           grepl("Phone|Mobile|Texting|Cellular|Device", DRDISTRACTNAME, ignore.case = TRUE) ~ "Phone Related",
#           grepl("Careless|Inattentive|Inattention|Details Unknown|Other Distraction|Lost in Thought", DRDISTRACTNAME, ignore.case = TRUE) ~ "Inattentive",
#           grepl("Person|Object|Occupant", DRDISTRACTNAME, ignore.case = TRUE) ~ "Objects/People",
#           grepl("Vehicle|Audio", DRDISTRACTNAME, ignore.case = TRUE) ~ "Controls",
#           TRUE ~ DRDISTRACTNAME
#         )
#       ) %>%
#       filter(DRDISTRACTNAME != "Not Reported") %>%
#       filter(STATE == input$state) %>%
#       group_by(DRDISTRACTNAME) %>%
#       summarize(accidents = n_distinct(ST_CASE), .groups = 'drop') %>%
#       arrange(desc(accidents))



# Run the application 
shinyApp(ui = ui, server = server)

