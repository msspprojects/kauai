library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(bslib)
library(readr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Kauai Fire and Flood"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Kauai Overview", tabName = "kauai_overview", icon = icon("map")),
      menuItem("Flood Information", tabName = "flood_info", icon = icon("cloud")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Fire Information", tabName = "fire_info", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          font-size: 16px; /* Adjust the overall font size */
        }
        h1 {
          font-size: 28px; /* Larger size for h1 headers */
        }
        p {
          font-size: 14px; /* Adjust paragraph text size */
        }
      "))
    ),
    tabItems(
      
      # Welcome Page
      tabItem(tabName = "welcome",
              h1("Welcome to the Kauai Island Flood Dashboard", 
                 style = "font-size: 30px;"),
              p("This dashboard provides insights into the flooding situation 
                on Kauai Island, including maps and models.", 
                style = "font-size: 18px;")
      ),
      
      # Kauai Overview Page
      tabItem(tabName = "kauai_overview",
              tabsetPanel(
                tabPanel("General Introduction to Kauai",
                         h2("About Kauai Island"),
                         p("Kauai, known as the ‘Garden Isle,’ is the oldest and 
                fourth-largest island in the Hawaiian archipelago, 
                located in the northwest part of the chain. 
                The island is renowned for its lush landscapes, 
                featuring dense tropical rainforests, dramatic cliffs, 
                waterfalls, and pristine beaches. 
                Kauai is home to several iconic natural attractions, 
                such as the Na Pali Coast, famous for its steep cliffs and 
                secluded beaches, and the Waimea Canyon, often referred to as 
                the ‘Grand Canyon of the Pacific.’ The island's climate is 
                tropical and wet, with the northern regions receiving the 
                highest rainfall in the U.S.", style = "font-size: 18px;"),
                         p("Kauai has a rich history, with the first Polynesian settlers 
                arriving around 1500 BC. The island was the first in the 
                Hawaiian Islands to be settled by humans and retains a deep 
                connection to its Polynesian roots. In the modern era, Kauai's 
                economy relies heavily on tourism, agriculture, and retail. 
                The island attracts visitors with its natural beauty, offering 
                activities like hiking, surfing, snorkeling, and exploring its 
                numerous beaches and gardens. However, Kauai is also prone to 
                flooding due to its high rainfall and mountainous terrain, 
                making flood management and emergency planning a critical 
                part of its infrastructure.", style = "font-size: 18px;"),
                         p("With its combination of stunning landscapes, rich culture, 
                and abundant outdoor activities, Kauai is a top destination for 
                travelers seeking both adventure and relaxation.", 
                           style = "font-size: 18px;")),
                 tabPanel("Kauai Map",
                          h2("Map of Kauai Island"),
                          leafletOutput("kauai_map", height = "500px"))
                )
      ),
      
      # Flood Information Page
      tabItem(tabName = "flood_info",
              tabsetPanel(
                tabPanel("Flood Causes and Risks",
                         h2("Flood Causes and Risks on Kauai Island"),
                         p("The primary causes of flooding on Kauai are heavy rainfall and storms, often brought by tropical systems like hurricanes and tropical storms. These weather events can lead to substantial rainfall in short periods, overwhelming the island's drainage systems and causing flash floods. The steep terrain exacerbates this, as water flows quickly down mountains and into the valleys, leading to swift and destructive flooding.

Kauai's rivers, such as the Wailua and Hanalei Rivers, are prone to overflowing during heavy rains, further contributing to flood risks in low-lying areas. In addition to flash floods, Kauai also faces the risk of coastal flooding, particularly during high tides or storm surges associated with tropical storms.")
                ),
                tabPanel("Historical Flood Events",
                         h2("Historical Flood Events on Kauai Island"),
                         p("Kauai has experienced several severe flooding events in its history. One of the most significant floods occurred in March 1992, when a tropical storm dumped more than 25 inches of rain in a 24-hour period, causing widespread damage across the island. The flooding affected infrastructure, homes, and agriculture, and resulted in the tragic loss of life.

Another notable event was the flooding caused by Hurricane Iniki in 1992, which brought intense rainfall and winds, devastating parts of the island, especially the western and northern regions. These events highlighted the vulnerability of Kauai's infrastructure to flooding and the need for effective flood control measures.")
                ),
                tabPanel("Flood Mitigation and Preparedness",
                         h2("Flood Mitigation and Preparedness on Kauai Island"),
                         p("In response to the recurring flooding risks, Kauai has implemented various flood control measures, such as improving drainage systems, constructing levees, and reinforcing riverbanks. Additionally, the island has developed emergency response protocols to ensure residents and visitors are prepared during flood events.

Kauai also conducts regular flood risk assessments and maintains flood maps to identify the most vulnerable areas. This helps guide development and ensures that new construction is built with flood risks in mind. Public education campaigns aim to raise awareness about the dangers of flooding and promote preparedness, especially in flood-prone regions.

Despite these efforts, the island remains at risk, and continued vigilance and adaptation to climate change will be crucial in managing future flood events on Kauai.")
                )
              )
      ),
      
      # Data Page
      tabItem(tabName = "data",
              h2("Flood Data")),
      tabItem(tabName = "fire_info",
              tabsetPanel(
                tabPanel("Fire Causes and Risks",
                         h2("Main Causes and Risks of Fire on Kauai Island")),
                tabPanel("Fire Events Location on Map",
                         leafletOutput("fire_map", height = "500px")),
                tabPanel("Fire Event EDA",
                         uiOutput("eda_plots")
                )
              )
            )
    )
  )
)

#Server
server <- function(input, output, session) {
  
  #Map
  output$kauai_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -159.5261, lat = 22.0964, zoom = 10) %>%
      addMarkers(lng = -159.5261, lat = 22.0964, popup = "Kauai Island")
  })
  eda_plot_paths <- list(
    c("Humidity1.png", "Temperature1.png", "WindSpeed1.png"),
    c("Humidity2.png", "Temperature2.png", "WindSpeed2.png"),
    c("Humidity3.png", "Temperature3.png", "WindSpeed3.png"),
    c("Humidity4.png", "Temperature4.png", "WindSpeed4.png"),
    c("Humidity5.png", "Temperature5.png", "WindSpeed5.png"),
    c("Humidity6.png", "Temperature6.png", "WindSpeed6.png"),
    c("Humidity7.png", "Temperature7.png", "WindSpeed7.png")
  )
  fire_data <- read_csv("../data/fires_range(in).csv")
  fire_data <- fire_data %>% slice(-n())
  fire_data <- fire_data %>%
    mutate(eda_plots = eda_plot_paths[row_number()])
  
  # Render the fire map
  output$fire_map <- renderLeaflet({
    leaflet(fire_data) %>%
      addTiles() %>%
      # Add markers proportional to the acres burned
      addCircleMarkers(
        lng = ~longitude,
        lat = ~lattitude,
        radius = ~sqrt(acres) / 2, # Scale radius based on acres
        color = "red",
        fillOpacity = 0.5,
        popup = ~paste0(
          "<strong>Start Date:</strong> ", start_date, 
          "<br><strong>End Date:</strong> ", end_date, 
          "<br><strong>Acres Burned:</strong> ", acres
        ),
        layerId = ~ID
      )
  })
  
  selected_event <- reactiveVal(NULL)
  # Observe map marker clicks
  observeEvent(input$fire_map_marker_click, {
    clicked_id <- input$fire_map_marker_click$id
    
    # Filter the corresponding row for the clicked event
    event_data <- fire_data %>% filter(ID == clicked_id)
    
    # Update the reactive value with selected event data
    selected_event(event_data)
  })
    
  # Dynamically render the plots
  output$eda_plots <- renderUI({
    req(selected_event())
    event_data <- selected_event()
    plot_paths <- event_data$eda_plots[[1]]
    if (!is.null(plot_paths)) {
      tagList(
        lapply(plot_paths, function(path) {
          img(src = path, width = "80%")
        })
      )
    } else {
      "No plots available for this fire event."
    }
  })
}


shinyApp(ui=ui, server = server)

