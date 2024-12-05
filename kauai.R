library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Kauai Island Flood Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Kauai Overview", tabName = "kauai_overview", icon = icon("map")),
      menuItem("Flood Information", tabName = "flood_info", icon = icon("cloud")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Model Fit", tabName = "model_fit", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Welcome Page
      tabItem(tabName = "welcome",
              h1("Welcome to the Kauai Island Flood Dashboard"),
              p("This dashboard provides insights into the flooding situation on Kauai Island, including maps and models.")
      ),
      
      # Kauai Overview Page
      tabItem(tabName = "kauai_overview",
              h2("About Kauai Island"),
              p("Kauai, known as the ‘Garden Isle,’ is the oldest and fourth-largest island in the Hawaiian archipelago, located in the northwest part of the chain. The island is renowned for its lush landscapes, featuring dense tropical rainforests, dramatic cliffs, waterfalls, and pristine beaches. Kauai is home to several iconic natural attractions, such as the Na Pali Coast, famous for its steep cliffs and secluded beaches, and the Waimea Canyon, often referred to as the ‘Grand Canyon of the Pacific.’ The island's climate is tropical and wet, with the northern regions receiving the highest rainfall in the U.S."),
              p("Kauai has a rich history, with the first Polynesian settlers arriving around 1500 BC. The island was the first in the Hawaiian Islands to be settled by humans and retains a deep connection to its Polynesian roots. In the modern era, Kauai's economy relies heavily on tourism, agriculture, and retail. The island attracts visitors with its natural beauty, offering activities like hiking, surfing, snorkeling, and exploring its numerous beaches and gardens. However, Kauai is also prone to flooding due to its high rainfall and mountainous terrain, making flood management and emergency planning a critical part of its infrastructure."),
              p("With its combination of stunning landscapes, rich culture, and abundant outdoor activities, Kauai is a top destination for travelers seeking both adventure and relaxation."),
              leafletOutput("kauai_map")
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
              h2("Flood Data")
      ),
      
      # Model Fit Page
      tabItem(tabName = "model_fit",
              h2("Flood Model Fit"),
              p("regression<-lm(flood~rainfall+slope+area+soil+heavy rain)")
              
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
}


shinyApp(ui = ui, server = server)

