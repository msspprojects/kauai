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
  dashboardHeader(title = "Kauai Fire Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Overview of Kauai", tabName = "kauai_overview", icon = icon("map")),
      menuItem("Fire Information", tabName = "fire_info", icon = icon("info-circle")),
      menuItem("Modeling and Analysis", tabName = "fire_model", icon = icon("lightbulb"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          font-size: 18px; /* Adjust the overall font size */
        }
        h1 {
          font-size: 28px; /* Larger size for h1 headers */
        }
        p {
          font-size: 18px; /* Adjust paragraph text size */
        }
      "))
    ),
    tabItems(
      
      # Welcome Page
      tabItem(tabName = "welcome",
              h1("Welcome to the Kauai Island Fire Events Dashboard", 
                 style = "font-size: 30px;"),
              fluidRow(
                column(6, img(src = "kauaimage.jpg", width = "100%")),  
                column(6, img(src = "jurassicfall2.jpg"))
              ),
              img(src = "jurassic3.webp", width = "50%", 
                  style = "margin-top: -170px;")
      ),
      
      # Kauai Overview Page
      tabItem(tabName = "kauai_overview",
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
      numerous beaches and gardens.", style = "font-size: 18px;"),
               p("With its combination of stunning landscapes, rich culture, 
      and abundant outdoor activities, Kauai is a top destination for 
      travelers seeking both adventure and relaxation.", 
                 style = "font-size: 18px;")),
      
      tabItem(tabName = "fire_info",
              tabsetPanel(
                tabPanel("Fire Causes and Risks",
                         h2("Main Causes and Risks of Fire on Kauai Island"),
                         p("Kauaʻi, located in the northernmost part of the Hawaiian Islands, is home to Mount Waiʻaleʻale in its center, one of the wettest places on Earth. 
                           
                          However, drier coastal areas, where wildfires typically occur, are more vulnerable due to the presence of dry brush and vegetation.
                           
                           This vegetation is a result of massive wet seasons on Kaua'i which create lots of fuel for these fires to burn.", style = "font-size: 18px;" 
                           
                           
                         ), 
                         p("Higher concentrations of fire ignitions are evident in the southern and eastern coastal areas. These regions are more prone to wildfires due to a combination of factors such as dry vegetation, human activities, and proximity to urban areas. In contrast, inland areas and regions in the northern parts of the island exhibit fewer fire ignitions, possibly due to lower population density and different environmental conditions.
", style = "font-size: 18px;")),
                tabPanel("Fire Events Location on Map",
                         leafletOutput("fire_map", height = "500px")),
                tabPanel("Fire Event EDA",
                         uiOutput("eda_plots")
                )
              )
            ),
      tabItem(tabName = "fire_model",
              tabsetPanel(
                tabPanel("Correlation Matrix",
                         img(src = "correlationplot.png", width = "100%")),
                tabPanel("Model and Results",
                         withMathJax(),
                         h2("Null Model"),
                         p("$$lm(acres \\sim 1)$$"),
                         h2("Model 1"),
                         p("$$lm(acres \\sim .)$$"),
                         img(src = "model1output.png", width = "80%"),
                         h2("Model 2"),
                         p("$$lm(acres \\sim \\beta_0 + \\beta_1 \\cdot air \\ temperature+\\beta_2 \\cdot wind \\ speed +\\beta_3 \\cdot wind \\ direction +\\beta_4 \\cdot dew \\ point \\ temperature)$$"),
                         img(src = "model2output.png", width = "80%"),
                         h2("Model 3"),
                         p("$$lm(acres \\sim \\beta_0 + \\beta_1 \\cdot air \\ temperature+\\beta_2 \\cdot wind \\ speed +\\beta_3 \\cdot wind \\ direction)$$"),
                         img(src = "model3output.png", width = "80%")
                         ),
                tabPanel("Model Comparison Using MSE",
                         h2("Model Comparison Table"),
                         tableOutput("mse_table")),
                tabPanel("Validation of Important Predictors Using XGBoost",
                         img(src = "xgboostoutput.png", width = "100%"),
                         h2("General Introduction to XGBoost"),
                         p("XGBoost is a powerful machine learning method that builds a series of decision trees to make predictions. Each new tree in the sequence tries to improve upon the errors of the previous trees, creating a strong overall model. It uses a technique called “gradient boosting,” where each tree is built using information about how the previous trees performed, gradually reducing errors."),
                         h2("What is this importance score?"),
                         p("It is a metric called 'Gain', 'Gain' measures the average improvement in accuracy or reduction in error brought by each feature’s splits. A higher gain means that when the model used that feature for splitting, it led to a more significant improvement in the model’s predictions. Features with high gain values are considered more important because they are more effective at reducing prediction errors."))
              )
          )
      )
    )
)


#Server
server <- function(input, output, session) {
  
  # Example data for the table
  models <- data.frame(
    "Model Name" = c("Null Model", "Model 1", "Model 2", "Model 3"),
    "MSE" = c(435780.8, 378426.4, 392416, 392430.1)
  )
  
  # Render the table
  output$mse_table <- renderTable({
    models
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
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
  fire_data <- read_csv("data/fires_range(in).csv")
  fire_data <- head(fire_data, -1)
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

