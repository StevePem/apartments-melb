# dashboard, as converted by ChatGPT

library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(sf)
library(leaflet)

# UI
ui <- fluidPage(
  titlePanel("Apartments and public transport in Melbourne 2004-2022"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("area", "Select an area type:",
                  choices = c("Greater Melbourne",
                              "Local Government Areas",
                              "Train corridors",
                              "Tram corridors",
                              "Bus corridors")),
      conditionalPanel(
        condition = "input.area == 'Local Government Areas'",
        selectInput("location.lga", "Select a Local Government Area", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.area == 'Train corridors'",
        selectInput("location.train", "Select a train corridor", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.area == 'Tram corridors'",
        selectInput("location.tram", "Select a tram corridor", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.area == 'Bus corridors'",
        selectInput("location.bus", "Select a bus corridor", choices = NULL)
      ),
      checkboxInput("apts", "Apartments", value = TRUE),
      checkboxInput("pop", "Population", value = TRUE),
      checkboxInput("serv", "Annual services", value = TRUE),
      checkboxInput("serv_capadj", "Annual services, capacity adjusted", value = TRUE),
      conditionalPanel(
        condition = "(input.area == 'Greater Melbourne' || input.area == 'Local Government Areas') && (input.serv || input.serv_capadj)",
        checkboxGroupInput("mode", "Select modes:",
                           choices = c("Train", "Tram", "Bus"),
                           selected = c("Train", "Tram"))
      ),
      conditionalPanel(
        condition = "input.area == 'Train corridors' && (input.serv || input.serv_capadj)",
        verbatimTextOutput("mode_output_train")
      ),
      conditionalPanel(
        condition = "input.area == 'Tram corridors' && (input.serv || input.serv_capadj)",
        verbatimTextOutput("mode_output_tram")
      ),
      conditionalPanel(
        condition = "input.area == 'Bus corridors' && (input.serv || input.serv_capadj)",
        verbatimTextOutput("mode_output_bus")
      ),
      actionButton("info", "Data information",
                   class = "btn btn-primary",
                   style = "border-radius: 5px; position: absolute; bottom: 8px;")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(
          width = 8,
          plotlyOutput("plot")
        ),
        column(
          width = 4,
          fluidRow(          verbatimTextOutput("apartment_growth"),
                             verbatimTextOutput("population_growth")
          ),
          fluidRow(
          verbatimTextOutput("public_transport_growth"),
          verbatimTextOutput("public_transport_capadj_growth")),
          fluidRow(
          leafletOutput("map"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # apartment, population and service numbers
  all.data <- readRDS("./data.rds")
  
  # details for input selectors
  LGA.names <- unique(all.data$location[all.data$type == "LGA"])
  corridors.train <- unique(all.data$location[all.data$type == "Train"])
  corridors.tram <- unique(all.data$location[all.data$type == "Tram"])
  corridors.bus <- unique(all.data$location[all.data$type == "Bus"])
  
  # data for map
  areas <- readRDS("./areas.rds")
  walkable.catchments <- readRDS("./walkable catchments.rds")
  apartments <- readRDS("./apartments.rds")
  
  observe({
    if (input$area == "Local Government Areas") {
      updateSelectInput(session, "location.lga", choices = LGA.names)
    } else if (input$area == "Train corridors") {
      updateSelectInput(session, "location.train", choices = corridors.train)
    } else if (input$area == "Tram corridors") {
      updateSelectInput(session, "location.tram", choices = corridors.tram)
    } else if (input$area == "Bus corridors") {
      updateSelectInput(session, "location.bus", choices = corridors.bus)
    }
  })
  
  observe({
    if (input$area == "Train corridors" && (input$serv || input$serv_capadj)) {
      output$mode_output_train <- renderText("Mode: Train")
    } else {
      output$mode_output_train <- NULL
    }
  })
  
  observe({
    if (input$area == "Tram corridors" && (input$serv || input$serv_capadj)) {
      output$mode_output_tram <- renderText("Mode: Tram")
    } else {
      output$mode_output_tram <- NULL
    }
  })
  
  observe({
    if (input$area == "Bus corridors" && (input$serv || input$serv_capadj)) {
      output$mode_output_bus <- renderText("Mode: Bus")
    } else {
      output$mode_output_bus <- NULL
    }
  })
  
  
  
  observeEvent(input$info, {
    showModal(modalDialog(
      title = "Data information",
      HTML("Data sources: <br>
    •	annual apartments: Victorian Department of Transport and Planning (DTP)<br>
    •	apartment baseline: Australian Bureau of Statistics 2006 census (apartment baseline) <br>
    •	train and tram services volumes: DTP <br> 
    •	tram capacities: Yarra Trams <br>
    •	bus service volumes: Public Transport Victoria GTFS data <br>
    •	tram capacities: Yarra Trams <br> 
    •	bus capacities: DTP and bus operators <br><br>
    'Apartments' are dwellings classified by DTP as 'attached 4 storey or more' (to 2016) or 'apartments' (from 2017), and other developments with 100 dwellings per hectare or more <br><br>
    'Annual services' are the annual number of public transport services operating within 800m walking distance of the relevant apartments <br><br>
    'Walkable catchments' shown on the map are:<br>
    •	for Greater Melbourne and Local Government Areas, places within within 800m walking distance of the public transport stops that are walkable from the apartments in those areas<br>
    •	for corridors, places within 800m walking distance of the stations or stops that make up the corridor, where those statons or stops have apartments within their catchments<br><br>
    'Population' is the population of the walkable catchment"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # data for plot
  data <- reactive({
    if (input$area == "Greater Melbourne") {
      filtered_data <- all.data %>%
        filter(location == "Greater Melbourne")
    } else if (input$area == "Local Government Areas") {
      filtered_data <- all.data %>%
        filter(location == input$location.lga)
    } else if (input$area == "Train corridors") {
      filtered_data <- all.data %>%
        filter(location == input$location.train)
    } else if (input$area == "Tram corridors") {
      filtered_data <- all.data %>%
        filter(location == input$location.tram)
    } else if (input$area == "Bus corridors") {
      filtered_data <- all.data %>%
        filter(location == input$location.bus)
    }
    
    # where bus is selected and service volumes shown, show from 2016 only
    if ("Bus" %in% input$mode & (input$serv | input$serv_capadj)) {
      filtered_data <- filtered_data %>%
        filter(Year >= 2016)
    }
    
    # Sum the selected services
    modes_selected <- input$mode
    
    filtered_data <- filtered_data %>%
      mutate(service = 0,
             service.capadj = 0)
    
    for (mode in modes_selected) {
      capadj_column <- paste0(mode, ".capadj")
      filtered_data <- filtered_data %>%
        mutate(service = service + !!sym(mode),
               service.capadj = service.capadj + !!sym(capadj_column))
    }
    
    # Extract the bases (usually 2004, or 2016 if bus; but may be first available
    # year for services that don't start until a different year)
    apt.base <- filtered_data %>% filter(Year == min(Year)) %>% pull(Apartments)
    pop.base <- filtered_data %>% filter(Year == min(Year)) %>% pull(Population)
    
    serv.base <- filtered_data %>% 
      filter(service > 0) %>%
      filter(Year == min(Year)) %>%
      pull(service)
    
    serv_capadj.base <- filtered_data %>% 
      filter(service.capadj > 0) %>%
      filter(Year == min(Year)) %>% 
      pull(service.capadj)
    
    return(list(filtered_data = filtered_data, 
                apt.base = apt.base, 
                pop.base = pop.base,
                serv.base = serv.base,
                serv_capadj.base = serv_capadj.base))
  })
  
  
  output$plot <- renderPlotly({
    data <- data()
    
    # ranges for y axis (excluding 0s in service or service.capadj)
    apt.values <- data$filtered_data$Apartments
    pop.values <- data$filtered_data$Population
    serv.values <- data$filtered_data$service
    serv_capadj.values <- data$filtered_data$service.capadj
    # lowest non-zero values if any, or else 100
    min.apt <- ifelse(max(apt.values) > 0,
                      min(apt.values[apt.values > 0]) / data$apt.base * 100, 
                      100)
    min.pop <- ifelse(max(pop.values) > 0,
                      min(pop.values[pop.values > 0]) / data$pop.base * 100, 
                      100)
    min.serv <- ifelse(max(serv.values) > 0,
                       min(serv.values[serv.values > 0]) / data$serv.base * 100, 
                       100)
    min.serv_capadj <- ifelse(max(serv_capadj.values) > 0,
                              min(serv_capadj.values[serv_capadj.values > 0]) / data$serv_capadj.base * 100, 
                              100)
    # min and max for y axis
    min_y <- min(min.apt, min.pop, min.serv, min.serv_capadj)
    max_y <- max(apt.values / data$apt.base * 100,
                 pop.values / data$pop.base * 100,
                 serv.values / data$serv.base * 100,
                 serv_capadj.values / data$serv_capadj.base * 100)
    
    # Create an empty plot
    p <- plot_ly()
    
    # Add a blank trace to ensure the legend is always present
    p <- p %>%
      add_trace(x = numeric(0), y = numeric(0), 
                type = "scatter", mode = "none", 
                name = "Legend Placeholder",
                showlegend = TRUE)
    
    
    if (input$apts) {
      p <- p %>%
        add_trace(data = data$filtered_data,
                  x = ~ Year, y = ~ Apartments / data$apt.base * 100, 
                  type = "bar",
                  marker = list(color = "#CCCCCC"),
                  name = "Apartments",  # Legend label
                  hovertemplate = "Apartments: %{customdata:,.0f}<extra></extra>",
                  customdata = ~ Apartments)
    }
    
    if (input$pop) {
      p <- p %>%
        add_trace(data = data$filtered_data,
                  x = ~ Year, y = ~ Population / data$pop.base * 100,
                  type = "scatter", mode = "lines+markers",
                  marker = list(color = "#33A02C", size = 8, symbol = "square"),
                  line = list(color = "#33A02C", width = 2),
                  name = "Population",  # Legend label
                  hovertemplate = "Population: %{customdata:,.0f}<extra></extra>",
                  customdata = ~ Population)
    }
    
    if (input$serv & length(input$mode > 0)) {
      p <- p %>%
        add_trace(data = data$filtered_data %>% filter(service > 0),
                  x = ~ Year, y = ~ service / data$serv.base * 100,
                  type = "scatter", mode = "lines+markers",
                  marker = list(color = "#FF7F00", size = 12),
                  line = list(color = "#FF7F00", width = 4),
                  name = "Annual services",  # Legend label
                  hovertemplate = "Annual services: %{customdata:,.0f}<extra></extra>",
                  customdata = ~ service)
    }
    
    if (input$serv_capadj & length(input$mode > 0)) {
      p <- p %>%
        add_trace(data = data$filtered_data %>% filter(service.capadj > 0),
                  x = ~ Year, y = ~ service.capadj / data$serv_capadj.base * 100,
                  type = "scatter", mode = "lines+markers",
                  marker = list(color = "#6A3D9A", size = 6),
                  line = list(color = "#6A3D9A", width = 2),
                  name = "Annual services, capacity adjusted",  # Legend label
                  hovertemplate = "Annual services: %{customdata:,.0f}<extra></extra>",
                  customdata = ~ service.capadj)
    }
    
    # build string for title
    if (input$apts & input$pop & (input$serv | input$serv_capadj)) {
      titlestring1 <- "Apartments, population and public transport"
    } else if (input$apts & input$pop) {
      titlestring1 <- "Apartments and population"
    } else if (input$apts & (input$serv | input$serv_capadj)) {
      titlestring1 <- "Apartments and public transport"
    } else if (input$pop & (input$serv | input$serv_capadj)) {
      titlestring1 <- "Population and public transport"
    } else if (input$apts) {
      titlestring1 <- "Apartments"
    } else if (input$pop) {
      titlestring1 <- "Population"
    } else if (input$serv | input$serv_capadj) {
      titlestring1 <- "Public transport"
    }
    
    if (input$area == "Greater Melbourne") {
      titlestring2 <- ""
      titlestring3 <- input$area
    } else if (input$area == "Local Government Areas") {
      titlestring2 <- ""
      titlestring3 <- input$location.lga
    } else if (input$area == "Train corridors") {
      titlestring2 <- "Train corridor:"
      titlestring3 <- input$location.train
    } else if (input$area == "Tram corridors") {
      titlestring2 <- "Tram corridor:"
      titlestring3 <- input$location.tram
    } else if (input$area == "Bus corridors") {
      titlestring2 <- "Bus corridor:"
      titlestring3 <- input$location.bus
    }
    
    titlestring <- paste(titlestring1, '<br>',
                         '<sup>', titlestring2, titlestring3, '</sup>')
    
    p <- p %>%
      layout(title = titlestring,
             
             xaxis = list(title = "Financial Year ended 30 June"),
             yaxis = list(title = "Values indexed to starting year = 100",
                          range = list(min_y - 5, max_y + 5)),
             
             # legend centered under X axis
             legend = list(orientation = "h",  # show entries horizontally
                           xanchor = "center",  # use centre of legend as anchor.
                           x = 0.5, y = -0.1)   # put legend in centre of x-axis
             
      ) 
    
    p
  })
  
  
  output$apartment_growth <- renderPrint({
    # Your apartment growth code here
  })
  
  output$population_growth <- renderPrint({
    # Your population growth code here
  })
  
  output$public_transport_growth <- renderPrint({
    # Your public transport growth code here
  })
  
  output$public_transport_capadj_growth <- renderPrint({
    # Your public transport capacity adjusted growth code here
  })
  
  display.polygons <- reactive({
    if (input$area == "Greater Melbourne") {
      area <- areas %>% filter(name == input$area)
      catchment <- walkable.catchments %>% filter(name == input$area)
    } else if (input$area == "Local Government Areas") {
      area <- areas %>% filter(name == input$location.lga)
      catchment <- walkable.catchments %>% filter(name == input$location.lga)
    } else if (input$area == "Train corridors") {
      area <- areas %>% filter(name == input$area)  # returns empty row
      catchment  <- walkable.catchments %>% filter(name == input$location.train)
    } else if (input$area == "Tram corridors") {
      area <- areas %>% filter(name == input$area)  # returns empty row
      catchment  <- walkable.catchments %>% filter(name == input$location.tram)
    } else if (input$area == "Bus corridors") {
      area <- areas %>% filter(name == input$area)  # returns empty row
      catchment <- walkable.catchments %>% filter(name == input$location.bus)
    }
    return(list(area = area, catchment = catchment))
  })
  
  output$map <- renderLeaflet({
    area <- display.polygons()$area
    catchment <- display.polygons()$catchment
    display.apartments <- st_intersection(apartments, display.polygons()$catchment)
    
    # 'area' section of legend (Greater Melbourne and LGAs only)
    if (input$area == "Greater Melbourne") {
      html_legend_area <- paste('
    <div class="legend">
      <div class="legend-item">
        <div class="legend-symbol-container">
          <div class="legend-symbol-square area-symbol"></div>
        </div>',
                                input$area,
                                '</div>
    ', sep = "")
    } else if (input$area == "Local Government Areas") {
      html_legend_area <- paste('
    <div class="legend">
      <div class="legend-item">
        <div class="legend-symbol-container">
          <div class="legend-symbol-square area-symbol"></div>
        </div>', 
                                input$location.lga, 
                                '</div>
    ', sep = "")
    } else {
      html_legend_area <- ''
    }
    
    # 'apartments' and 'catchment' sections of legend
    html_legend <- paste(
      html_legend_area, '
    <div class="legend">
      <div class="legend-item">
        <div class="legend-symbol-container">
          <div class="legend-symbol-point apartments-symbol"></div>
        </div>
        Apartments
      </div>
      <div class="legend-item">
        <div class="legend-symbol-container">
          <div class="legend-symbol-square catchment-symbol"></div>
        </div>
        Walkable catchments
      </div>
    </div>
  ', sep = "")
    
    leaflet() %>%
      addTiles(options = providerTileOptions(opacity = 0.7)) %>%
      addPolygons(data = area, color = "#e7298a", weight = 4, opacity = 1, 
                  fillColor = "transparent") %>%
      addPolygons(data = catchment, color = "black", weight = 2, opacity = 1,
                  fillColor = "transparent") %>%
      addCircleMarkers(data = display.apartments, color = "blue", radius = 1, opacity = 1) %>%
      addControl(html = html_legend, position = "topright")
  })
}

shinyApp(ui, server)
