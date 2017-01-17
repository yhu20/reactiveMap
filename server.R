library(shinydashboard)
library(leaflet)
library(ggplot2)
library(iterators)
library(rMaps)


# Get data from file
get_data <- function(){
  dt1 <- read.csv(file.path(getwd(), 'data/csc_pi_dummy_data.csv'), nrows = 500)
  dt2 <- read.csv(file.path(getwd(), 'data/csc_pi_dummy_data.csv'), skip = 4000, 
                  nrows = 500, header=FALSE)
  dt3 <- read.csv(file.path(getwd(), 'data/csc_pi_dummy_data.csv'), 
                  skip = 8000, header = FALSE,
                  nrows = 500)
  names(dt2) <- names(dt1)
  names(dt3) <- names(dt1)
  return(rbind(dt1, dt2, dt3))
  }


function(input, output, session){
  data <- na.omit(get_data())
  data$AT_SERIAL <- as.character(data$AT_SERIAL)
  data$QUEUE_ZONE_DURATION <- as.numeric(data$QUEUE_ZONE_DURATION)
  data$DATE <- as.Date(data$DATE)
  data$D_L_AT_FUEL <- as.numeric(as.character(data$D_L_AT_FUEL))
  data$Capacity <- as.numeric(as.character(data$Capacity))
  data$CYCLE_TIME <- as.numeric(as.character(data$CYCLE_TIME))
  data$DZ_LZ_TRAVEL_START <- as.POSIXct(as.character(data$DZ_LZ_TRAVEL_START), format="%Y-%m-%d %H:%M:%S")

  #
  # Machine map dashboard
  #
  
  # Set auto refresh data
  autoIvalidate <- reactiveTimer(5000, session)
  
  machineData <- reactive({
    # data[data$AT_SERIAL == input$serial_num,]
    if(is.na(input$dateSelect1)){na.omit(data[data$AT_SERIAL == input$serial_num,])}
    else{na.omit(data[data$AT_SERIAL == input$serial_num & data$DATE == input$dateSelect1,])}
  })
  
  imachineData <- reactive({
    iter(machineData(), by="row")
  })
  
  # Number of seconds since last update
  output$timeSinceLastUpdate <- renderUI({
    # Trigger this every 1 seconds
    invalidateLater(1000, session)
    p(
      class = "text-muted",
      "Data refreshed ",
      round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
      " seconds ago."
    )
  })
  
  # Create an iterator to stream the data
  next_machine_data <- reactive({
    input$refresh  # Refresh if button clicked
    interval <- max(as.numeric(input$interval), 30)
    invalidateLater(interval * 1000, session)
    # nextElem(imachineData)
    return(tryCatch(nextElem(imachineData(),
             error=function(e){machineData()})))
  })
  
  # Get last update time
  lastUpdateTime <- reactive({
    next_machine_data() # Trigger this when machineData updated
    Sys.time()
  })
  
  # Render the machine select manu
  output$machineSelect <- renderUI(
    {sn <- unique(as.character(data$AT_SERIAL))
    selectInput("serial_num", "Serial Number", choices = sn, selected = sn[1])
    }
    )
  
  # Render the date select manu
  output$dateSelect1 <- renderUI(
    {
      dt <- unique(as.character.Date(data[data$AT_SERIAL == input$serial_num,c("DATE")]))
      selectInput("dateSelect1", "Date", choices = dt, selected=dt[1])
    }
  )
  
  # Render map view
  output$map <- renderLeaflet({
    
    # map_data <- na.omit(machineData())
    map_data <- next_machine_data()
    
    # Find the load zone and dump zone geo axises
    ld_lat <- as.numeric(as.character(map_data$LOAD_ZONE_LAT))
    ld_lng <- as.numeric(as.character(map_data$LOAD_ZONE_LON))
    
    ld_map <- data.frame(ld_lat, ld_lng, map_data$zone)
    
    # Create a palette that maps factor levels to colors
    pal <- colorFactor(c("navy", "red"), domain=c("Dump", "Load"))
    
    # Plot map
    leaflet(ld_map) %>% addTiles(
#       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#       attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      # 'http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png'
      'http://{s}.tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png'
    ) %>%
      setView(lng = median(map_data$Longitude), lat = median(map_data$Latitude), zoom = 18) %>%
      addMarkers(map_data$Longitude, 
                 map_data$Latitude,
                 popup=input$serial_num) %>%
      addCircleMarkers(lng=~ld_lng, 
                       lat=~ld_lat, 
                       color=~pal(map_data.zone),
                       stroke=FALSE,
                       fillOpacity = 0.5,
                       popup=~map_data.zone)
   
  })
  
#   # Get the zoom level and keep it
#   observe({
#     isolate({
#       new_zoom <- 6
#       if(!is.null(input$map_zoom)){new_zoom <- input$map_zoom}
#       leafletProxy('map') %>% mapOptions(zoomToLimits = new_zoom)
#     })
#   })
  

  # Use an seperate observer to add markers as needed
#   observe({
#     map_data <- na.omit(machineData())
#     leafletProxy("map", data=map_data) %>%
#       clearShapes() %>%
#       addMarkers(~Longitude, ~Latitude)
#   })
  
  # get results for avg_q_z_duration
  output$avg_q_z_box <- renderInfoBox({
    machine_data <- machineData()
    average_qz_duration <- mean(as.numeric(machine_data$QUEUE_ZONE_DURATION))
    
    infoBox("Avg Queue Dur", 
            paste0(" ", round(average_qz_duration), "  Secs"), 
            icon=icon("clock-o"),
            color = if(average_qz_duration > 500){"red"}else{"aqua"})
  })
  
  # get results for num_cycles
  output$num_cycle_box <- renderInfoBox({
    machine_data <- machineData()
    cycle_id <- unique(machine_data$cycleid)
    infoBox("Num Cycles", 
            paste0(" ", length(cycle_id), "  Cycles"), 
            icon=icon("recycle"),
            color=if(length(cycle_id) < 10){"red"}else{"aqua"})
  })
  
  # get results for fuel rate
  output$fuel_box <- renderInfoBox({
    machine_data <- machineData()
    infoBox("Fuel", 
            paste0(" ", round(sum(machine_data$D_L_AT_FUEL, na.rm=TRUE))), 
            icon=icon("tint"),
            color = if(sum(machine_data$D_L_AT_FUEL, na.rm=TRUE) > 200){"red"}else{"aqua"})
  })
  
  # show site name
  output$site_name <- renderText({
    machine_data <- machineData()
    paste0("SITE: ", machine_data$site[1])
  })
  
  # get rainfall info
  output$rain_box <- renderInfoBox(
    {
      machine_data <- machineData()
      infoBox("Rainfall", paste0(mean(machine_data$DailyRainfall), "%"), icon=icon("umbrella"), width = 2.5)
    }
  )
  
  # get ground status
  output$ground_box <- renderInfoBox(
    {
      machine_data <- machineData()
      infoBox("Ground Status", ifelse(machine_data$TractorTime < 2, "good", "bad"), icon=icon("road"))
    }
  )
  
  
  #
  # Site statistics dashboard
  #
  
  # select a site
  output$siteSelect <- renderUI({
    st <- unique(as.character(data$site))
    radioButtons("siteSelect", "Site", choices = st, selected=st[1])
  })
  
  # Render the date select manu
  output$dateSelect2 <- renderUI(
    {
      dt <- unique(as.character.Date(data[data$site == input$siteSelect,c("DATE")]))
      selectInput("dateSelect2", "Date", choices = dt, selected=dt[1])
    }
  )
  
  
  # filter data for each site
  siteData <- reactive({
    if(is.na(input$dateSelect2)){data[data$site == input$siteSelect,]}
    else{data[data$site == input$siteSelect & data$DATE == input$dateSelect2,]}
  })
  
  # create a fill factor parameter
  output$fill_factor <- renderUI({
    numericInput("fill_factor", "Fill Factor", 1, min=0.8, max=1.2, step=0.05)
  })
  
  # Plot number of cycles per machine
  output$cp_plot <- renderPlot({
    capacity <- siteData()[,c("AT_SERIAL", "site", "DATE", "Capacity")]
    capacity_by_machine_day <- aggregate(capacity, 
                                         by=list(capacity$AT_SERIAL), 
                                         FUN=mean,
                                         na.rm=TRUE) 
    
    cycle <- siteData()[, c("AT_SERIAL", "site", "DATE", "cycleid")]
    cycle_by_machine_day <- aggregate(cycle,
                                      by = list(cycle$AT_SERIAL),
                                      FUN = length
                                      )
    # actual_payload <- site_data$Capacity * input$fill_factor
    # Bar graph of number of cycles
    ggplot(data=cycle_by_machine_day, aes(x=Group.1, y=cycleid, fill=Group.1)) +
      geom_bar(stat="identity") +
      xlab("Serial Number") +
      ylab("Number of Cycles") +
      ggtitle("Number of Cycles by Serial Number") +
      guides(fill=guide_legend(title=NULL)) +
      theme(legend.position="bottom")
  })
  
  # Plot line chart for cycle time vs. timestamp
  output$ct_line <- renderPlot({
    line_data <- na.omit(siteData()[, c("AT_SERIAL", "DZ_LZ_TRAVEL_START", "CYCLE_TIME")])
    ggplot(data = line_data, aes(x=DZ_LZ_TRAVEL_START, y=CYCLE_TIME, 
                                 group=AT_SERIAL, shape=AT_SERIAL, color=AT_SERIAL)) +
      geom_line() +
      geom_point() +
      xlab("Date Time") +
      ylab("Cycle Time (mins)") +
      scale_x_datetime() +
      ggtitle("Time of cycles by Datetime") +
      guides(fill=guide_legend(title=NULL)) +
      theme(legend.position = "bottom")
  })
  
  
}