library(shinydashboard)
library(leaflet)

dashboardPage(
  dashboardHeader(title = "CSC PI Dashboard",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Support",
                                 message = "Machine Status today",
                                 icon = icon("life-ring")
                               )),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "Fuel rate too high today",
                                 status = "warning",
                                 icon("exclamation-triangle")
                               ),
                               notificationItem(
                                 text = "Five machines are running on the site",
                                 status = "success",
                                 icon("truck")
                               )
                               ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Progress"
                                        ),
                               taskItem(value = 17, color = "red",
                                        "Check Sensor Status"
                                        ),
                               taskItem(value = 75, color = "yellow",
                                        "Replace Sensor"
                                        )
                               )
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Machine Map", tabName = "map", icon = icon("map-marker")),
      menuItem("Site Metrics", tabName = "stat", icon = icon("bar-chart")),
      menuItem("Sensor Management", tabName = "sensor", icon = icon("bullseye"))
              )
  ),
  dashboardBody(
    
    
    tabItems(
      # Machine Map Tab
      tabItem(tabName = "map",
              fluidRow(
                
                # Main panel 
                column(width = 9, 
                       # Box to display map
                       box(width = NULL, solidHeader = TRUE,
                           title = textOutput("site_name"),  # print out site name
                           status = "info",
                         leafletOutput("map", height=500)),
                       infoBoxOutput("avg_q_z_box"),
                       infoBoxOutput("num_cycle_box"),
                       infoBoxOutput("fuel_box"),
                       
                       # get rainfall info
                       infoBoxOutput("rain_box"),
                       
                       # get ground condition 
                       infoBoxOutput("ground_box")
                      ),
                
                # Right hand side filters
                column(width = 3,
                       # Box for user select machine
                       box(
                           title = "Inputs", solidHeader = TRUE,
                           width=NULL, status = "warning",
                           
                           #Serial number filter
                           uiOutput("machineSelect"),
                           #date select filter
                           uiOutput("dateSelect1")
                          ),
                       
                       # Box to show update time
                       box(width = NULL, status = "warning",
                           selectInput("interval", "Refresh Interval",
                             choices = c(
                               "30 seconds" = 30,
                               "1 minute" = 60,
                               "2 minutes" = 120,
                               "5 minutes" = 300,
                               "10 minutes" = 600
                          ),
                          selected = "60"
                           ),
                          uiOutput("timeSinceLastUpdate"),
                          actionButton("refresh", "Refresh now"),
                          p(class = "text-muted",
                            br(),
                            "Source data updates every 30 seconds.") 
                        )
                      )
              )
              ),
      
      # Site Statistics Tab
      tabItem(tabName = "stat",
              fluidRow(
                # First Column
                column(width = 4.5,
                       # Plot payload vs cycle
                       box(
                         title = "By Machine", solidHeader = TRUE,
                         status = "info",
                         plotOutput("cp_plot"))
                       ),
                
                # Second Column
                column(width = 4.5,
                       # Plot timestamp vs cycle time
                       box(
                         title = "By Time", solidHeader = TRUE,
                         status = "info",
                         plotOutput("ct_line"))
                       )
              ),
              fluidRow(
           
                       # Filters
                       box(
                         title = "Site", solidHeader = TRUE,
                         width = 4, status = "warning",
                         
                         # select a site
                         uiOutput("siteSelect")
                       ),
                       
                       box(
                         title = "Date", solidHeader = TRUE,
                         width = 4, status = "warning",
                         # select a date
                         uiOutput("dateSelect2")
                       ),
                         
                       box(
                         title = "Fill Factor", solidHeader = TRUE,
                         width = 4, status = "warning",
                         # select a fill factor
                         uiOutput("fill_factor")
                       )

              )
              )
      
      )
    
    )
    
  )
