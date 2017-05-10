if (!require("devtools")) install.packages("devtools")
if (!require("leaflet.minicharts")) devtools::install_github("rte-antares-rpackage/leaflet.minicharts")
if (!require("rgdal")) install.packages("rgdal")
if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggvis")) install.packages("ggvis")
if (!require("rCharts")) install.packages("rCharts")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("DT")) install.packages('DT')
if (!require("geojsonio")) install.packages('geojsonio')


source("read_aviation.R")

ui <- fluidPage(
  tabsetPanel(
    type='tabs',
    tabPanel("Overview",
             fluidRow(column(6,h3('Dataset'),
                             tags$p("For our data visualization project, we chose to work with the National Transporation Safety Board's (NTSB)",
                                    tags$a(href="https://www.ntsb.gov/_layouts/ntsb.aviation/index.aspx","aviation accident database"),
                                    ". This database contains aviation accident reports from 1982 to today. We did not include 2017 data 
                                    since we wanted to focus on full years' worth of data. In total, there are around 80,000 observations and
                                    31 columns reporting information such as geo-info of where the accidents occured and number of injuries sustained. "
                                    ,br(),tags$p("In the following table, we summarize the four major features that we investigated:")),
                             tableOutput("summary"),
                             br(),
                             tags$p("We also focused on the following categorical features:",br(),
                                    tags$ul(
                                      tags$li(tags$b("board.phase.of.flight"),": the phase the flight was in at the time of the accident."),
                                      tags$li(tags$b("aircraft.damage"),": the degree of damage sustained by the plane during the accident."),
                                      tags$li(tags$b("amateur.build"),":  whether the plane is built by an amateur or a professional.")
                                    )
                             )),
                      column(6,h3('Technique & Graphs'),
                             tags$p("We used the following packages and techniques to build the graphs: ",br(),
                                    tags$ul(
                                      tags$li("javascript"),
                                      tags$li("ggvis"),
                                      tags$li("leaflet"),
                                      tags$li("ggplot")
                                    ),br(),
                                    tags$p("We have four visualizations of this data. We will display the visualizations from the most general to the most specific, with descriptions for each graph. 
                                           After displaying the graphs, you can play around with the data explorer if you are interested in digging through the data. The display order is shown as below:
                                           "),br(),
                                    tags$ul(
                                      tags$li("Choropleth map"),
                                      tags$li("Clustered map"),
                                      tags$li("Heatmap"),
                                      tags$li("Time Series Plot"),
                                      tags$li("Data Explorer")
                                    ))
                             ))),
    tabPanel("Choropleth",
             fluidRow(column(3,
                             wellPanel(
                               selectInput('injury_type_choropleth', label = 'Type of Injury:',
                                           choices= c("Fatal Injury" = "Total_Fatal_Injuries",
                                                      "Serious Injury" = "Total_Serious_Injuries",
                                                      "Minor Injury" = "Total_Minor_Injuries",
                                                      "All Injury Types" = "Total_Injuries"),
                                           multiple =FALSE, selected = c("Fatal Injury"))
                             ),
                             tags$h4("Description"),
                             tags$p("We first take a general view of the data by aggregating the different types of injuries for each state.
                                    The color scheme highlights the states that have the highest number of injuries. We've also added a slider
                                    for the year so that we can see how this data changes over time, as well as a drop down menu to select
                                    which type of injury to focus on.")),
                      column(9,h3("US Flight Injuries per State"),leafletOutput("choropleth"), sliderInput("year_slider", label = NULL, min = 1982, max = 2016, value = 1982, ticks = FALSE, round = TRUE,
                                             sep = "", step = 1, width = '100%'))
             )),
    
    tabPanel("Map",
             fluidRow(column(3,
                             wellPanel(
                               selectInput('accident_type', label = 'What type of accident?', 
                                           choices=c("Substantial"="Substantial",
                                                     "Destroyed" = "Destroyed",
                                                     'Minor' = 'Minor'), multiple =TRUE),
                               selectInput('aircraft', label = 'What type of airplane?', 
                                           choices=c("Airplane"="Airplane","Weight-Shift"="Weight-Shift",
                                                     "Powered Parachute" = "Powered Parachute",
                                                     "Helicopter"= "Helicopter","Gyrocraft"="Gyrocraft",
                                                     "Gyroplane"="Gyroplane","Powered-Lift"="Powered-Lift",
                                                     "Balloon"="Balloon","Ultralight"="Ultralight"), multiple =TRUE),
                               selectInput('phase', label = 'broad phase of flight', 
                                           choices=c("Approach"="APPROACH", "Takeoff" = "TAKEOFF",
                                                     "Maneuvering"= "MANEUVERING","Landing"="LANDING",
                                                     "Descent"="DESCENT",'Taxi'='TAXI','Standing'='STANDING',
                                                     'Cruise'='CRUISE','Go-around'='GO-AROUND','Climb'='CLIMB'), multiple =TRUE),
                               selectInput("buildby", label="who build the aircraft?",
                                           choices = c("Amateur" = 'Yes',
                                                       "Professional" = 'No'), multiple =TRUE),
                               plotOutput("barplot", height = 200)
                              
                             )),
                      column(9,
                             tags$h4("Description"),
                             tags$p("We get more specific about the location with our next plot, using the longitude and latitude values from 
                                    the data for plotting the exact location of each flight accident. By zooming in on the map we can isolate
                                    exactly where an accident occurred and even hover of the point to get some additional information about
                                    that incident. Additionally we color the single accident points blue when no fatal injuries occurred, and
                                    red otherwise. We added multiple select boxes so that the user can narrow down the type of accident,
                                    type of airplane (or mode of flight), phase the flight was in, and whether the aircraft was
                                    professionally built or not."),
                             tags$p("In addition to the graph, we give a bar plot that compares the total number of fatal injuries to the total
                                    number of injuries of any other type. The user can play around with the choices in the multiple select menus
                                    to see which combinations produce a larger number of fatal injuries than other types of injuries."),
                             h3("Clustered Map of US Flight Accidents"),leafletOutput("mymap"))
             )),
    tabPanel("Heatmap",
             fluidRow(
               tags$head(tags$style(HTML('a[class="ggvis-dropdown-toggle"]{display:none;}'))),
               column(10, ggvisOutput("heatmap"), uiOutput("plot_ui")),
               column(2,
                    wellPanel(
                      selectInput('injury_type', label = 'Type of Injury:',
                                  choices= c("Fatal Injury" = "Total_Fatal_Injuries_Per_Incident",
                                             "Serious Injury" = "Total_Serious_Injuries_Per_Incident",
                                             "Minor Injury" = "Total_Minor_Injuries_Per_Incident",
                                             "All Injury Types" = "Total_Injuries_Per_Incident"),
                                  multiple =FALSE, selected = c("Fatal Injury")),
                      selectInput('state_order', label = 'Order of States:',
                                  choices = c("Alphabetical" = "state_alphabetical",
                                              "Hierarchical" = "state_hierarchical"),
                                  multiple = FALSE, selected = c("Alphabetical"))),
                           tags$h4("Description"),
                           tags$p("We go a little more in depth on the state aggregated injuries in this 
                                  heatmap, where the color scheme helps us locate the year and state 
                                  combinations that produced the highest number of injuries per accident.
                                  Dividing the number of injuries by the number of flight accidents for a given
                                  state and year has the effect of increasing the flight accident injury
                                  rates for states that experienced fewer flight accidents, but whose accidents
                                  affected a larger number of people on the flight."),
                           tags$p("We again give the user a drop down menu where they can focus on different types of 
                                  injuries, as well as the option to reorder the states based on the hierarchical clustering
                                  order of their injury data.")))
    ),
    tabPanel("Time Series",
             fluidRow(
               column(3,
                    wellPanel(
                      selectInput('y', label = 'Type of Injury:',
                                  choices= c("Fatal Injury" = "Total_Fatal_Injuries",
                                             "Serious Injury" = "Total_Serious_Injuries",
                                             "Minor Injury" = "Total_Minor_Injuries",
                                             "All Injury Types" = "Total_Injuries"),
                                  multiple =FALSE, selected = c("Fatal Injury"))),
                    tags$h4("Description"),
                    tags$p("Now that we've seen all the overview information of flight accident injuries, we proceed to focus on comparing 
                           the injuries by state in a time series line plot. Without any states selected, we show the time series lines
                           for every state. The user can then compare specific states by clcking on them in the legend. The hover functionality
                           makes it easy to compare the year and number of injuries for a subset of the states (again giving the option to choose
                           the type of injury).")),
             column(9,h3("Time Series Plot of Flight Accident Injuries by State"),
                    p("Interact with the legend to compare different states"),
                    showOutput("rchart", "dimple"))
    )),
    tabPanel("Data explorer",
             fluidRow(
               column(3,
                      selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
               ),
               column(3,
                      conditionalPanel("input.states",
                                       selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                      )
               )
             ),
             hr(),
             dataTableOutput("accidenttable")
    )
  )
)

injury_name_func <- function(injury_type){
  if (injury_type %in% c("Total_Fatal_Injuries_Per_Incident",
                        "Total_Serious_Injuries_Per_Incident",
                        "Total_Minor_Injuries_Per_Incident")){
    injury <- strsplit(injury_type, '_')[[1]][2]
  } else {
    injury <- "Total"
  }
  return(injury)
}

injury_name_func2 <- function(injury_type){
  if (injury_type %in% c("Total_Fatal_Injuries",
                         "Total_Serious_Injuries",
                         "Total_Minor_Injuries")){
    injury <- strsplit(injury_type, '_')[[1]][2]
  } else {
    injury <- "Total"
  }
  return(injury)
}




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$summary<- renderTable({
    table<- d %>% select(Total.Fatal.Injuries,
                         Total.Serious.Injuries, Total.Minor.Injuries, Total.Uninjured) 
    
    
    
    tmp <- do.call(data.frame, 
                   list(mean = apply(table, 2, mean),
                        sd = apply(table, 2, sd),
                        min = apply(table, 2, min),
                        max = apply(table, 2, max)
                   ))
    tmp$name <- names(table)
    tmp[,c('name','mean','sd','min','max')]
    
  })
  

  d <- aviation_US %>%
    mutate(Total.Serious.Injuries = ifelse(is.na(Total.Serious.Injuries),0,Total.Serious.Injuries),
           Total.Fatal.Injuries = ifelse(is.na(Total.Fatal.Injuries),0,Total.Fatal.Injuries),
           Total.Uninjured = ifelse(is.na(Total.Uninjured),0,Total.Uninjured),
           Total.Minor.Injuries = ifelse(is.na(Total.Minor.Injuries),0,Total.Minor.Injuries),
           Longtitude = longitude2,
           Latitude = latitude2) %>%
    select(city,state, Latitude, Longitude,
           Injury.Severity, Aircraft.Damage, 
           Aircraft.Category, Amateur.Built, Broad.Phase.of.Flight,
           Total.Fatal.Injuries,
           Total.Serious.Injuries, Total.Minor.Injuries, Total.Uninjured
            ) %>%
    na.omit() 
    
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(d, state %in% input$states) %>%
        `$`(city) %>%
        unique() %>%
        sort()
      
    }
    #if (is.null(input$states)) character(0) else {print(input$states)}
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  output$accidenttable<- DT::renderDataTable({

  
    df <- d %>%
      filter(
        is.null(input$states) | state %in% input$states,
        is.null(input$cities) | city %in% input$cities
      ) %>%
      mutate(Fatal =Total.Fatal.Injuries,
             Injured = Total.Serious.Injuries+ Total.Minor.Injuries)%>%
      select(Latitude,Longitude,Broad.Phase.of.Flight,Fatal, Injured)

      DT::datatable(df)
    })
  
  filter_data <- reactive({
    a <- d %>%
      gather(conditions, measurement,Aircraft.Damage:Broad.Phase.of.Flight, factor_key=TRUE)
    
    if (!is.null(input$accident_type)){
      a <- a[a$measurement %in% input$accident_type,]
    }
    if (!is.null(input$aircraft)){
      a <- a[a$measurement %in% input$aircraft,]
    }
    if(!is.null(input$buildby)){
      a<- a[a$measurement ==input$buildby,]
    }
    if (!is.null(input$phase)){
      a <- a[a$measurement %in% input$phase,]
    }
    a
    
  })
  
  output$barplot <- renderPlot({
    c<- filter_data() %>%
      mutate(Injured = Total.Serious.Injuries+Total.Minor.Injuries,
             Fatal=Total.Fatal.Injuries) %>% 
      select(Fatal,Injured) %>%
      colSums()
  
    barplot(c,
            main = "Fatal vs. Injured",
            col = c('#F18904','#36688D'),
            border = 'white')
  })
  
  output$choropleth <- renderLeaflet({
    # Code for this choropleth plot was produced following the tutorial at
    # https://rstudio.github.io/leaflet/choropleths.html
    injury_name <- injury_name_func2(input$injury_type_choropleth)
    aviation_single_year <- aviation_long %>% filter(year == input$year_slider)
    colnames(aviation_single_year) <- c('State_abbr', colnames(aviation_single_year)[2:12])
    state_data <- left_join(left_join(data.frame(State_full_name = states$NAME), State_name_mapping, by = 'State_full_name'), aviation_single_year, by = 'State_abbr')
    states$Injuries <- state_data[, input$injury_type_choropleth]
    
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addTiles()
    
    bins <- c(0, 10, 20, 50, 100, 200, 500)
    pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s injuries",
      states$NAME, states$Injuries, tolower(injury_name) 
    ) %>% lapply(htmltools::HTML)
    
    m %>% addPolygons(
      fillColor = ~pal(Injuries),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% 
      addLegend(pal = pal, values = ~Injuries, opacity = 0.7, title = paste(injury_name, "Injuries"),
                position = "bottomright")
    
  })
  
  
  
  output$mymap <- renderLeaflet({
    
    a<-filter_data()
    
    a_fatal<- subset(a, Total.Fatal.Injuries!=0)
    a_non_fatal<- subset(a, Total.Fatal.Injuries==0)
    m <- leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lng = -97.642667, lat =39.532024,  zoom = 4) %>% 
      addCircleMarkers(data =a_fatal, lng = ~Longitude, lat = ~Latitude,
                       color ='red',opacity =1,weight=1,
                       popup = ~paste("<style> div.leaflet-popup-content-wrapper { opacity: 0.8; } </style>",
                                      "<h4>Flight Details</h4>",
                                      "Flight From: ", a_fatal$city, ", ", a_fatal$state,'</br>',
                                      "Fatal: ", a_fatal$Total.Fatal.Injuries,'</br>',
                                      "Injuries: ", a_fatal$Total.Serious.Injuries+a_fatal$Total.Minor.Injuries,'</br>',
                                      sep = ""),clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data =a_non_fatal, lng = ~Longitude, lat = ~Latitude,
                       color ='blue',opacity =1,weight=1,
                       popup = ~paste("<style> div.leaflet-popup-content-wrapper { opacity: 0.8; } </style>",
                                      "<h4>Flight Details</h4>",
                                      "Flight From: ", a_non_fatal$city, ", ", a_non_fatal$state,'</br>',
                                      "Fatal: ", a_non_fatal$Total.Fatal.Injuries,'</br>',
                                      "Injuries: ", a_non_fatal$Total.Serious.Injuries+a_non_fatal$Total.Minor.Injuries,'</br>',
                                      sep = ""),clusterOptions = markerClusterOptions()) 
    
  })
  renderChart3 <- function( expr, env = parent.frame(), quoted = FALSE ){
    func <- shiny::exprToFunction(expr, env, quoted)
    function() {
      rChart_ <- func()
      cht_style <- sprintf("<style>.rChart {width: %spx; height: %spx} </style>", 
                           rChart_$params$width, rChart_$params$height)
      cht <- paste(
        capture.output(cat(
          rChart_$print()
          ,render_template(
            rChart_$templates$afterScript %||% 
              "<script></script>"
            , list(chartId = rChart_$params$dom, container = rChart_$container)
          )
          ,sep = ""
        ))
        , collapse = "\n")
      HTML(paste(c(cht_style, cht), collapse = "\n"))
    }
  }
  
  
  output$rchart<- renderChart3({

    data <- aviation_long %>%
      ungroup()%>%
      mutate(state= factor(state),
             year = as.character(year))

    line <-dPlot(y=input$y, x='year',
                 groups="state",
                 data=data,
                 type="line",
                 height =600,
                 width =1000,
                 bounds = list(x=60,y=30,height=450,width=700))
    
    line$defaultColors( "#!d3.scale.ordinal().range([
                        '#ff0000', '#cc0000', '#330d0d', '#bf6060', '#f2b6b6',
                        '#403030', '#735a56', '#f29979', '#a64200', '#4c2a13',
                        '#f28100', '#d9a66c', '#bfa98f', '#ffaa00', '#7f5500',
                        '#403010', '#eaf279', '#888c46', '#ccff00', '#3b4d26',
                        '#338000', '#d9ffbf', '#39e639', '#53a674', '#40ffbf',
                        '#004033', '#00d6e6', '#007780', '#002933', '#00aaff',
                        '#003059', '#5989b3', '#bfe1ff', '#4d5766', '#7989f2',
                        '#bfc8ff', '#000040', '#25008c', '#8100f2', '#c480ff',
                        '#6c468c', '#352040', '#83008c', '#e6ace2', '#ff40d9',
                        '#e5007a', '#33001b', '#994d75', '#73002e', '#d9003a']).domain(['R','D','O','U'])!#")
    line$legend(x=800,y=60,height=500,width=180,horizontalAlign='right')

    line$setTemplate(
      afterScript = 
        '<script>
      myChart.axes.filter(function(ax){return ax.position == "x"})[0].titleShape.text(opts.xlab)
      myChart.axes.filter(function(ax){return ax.position == "y"})[0].titleShape.text(opts.ylab)
      // This is a critical step.  By doing this we orphan the legend. This
      // means it will not respond to graph updates.  Without this the legend
      // will redraw when the chart refreshes removing the unchecked item and
      // also dropping the events we define below.
      myChart.legends = [];
      // This block simply adds the legend title. I put it into a d3 data
      // object to split it onto 2 lines.  This technique works with any
      // number of lines, it isn\'t dimple specific.
      svg.selectAll("title_text")
      .data(["CLICK BELOW:"])
      .enter()
      .append("text")
      .attr("x", 830)
      .attr("y", function (d, i) { return 40 + i * 14; })
      .style("font-family", "sans-serif")
      .style("font-size", "16px")
      .style("color", "Black")
      .text(function (d) { return d; });
      // Get a unique list of Owner values to use when filtering
      var filterValues = dimple.getUniqueValues(data, "state");
      var tmp  = [];
      // Get all the rectangles from our now orphaned legend
      // Add a click event to each rectangle
      l.shapes.selectAll("rect").style("opacity", 0.2);
      l.shapes.selectAll("rect")
      .on("click", function (e) {
      // This indicates whether the item is already visible or not
      var show = false;
      var newFilters = [];
      
      // If the filters contain the clicked shape hide it
      tmp.forEach(function (f) {
      if (f === e.aggField.slice(-1)[0]) {
      show = true;
      } else {
      newFilters.push(f);
      }
      });
      if (show) {
      d3.select(this).style("opacity", 0.2);
      } else {
      newFilters.push(e.aggField.slice(-1)[0]);
      d3.select(this).style("opacity", 0.8);
      }
      // Update the filters
      tmp = newFilters;
      if (newFilters.length == 0 ){
      newFilters = filterValues;
      }
      // Filter the data
      myChart.data = dimple.filterData(data, "state", newFilters);
      // Passing a duration parameter makes the chart animate. Without
      // it there is no transition
      myChart.draw(800);
      myChart.axes.filter(function(ax){return ax.position == "x"})[0].titleShape.text(opts.xlab)
      myChart.axes.filter(function(ax){return ax.position == "y"})[0].titleShape.text(opts.ylab)
      });
      </script>'
    )
    
    
    line$addParams(dom = 'myChart')

    return(line)
    
  })
  
  
  vis <- reactive({
    aviation_wide <- eval(parse(text = paste("spread(aviation_long %>% select(state, year,", input$injury_type, "), key = year, value =",
                                             input$injury_type, ")")))
    state_hierarchical <- aviation_wide$state[hclust(dist(aviation_wide[, 2:36]))$order]
    state_alphabetical <- aviation_wide$state[order(aviation_wide$state)]
    
    aviation_long$state <- factor(aviation_long$state, levels = get(input$state_order))
    fys <- as.data.frame(xtabs(get(input$injury_type) ~ year + state, aviation_long))
    injury_name <- injury_name_func(input$injury_type) 
    
    fys %>% 
      ggvis(x = ~year, y = ~state, fill = ~Freq) %>% 
      layer_rects(width = band(), height = band(), stroke := "#fdd49e",
                  stroke.hover:= "white", fillOpacity:= 1,
                  fillOpacity.hover := 0.6) %>%
      scale_nominal("x", padding = 0, points = FALSE) %>%
      scale_nominal("y", padding = 0, points = FALSE) %>% 
      add_tooltip(function(data){
        paste0("<b>", data$state, " (", data$year, ")</b>", "<br>",
               round(data$Freq, 2), " injuries per accident")
      }, "hover") %>% 
      scale_numeric("fill", range = c("#fdd49e", "#b30000")) %>%
      hide_legend("fill") %>% 
      add_axis("x", grid = FALSE, offset = -10, tick_padding = 3, title = "Year", title_offset = 30,
               properties = axis_props(axis = list(stroke = "white"), ticks = list(stroke = "white"),
                                       title = list(fontSize = 14))) %>% 
      add_axis("y", grid = FALSE, offset = -10, tick_padding = 0, title = "State", title_offset = 40,
               properties = axis_props(axis = list(stroke = "white"), ticks = list(stroke = "white"),
                                       title = list(fontSize = 14))) %>%
      add_axis("x", grid = FALSE, orient = "top", ticks = 0, title = paste0(injury_name, " Injuries per Flight Accident"),
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0),
                 ticks = list(stroke = "white"),
                 title = list(fontSize = 24))) %>% 
      set_options(height = 700, width = 1200, resizable = FALSE)
  }) 
  
  vis %>% bind_shiny("heatmap", "plot_ui")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
