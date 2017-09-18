# SharkPlotViewer: interactive navigation of shark scatterplots for Sharkduino
# By Dara Kharabi
#

library(shiny) # web app library
library(ggplot2) # better graphs
library(scales) # for better scales
library(cowplot) # for arranging plots in grids
source("../packages/import_data.R") # for importing data
source("../packages/subsample.R") # for subsampling


# Read in raw CSV. Path must be changed if you're not on Dara's laptop.
head.data = import_data("~/projects/Sharkduino/sharkduino_R_analysis/data/tmp-data.csv");
# Dataset Name, for pretty titles
head.datasetName = "Shark 2017"
print(head.data);

# Alias ss.simpleszzz from subsample.R
subsample = ss.simple

clamp = function(vec, LB=-Inf, UB=Inf) pmax( LB, pmin( vec, UB))

# Function for making our plots (with lapply)
makeScatterPane = function(ds, data, datasetName = "NO NAME", dataRange = 1:nrow(data), ssres = 1) {
  # Titles/labels/limits for the various plots
  titles = c(
    paste("Accelerometer X Data for Dataset  \"", datasetName, "\"  (SS: 1/", ssres, ")", sep=""),
    paste("Accelerometer Y Data for Dataset  \"", datasetName, "\"  (SS: 1/", ssres, ")", sep=""),
    paste("Accelerometer Z Data for Dataset  \"", datasetName, "\"  (SS: 1/", ssres, ")", sep=""),
    paste("Gyro X Data for Dataset  \"", datasetName, "\"  (SS: 1/", ssres, ")", sep=""),
    paste("Gyro Y Data for Dataset  \"", datasetName, "\"  (SS: 1/", ssres, ")", sep=""),
    paste("Gyro Z Data for Dataset  \"", datasetName, "\"  (SS: 1/", ssres, ")", sep="")
  )
  
  ylabs = c(
    "Accelerometer X-Axis (Gs)",
    "Accelerometer Y-Axis (Gs)",
    "Accelerometer Z-Axis (Gs)",
    expression("Gyro X-Axis ("*degree*"/s)"),
    expression("Gyro Y-Axis ("*degree*"/s)"),
    expression("Gyro Z-Axis ("*degree*"/s)")
  )
  
  # Scaling stuff.
  myLims = list(
    c(-2.0, 2.0),    # Accelerometer X
    c(-2.0, 2.0),    # Accelerometer Y
    c(-2.0, 2.0),    # Accelerometer Z
    c(-1000,  1000), # Gyroscope X
    c(-1000,  1000), # Gyroscope Y
    c(-1000,  1000)  # Gyroscope Z
  )
  
  # Make plot with GGPlot2
  myPlot <<- ggplot(
    data.frame(
      dates = subsample(data[dataRange][[7]], ssres),
      series = subsample(data[dataRange][[ds]], ssres)
    )) +
    geom_line(aes(x=dates, y=series), size=0.4) +
    labs(
      x="Time (hh:mm)", 
      y=ylabs[ds], 
      title=titles[ds] 
    ) + 
    background_grid(major = 'xy', minor = "none") + 
    scale_x_datetime(labels = date_format("%H:%M:%S")) +
    ylim(myLims[[ds]])
  
  return(myPlot)
}




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SharkPlotViewer"),
  fluidRow(
    plotOutput("sharkPlot", height="450px")
  ),

  fluidRow(
    column(2, offset = 1,
           selectInput("ds", "Data Series:",
                       c("Accelerometer X" = 1,
                         "Accelerometer Y" = 2,
                         "Accelerometer Z" = 3,
                         "Gyro X" = 4,
                         "Gyro Y" = 5,
                         "Gyro Z" = 6),
                          selected = 1)
           ),
           
           column(8, offset = 0,
                  sliderInput("start",
                              "Data Start Point:",
                              min = 1,
                              max = nrow(head.data),
                              step = 250,
                              value = 900000,
                              width = "100%")
           )
           
    ),
    
    fluidRow(
      column(2, offset = 1,
             radioButtons("mult", "Window Length Multiplier:",
                          c("1x"   =   1,
                            "10x"  =  10,
                            "100x" = 100 )
                          )
             
      ),
      
      column(8, offset = 0,
             sliderInput("len",
                         "Window Length:",
                         min = 0,
                         max = nrow(head.data) / 100,
                         step = 25,
                         value = 250,
                         width = "100%")
      )
      
    )
    
  )
  
  
  oldMult = 1
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    observeEvent({input$mult}, {
      adj <- as.numeric(input$mult)/oldMult
      mult <- as.numeric(input$mult)
      # Control the value, min, max, and step.
      # Step size is 2 when input value is even; 1 when value is odd.
      updateSliderInput(session, "len", value = input$len*adj,
                        min = 0, max = nrow(head.data)/100*mult, step = 25*mult)
      oldMult <<- mult
    })

    ssr = reactive({clamp(trunc(as.numeric(input$len)/20000)*5, 1, 50)})
    endp = reactive({clamp(input$start + clamp(input$len, 1, nrow(head.data)), 1, nrow(head.data))})
    
    
    output$sharkPlot <- renderPlot({
      makeScatterPane(ds = as.numeric(input$ds), 
                      data=head.data, datasetName = head.datasetName, 
                      dataRange = input$start:endp(), 
                      ssres=ssr())
    }, res=120)
    
    output$sharkPlot2 <- renderPlot({
      makeScatterPane(ds = 1, 
                      data=head.data, datasetName = head.datasetName, 
                      dataRange = input$start:endp(), 
                      ssres=ssr())
    }, res=120)
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
  