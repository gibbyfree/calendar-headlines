#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(lubridate)
library(colourpicker)

# UI -- split into separate file?
ui <- fluidPage(
  
  # ui theme
  theme = shinythemes::shinytheme("simplex"),  
  # App title
  headerPanel("Headline Calendar-ify-er"),
  
  # sidebar layout 
  sidebarLayout(
    
    # panel for file input
    sidebarPanel(
      
      # select a file
      fileInput('datafile', 'Choose CSV File',
                accept = c("text/csv", "text/comma-separated-values,text/plain")),
      # line
      tags$hr(),
      
      # check for header
      checkboxInput('header', 'Does your file have a header?', TRUE),
      
      # prompt for keyword 
      textInput('toFind', 'What keyword are you searching for?', value=""),
      
      colourInput('selectedColor', 'Select a color.', value="orangered3"),
      
      dateRangeInput('dateRange', 'Choose your date range.'),
      
      # action button
      actionButton('go', 'Go')
    ),
    
    # panel for displaying output
    mainPanel(
      # output calendar
      plotOutput("calendar")
    )
    
  )
)

# server logic
server <- function(input, output) {
  
  observeEvent(input$go,  {
    
    # read file
    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      rawData <- read.csv(infile$datapath, 
                          header = input$header,
                          strip.white = TRUE)
      dateCol <- rawData[,c(1)]
      headlineCol <- rawData[c(2)]
    })
    
    
    # generate and render calendar 
    output$calendar <- renderPlot({
      set.seed(42)
      library(ggplot2)
      library(lubridate)
      
      
      dates <- seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), by = "1 day")
      counts <- 1:length(dates)
      filterField <- sample(1:42,length(dates),replace=T)
      df <- data.frame(dates, counts, filterField)
      
      wom <- function(date) { # week-of-month
        first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
        return((mday(date)+(first-2)) %/% 7+1)
      }
      df$month <- month(df$dates)
      df$day   <- mday(df$dates)
      
      rng   <- range(df$dates)
      rng   <- as.Date(paste(year(rng),month(rng),1,sep="-"))
      start <- rng[1]
      end   <- rng[2]
      month(end) <- month(end)+1
      day(end)   <- day(end)  -1
      
      cal <- data.frame(dates=seq(start,end,by="day"))
      cal$year  <- year(cal$date)
      cal$month <- month(cal$date)
      cal$cmonth<- month(cal$date,label=T)
      cal$day   <- mday(cal$date)
      cal$cdow  <- wday(cal$date,label=T)
      cal$dow   <- wday(cal$date)
      cal$week  <- wom(cal$date)
      
      cal        <- merge(cal,df[,c("dates","counts")],all.x=T)
      
      print(ggplot(cal, aes(x=cdow,y=-week))+
              geom_tile(aes(fill=counts,colour="grey50"))+
              geom_text(aes(label=day),size=3,colour="grey20")+
              facet_wrap(~cmonth, ncol=3)+
              scale_fill_gradient(low = "moccasin", high = input$selectedColor, na.value="white")+
              scale_color_manual(guide=F,values="grey50")+
              scale_x_discrete(labels=c("S","M","T","W","Th","F","S"))+
              theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
              theme(panel.grid=element_blank())+
              labs(x="",y="")+
              coord_fixed())
      
    }
    )})
}

shinyApp(ui, server)
