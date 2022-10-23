library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)


Sys.setlocale("LC_TIME", "C")

#### we load the data
data <- read.csv("data_task_oct22.csv", header = TRUE , sep = ",")
data$date <- as.Date(data$date)



##### Create a data frame with all the variables asked
shiny  <- subset(data, data$package == "shiny")
shiny <- na.omit(shiny)

### formatting and creating a dataframe with all the statistics we wanna know
shiny$week_day <- weekdays(shiny$date)
str(shiny)
min <- NULL
max <- NULL
date.average <- NULL
average <- NULL
date.min <- NULL
date.max <- NULL
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

for( i in 1:length(days)){
  shiny_day <- subset(shiny, shiny$week_day == days[i])
  min[i] <- min(shiny_day$count)
  date.min[i] <- as.character(shiny_day$date[which.min(shiny_day$count)])
  date.max[i] <- as.character(shiny_day$date[which.max(shiny_day$count)])
  date.average[i] <- NA
  max[i] <- max(shiny_day$count)
  average[i] <- mean(shiny_day$count)}
  

#### adapt the data frames for the app 
shiny_table <- data.frame(average,min,date.min,max,date.max,days, row.names = days)
shiny_plot <- data.frame(average,min,max,days, row.names = days)
dates <- data.frame(date.average,date.min ,date.max,days)
dates$date.min <- format(as.Date(dates$date.min), "%d/%m/%y")
dates$date.max <- format(as.Date(dates$date.max), "%d/%m/%y")



ui <- fluidPage(


    titlePanel("Task Interview"),


    sidebarLayout(
        sidebarPanel(
          #### inputs
            h2("First task"),
            selectInput("package",
                        "Choose package", c("All packages",unique(data$package)), multiple = TRUE),
            checkboxInput("line", "regresion line"),

            h2("Second Task"),
            selectInput("day",
                        "Choose Day", c("All days","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), multiple = TRUE),
            actionButton("calculate", "Calculate"),
        ),


        mainPanel(
          #### Outputs
          tabsetPanel(
            tabPanel("First task",
                     h2("First task"),
                     p("Process the data and create an aesthetically appealing visualisation of the evolution of download numbers for each package throughout the year.
                       Feel free to pick any data wrangling and visualisation library and any type of chart that you deem appropriate.
                       You can get as creative as you want with colours, fonts, labels etc."),
                     uiOutput("plotpackage")),
            tabPanel("Second task",
                     h2("Second task"),
                     p("In addition, create a chart showing the average number of downloads of the shiny package per day of the week,
                       as well as the minimum and maximum single-day value obtained, for each day of the week (i. e. Mon-Sun),
                       across the timespan covered in the dataset. For the sake of simplicity, ignore or remove dates without any downloads."),
                     uiOutput("secondtask"))
          ),

        )
    )
)


server <- function(input, output) {
  
  ##### observer event calculate
  observeEvent(input$calculate,{
    
    #### reactives
    packages <- eventReactive(input$calculate,{ input$package})
    days <- eventReactive(input$calculate,{ input$day})

    
    ####################
    #### FIRST TASK ####
    ####################
    output$plotpackage <- renderUI({
      
      #### lapply for each package input
      lapply(packages(), function(p) {
        
        #### naming the outputs
        plotname_text <- paste("text",p,sep = "_")
        plotname_plot <- paste("plot",p,sep = "_")
        
        #### filtering by input
        if (p != "All packages"){data <- subset(data, data$package == p)}
        
        #### output text
        output[[plotname_text]] <- renderText({ text <- paste("Downloads package",p, sep = " ")})
        
        
        #### output plot 
        output[[plotname_plot]] <- renderPlot({ 
          
          if (input$line == TRUE){
            plot <- ggplot(data,aes(x = date, y = count, color = package)) + geom_line() + 
              facet_wrap( ~ package) + scale_x_date(date_breaks = "1 month", date_labels =  "%b") + 
              geom_smooth(method='lm', color= "black") + theme_light() +theme(legend.title = element_text(size = 12, face = "bold"),
                                                                              legend.text=element_text(size=11),
                                                                              text = element_text(size = 14, face = "bold"))
          }
          else{plot <- ggplot(data) + geom_line(aes(x = date, y = count,color = package)) + facet_wrap( ~ package) + 
            scale_x_date(date_breaks = "1 month", date_labels =  "%b")+ theme_light() + theme(legend.title = element_text(size = 12, face = "bold"),
                                                                                              legend.text=element_text(size=11),
                                                                                              text = element_text(size = 14, face = "bold"))
          }
          
          plot}) ##### output plot
        
        
        ##### Creating the UI form
        tagList(
          hr(),
          h2(textOutput(plotname_text)),
          p("let's take a look the time series chart of the downloads during 2022. As extra we can add the regression line to observe the increase/decrease of downloads alon the year. "),
          hr(),
          plotOutput(plotname_plot),
          hr(),
        )### tag list
        
      }) ### Lapply
    
    })### plot packages
    
    #####################
    #### SECOND TASK ####
    #####################
    
    output$secondtask <- renderUI({
      
      lapply(days(), function(p) {

        #### naming the outputs
        plotname_text2 <- paste("text2",p,sep = "_")
        plotname_table2 <- paste("table2",p,sep = "_")
        plotname_plot2 <- paste("plot2",p,sep = "_")
        
        ##### output text2
        output[[plotname_text2]] <- renderText({ text <- paste("Downloads of shiny on",p, sep = " ")})
        
        ##### output table2
        output[[plotname_table2]] <- renderTable({ 
          if (p != "All days"){shiny_table <- subset(shiny_table , shiny_table$days == p)}
          shiny_table$date.min <- format(as.Date(shiny_table$date.min), "%d/%m/%y")
          shiny_table$date.max <- format(as.Date(shiny_table$date.max), "%d/%m/%y")
          colnames(shiny_table)<- c("Average", "Min", "Date Min", "Max", "Date Max", "days")
          shiny_table[1:5]}, rownames = TRUE) #### output table 2
        
        ##### output plot2
        output[[plotname_plot2]] <- renderPlot({

          ##### filtering by days
          if (p != "All days"){
            shiny_plot <- subset(shiny_plot , shiny_plot$days == p)
            dates <- subset(dates , dates$days == p)
          }
          ##### creating the plot
          dates <- unlist(dates[1:3])
          shiny_plot <- melt(shiny_plot, id.vars="days")
          shiny_plot$dates <- dates
          plot <- ggplot(shiny_plot, aes(x=factor(days, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),y =value, col = variable, fill = variable)) + geom_bar(stat="identity", position = "dodge", color = "black") + 
            scale_fill_manual( values = c("#619CFF", "#F8766D", "#00BA38")) +
            geom_text(aes(label = dates), vjust= -0.5, position = position_dodge(0.9), fontface= "bold", color = "black")+ xlab("Week days") +
            theme_light() + theme(legend.title = element_text(size = 12, face = "bold"),
                                                                                                                                                                       legend.text=element_text(size=11),
                                                                                                                                                                       text = element_text(size = 14, face = "bold"))
          
          plot
          }) ##### output polot 2
        
        ##### creating the UI distribution
        tagList(
          hr(),
          h2(textOutput(plotname_text2)),
          p("First, let's observe the chart with the average, the min, and the max"),
          hr(),
          plotOutput(plotname_plot2),
          p("Te labels on top of the min/max bars are the date where this number of downloads happened, the format of the date is in dd/mm/yy. In the following data frame we can see the numbers from the graph"),
          hr(),
          tableOutput(plotname_table2)
          
        )

        
      })
    })
    
    
    
  }) #### observe event
}


shinyApp(ui = ui, server = server)
