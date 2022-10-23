library(ggplot2)
library(dplyr)
library(reshape2)
library(shiny)

#### load the data
Sys.setlocale("LC_TIME", "C")
data <- read.csv("data_task_oct22.csv", header = TRUE , sep = ",")


#### First Task

### set date as date 
data$date <- as.Date(data$date)


#### creating the time series plot
plot <- ggplot(data) + geom_line(aes(x = date, y = count,color = package)) + facet_wrap( ~ package) + scale_x_date(date_breaks = "1 month", date_labels =  "%b")
plot


#### Second task

#### subset with package shiny and remove NA
shiny  <- subset(data, data$package == "shiny")
shiny <- na.omit(shiny)

### formatting and creating a dataframe with all the statistics we wanna know
shiny$week_day <- weekdays(shiny$date)
str(shiny)
min <- NULL
max <- NULL
average <- NULL
date.average <- NULL
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
  average[i] <- mean(shiny_day$count)
  
}

#### generating the data frames
shiny_output <- data.frame(average,min,max,days, row.names = days)
shiny_output <- melt(shiny_output, id.vars="days")

####### creating labels for the plot
dates <- data.frame(date.average,date.min, date.max)
dates$date.min <- format(as.Date(dates$date.min), "%d/%m")
dates$date.max <- format(as.Date(dates$date.max), "%d/%m")
dates <- unlist(dates)
shiny_output$dates <- dates

### Generating the plot
plot <- ggplot(shiny_output, aes(x = factor(days, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),y = value, col = variable, fill = variable)) + geom_bar(stat="identity", position = "dodge", color = "black") + 
  scale_fill_manual( values = c("#619CFF", "#F8766D", "#00BA38")) +
  geom_text(aes(label = dates), vjust= -0.5, position = position_dodge(0.9), fontface= "bold", color = "black")+ xlab("Week days")

plot




#### Want to run the shiny app?

runGitHub(repo = "Task_MDN", username = "Sanpepe92", ref = "main")