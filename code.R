library(ggplot2)
library(dplyr)
library(lubridate)

data = read.csv("https://raw.githubusercontent.com/theshahzaibc/Covid-19-Vaccines-Analysis-with-R/main/pkcovid.csv")
data$Date <- as.Date(data$Date, format="%m/%d/%Y", tz="UTC")
head(data)

str(data)

total <- aggregate((Cases ~ Date),data,sum)
plot(total$Date, total$Cases, type = "o", col = "BLUE", main = "Total Cases", xlab = "Date", ylab = "Cases")


lbls <- paste0(month.abb[month(data$Date)], " ", lubridate::year(data$Date))
brks <- data$Date
# plot
ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=Cases)) + 
  labs(title="Monthly Cases Analytics", 
       y="Cases") +
  ylim(0, 500) + 
  scale_x_date(labels = lbls, 
               breaks = brks) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        panel.grid.minor = element_blank())


## cases and recovered

plot_covid <- function(sort_, type_){
  if (type_ == "Cases"){
    ggplot(data, aes(x=Date, y=Cases, group = sort_, colour = sort_)) +
      geom_line() +
      labs(y= "Cases", x = "Date") +
      ggtitle("Daily COVID19 Cases in Pakistan") +
      geom_point()
  }
  else if (type_ == "Deaths"){
    ggplot(data, aes(x=Date, y=Deaths, group = sort_, colour = sort_)) +
      geom_line() +
      labs(y= "Deaths", x = "Date") +
      ggtitle("Daily COVID19 Deaths in Pakistan") +
      geom_point()
  }
  else if (type_ == "Recovered"){
    ggplot(data, aes(x=Date, y=Recovered, group = sort_, colour = sort_)) +
      geom_line() +
      labs(y= "Recovered", x = "Date") +
      ggtitle("Daily COVID19 Recovered in Pakistan") +
      geom_point()
  }
}

plot_covid(sort_ = data$City, type_ = "Cases")