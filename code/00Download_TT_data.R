#Install packages
#devtools::install_github("https://github.com/jpkabala96/ttprocessing")
library(ttprocessing)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Presetting
options(timeout = max(1000, getOption("timeout"))) #time out

##### Read data from TTCloud
tt_data <- readTTData("http://www.ittn.altervista.org/CXXXXXX/ttcloud.txt") #Link server or TT-Cloud (CXXXXXX)
                      #https://naturetalkers.altervista.org/CXXXXXX/ttcloud.txt

#convert the string 49
tt_s49<- string49Handling(tt_data) # s49 = TT+ spectrometer data

# Filter by hour and date
tt_data_9am <- tt_s49%>%
  filter(hour == 9) %>%#For example data only for 9am
  filter(between(date, as.Date('2022-01-01'), as.Date('2022-12-31'))) %>% drop_na()

#Plot individual bands
plot <- ggplot(tt_data_9am, aes(x = date)) +
  geom_point(aes(y = DN_810, color = "DN_810")) +
  geom_point(aes(y = DN_500, color = "DN_500")) +
  geom_line(aes(y = DN_550, color = "DN_550")) +
  labs(x = "Date", y = "Value", color = "DN_") +
  scale_color_manual(values = c(DN_810 = "red", DN_500 = "blue", DN_550 = "green")) +
  theme_minimal()

plot
