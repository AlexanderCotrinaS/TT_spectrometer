library(readr)
library(tidyverse)

#Sentinel 2 satellite data-------------------------------------------------
sentinel2 <- read_csv("data/S2_san1_22.csv")
s2_2022 <- sentinel2%>%filter(between(date, as.Date('2022-01-01'), as.Date('2022-12-01'))) # Filter by date

s2_2022_NDVI <- s2_2022 %>% mutate(NDVI.s2 = (s2_2022$B8-s2_2022$B4)/
                                                 (s2_2022$B8 + s2_2022$B4))  # NDVI calculation
#TT data ----------------------------------------------------------------
TT_data_sa1 <- read_csv("data/TT_sa1_2022_median.csv")

# Function to calculate NDVI
ndvi_f <- function(data) {
  # NDVI = (NIR - Red) / (NIR + Red)
  data$NDVI <- (data$DN_860 - data$DN_650) / (data$DN_860 + data$DN_650)
  return(data)
}

TT_NDVI_sa1 <- ndvi_f(TT_data_sa1)

#### Combining TT+ and S2 data

total_braca_ndvi21<- merge(braca21.ndvi,bracasentinel2_NDVI, by="date")
total_braca_ndvi21 <- total_braca_ndvi21 %>% mutate(Plot = "Campo Braca", Year = "2021")
write.csv(braca21.ndvi, "braca21.ndvi.csv")

