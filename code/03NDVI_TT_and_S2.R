library(readr)
library(tidyverse)

#Sentinel 2 satellite data-------------------------------------------------
sentinel2 <- read_csv("data/S2_san1_22.csv")
s2_2022 <- sentinel2%>%filter(between(date, as.Date('2022-01-01'), as.Date('2022-12-01'))) # Filter by date

s2_2022_NDVI <- s2_2022 %>% mutate(NDVI.s2 = (s2_2022$B8-s2_2022$B4)/
                                                 (s2_2022$B8 + s2_2022$B4))  # NDVI calculation NIR = B8 ; Red = B4

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
total_sa1_ndvi22 <- merge(s2_2022_NDVI,TT_NDVI_sa1, by="date")

##Delete duplicated rows
total_sa1_ndvi22 <- total_sa1_ndvi22 %>% filter(!duplicated(date))

#write.csv(total_sa1_ndvi22, "sa1_22_ndvi_fusion.csv")

#########
ggplot(total_sa1_ndvi22, aes(x = date)) +
  geom_point(aes(y = NDVI.s2, color = "NDVI_S2"), size = 2) +
  geom_point(aes(y = NDVI, color = "NDVI_TT"), size = 2) +
  labs(title = "NDVI TT+ and S2",
       x = "Date",
       y = "NDVI") +
  scale_color_manual(values = c("NDVI_S2" = "#B8860B", "NDVI_TT" = "#32CD32"))+
  theme_minimal()

