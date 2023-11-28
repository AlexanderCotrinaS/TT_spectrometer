#Install packages
#devtools::install_github("https://github.com/jpkabala96/ttprocessing")
library(ttprocessing)
library(dplyr)

#Presetting
options(timeout = max(1000, getOption("timeout"))) #time out

##### Read data from TTCloud
tt_data <- readTTData("http://www.ittn.altervista.org/C0200122/ttcloud.txt") #Link server or TT-Cloud (CXXXXXX)

#convert the string 49
tt_s49<- string49Handling(tt_data) # s49 = TT+ spectrometer data

# Filter by hour
tt_data_9am <- tt_s49%>%
  filter(hour == 9) #For example data only for 9am

# Obtener el año de la columna "date"
filtered_roca_uk <- roca_9am %>%
  mutate(year = lubridate::year(date))

# TT ID subplots----------------------------------------------------------------------------------------------------------------------------
subset1_ids <- c("52140002", "52140012", "52140014","52140011","52140016", "52140018", "52140026", "52140028")
subset2_ids <- c("52140007", "51240015", "R51240017","52140021","52140022", "52140024", "52140001", "52140023")
subset3_ids <- c("52140004", "52140005", "52140006","52140008", "52140009", "52140019", "52140025","52140027", "52140029", "52140030")

# Generar subsets para cada grupo de ids
subset1 <- filtered_roca_uk %>%
  filter(id %in% subset1_ids)

subset2 <- filtered_roca_uk %>%
  filter(id %in% subset2_ids)

subset3 <- filtered_roca_uk %>%
  filter(id %in% subset3_ids)

# Función para calcular la mediana y agregar la columna "Plot"
calculate_median <- function(data) {
  medians_df <- data %>%
    group_by(date) %>%
    summarize(across(starts_with("DN_"), median, na.rm = TRUE), .groups = "drop")
  medians_df$Plot <- "Rocas"
  medians_df <- na.omit(medians_df)
  return(medians_df)
}

# Calcular medianas y agregar columna "Plot" para cada subset
medians_subset1 <- calculate_median(subset1)
medians_subset2 <- calculate_median(subset2)
medians_subset3 <- calculate_median(subset3)
medians_total_roca_uk <- calculate_median(filtered_roca_uk)

calcular_ndvi <- function(data) {
  # NDVI = (NIR - Red) / (NIR + Red)
  data$ndvi <- (data$DN_860 - data$DN_650) / (data$DN_860 + data$DN_650)
  return(data)
}

# ejecutar function of NDVI
medians_subset1 <- calcular_ndvi(medians_subset1)
medians_subset2 <- calcular_ndvi(medians_subset2)
medians_subset3 <- calcular_ndvi(medians_subset3)
total_roca_uk_ndvi <- calcular_ndvi(filtered_roca_uk)

# Filtrar los valores de ndvi mayores o iguales a 0
medians_subset1 <- medians_subset1[medians_subset1$ndvi >= 0, ]
medians_subset2 <- medians_subset2[medians_subset2$ndvi >= 0, ]
medians_subset3 <- medians_subset3[medians_subset3$ndvi >= 0, ]

# Total Rocas without NAs
total_roca_uk_ndvi <- total_roca_uk_ndvi[total_roca_uk_ndvi$ndvi >=0,]
total_roca_uk_ndvi <- na.omit(total_roca_uk_ndvi)

# Crear un gráfico de puntos con ggplot2
ggplot(data = total_roca_uk_ndvi, aes(x = date, y = ndvi)) +
  geom_point() +
  labs(x = "Fecha", y = "NDVI") +
  ggtitle("Gráfico de NDVI")


# Fusionar los subsets---------------------------------------------------------------------------------------------------
rocas <- bind_rows(medians_subset1, medians_subset2, medians_subset3)
rocas$Specie <- "Quercus cerris"

# Plot de serie de tiempo
plot <- ggplot(merged_df, aes(x = date)) +
  geom_line(aes(y = DN_810, color = "DN_810")) +
  geom_line(aes(y = DN_500, color = "DN_500")) +
  geom_line(aes(y = DN_550, color = "DN_550")) +
  labs(x = "Fecha", y = "Valor DN_", color = "DN_") +
  scale_color_manual(values = c(DN_810 = "red", DN_500 = "blue", DN_550 = "green")) +
  theme_minimal()

plot

#Export data----------------------------
write.csv(total_roca_uk_ndvi, "Roca_total.csv", row.names = FALSE)












