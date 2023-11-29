#####
source("code/00Download_TT_data.R")

# Filtering for individual device" -------------------------------------------------------
ids.s49 <- unique(tt_data_9am$id) #ID for each device
print(ids.s49)

device1 <- c("52140022") #Device ID

device_1 <- tt_data_9am %>%
  filter(id %in% device1)

#Plot
ggplot(device_1, aes(x = date, y = DN_810)) +
  geom_point() +
  labs(title = "Individual TT+ ",
       x = "Date", y = "Value")

# TT ID subplots----------------------------------------------------------------------------------------------------------------------------
subset1_ids <- c("52140002", "52140012", "52140014","52140011","52140016", "52140018", "52140026", "52140028")

# Generar subsets para cada grupo de ids
subset1 <- tt_data_9am %>%
  filter(id %in% subset1_ids)

ggplot(subset1, aes(x = date, y = DN_810, color = id)) +
  geom_point() +
  labs(title = "",
       x = "Date", y = "DN_810")


## Median --------------------------------------------------------------------------------------------

# Function to calculate the median and add the "Plot" column name
calculate_median <- function(data) {
  medians_df <- data %>%
    group_by(date) %>%
    summarize(across(starts_with("DN_"), median, na.rm = TRUE), .groups = "drop")
  medians_df$Plot <- "Rocas Plot"
  medians_df <- na.omit(medians_df)
  return(medians_df)
}

# Calculate median
medians_subset1 <- calculate_median(subset1)

# Rearranging the dataframe for plot
median_long <- tidyr::pivot_longer(medians_subset1, cols = starts_with("DN_"), names_to = "DN", values_to = "DN_Value")

# Crear el gráfico con ggplot
ggplot(median_long, aes(x = date, y = DN_Value, color = DN)) +
  geom_line(linewidth = 0.2) +
  labs(title = "Timeseries data according DN_ value",
       x = "Date",
       y = "DN_Value") +
  theme_minimal()














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

