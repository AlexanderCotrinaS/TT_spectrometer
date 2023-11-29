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
       x = "Date", y = "Value")+
  theme_minimal()

# TT ID subplots----------------------------------------------------------------------------------------------------------------------------
subset1_ids <- c("52140002", "52140012", "52140014","52140011","52140016", "52140018", "52140026", "52140028")

# Generar subsets para cada grupo de ids
subset1 <- tt_data_9am %>%
  filter(id %in% subset1_ids)

ggplot(subset1, aes(x = date, y = DN_810, color = id)) +
  geom_point() +
  labs(title = "",
       x = "Date", y = "DN_810")+
  theme_minimal()


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

