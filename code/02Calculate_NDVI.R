####---------------------------------------
source("code/00Download_TT_data.R")
source("code/01Filter_median.R")

#### NDVI function -----------------------------------------------------------------------------------------
ndvi_f <- function(data) {
  # NDVI = (NIR - Red) / (NIR + Red)
  data$NDVI <- (data$DN_860 - data$DN_650) / (data$DN_860 + data$DN_650)
  return(data)
}

# execute function of NDVI
medians_subset1 <- ndvi_f(medians_subset1)

# Filtering NDVI values >= 0
medians_subset1 <- medians_subset1[medians_subset1$NDVI>= 0, ] %>% na.omit()

# Plot NDVI
ggplot(data = medians_subset1, aes(x = date, y = NDVI)) +
  geom_point(color = "#32CD32") +
  geom_smooth(method = "loess", se = FALSE, span = 0.1, color = "blue") + #value span 0.1
  labs(x = "Date", y = "NDVI") +
  ggtitle("NDVI - TT+")


#### Smoothing plot ------------------------------

plot(medians_subset1$date, medians_subset1$DN_550, type = "l", col = "white",                             # Regular X-Y plot in R
     main = "NDVI - TT+", cex.main = 1,
     lwd = 1,
     xlab = "Date",
     ylab = "NDVI",
     ylim = c(-0.1, 1))

points(lowess(medians_subset1$date, medians_subset1$NDVI, f = 0.1),
      col = "#32CD32", pch = 20, cex = 0.7)

legend("top",                                                   # Add legend to plot
       col = c("#32CD32"),lwd = 0,lty = 0,pch = 19,
       c("Rocas - NDVI"),
       cex = 0.8)

## Save Plot
ggsave("NDVI_TT+.png", width = 15, height = 7, units = c("cm"), dpi = 500)

#Export data----------------------------
#write.csv(medians_subset1, "Data_and_NDVI.csv", row.names = FALSE)

