library(Rbeast)

##source("code/03NDVI_TT_and_S2.R")
#TT data ----------------------------------------------------------------
TT_data_sa1 <- read_csv("data/TT_sa1_2022_median.csv")

# Function to calculate NDVI
ndvi_f <- function(data) {
  # NDVI = (NIR - Red) / (NIR + Red)
  data$NDVI <- (data$DN_860 - data$DN_650) / (data$DN_860 + data$DN_650)
  return(data)
}

TT_NDVI_sa1 <- ndvi_f(TT_data_sa1)

TT_NDVI_sa1 <- TT_NDVI_sa1[TT_NDVI_sa1$NDVI>= 0, ] %>% na.omit()

##### - Plot -----------------------------------------------------------------
plot(TT_NDVI_sa1$date, TT_NDVI_sa1$NDVI,type='p')
points(lowess(TT_NDVI_sa1$date, TT_NDVI_sa1$NDVI, f = 0.1),
       col = "#32CD32", pch = 20, cex = 0.7)


## Change detection
ss=beast.irreg(TT_NDVI_sa1$NDVI,time=TT_NDVI_sa1$date,
               deltat=' 1 day', season='none',tseg.min=1, # 1 day = TT+ recompile daily data
               tcp.minmax   = c(0,3),  torder.minmax = c(0,1)) #Detecting change every 3 days

plot(ss, vars=c( "t",'tslp'), col=c("#00CD00",'#6C7B8B'),
     main        = "NDVI Changepoint detection 2022",
     xlab        = 'Time',
     ylab        = NULL,
)
##### Saving Plot
sa1beast <- 'plot(ss, vars=c( "t","tslp"), col=c("#00CD00","#6C7B8B"),
main        = NULL,
xlab        = NULL,
ylab        = NULL,
)'
jpeg("figures/sa1beast.jpg", width = 6, height = 3, units = "in", res = 500)
eval(parse(text = sa1beast))
dev.off()
##### List of probable trend changepoints --------------------------------------------------------
print(ss)

##|tcp#              |time (cp)                  |prob(cpPr)          |
#|------------------|---------------------------|--------------------|
#  |1                 |2022.786377                |0.93638             | 93.6 %
#  |2                 |2022.356201                |0.74779             | 74.6 %
#  |3                 |2022.745239                |0.05100             |
#  .-------------------------------------------------------------------.


### Converting date to DOY format
change1 <- as.Date(0.356201 * 365, origin = "2022-01-01")
format(change1, "%Y-%m-%d")                                                  #"2022-05-11"
change2 <- as.Date(0.786377 * 365, origin = "2022-01-01")
format(change2, "%Y-%m-%d")                                                  #"2022-10-15"

