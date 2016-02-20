library('ggplot2')
library('stringr')
library('dplyr')

cyclones <- read.csv(
  './Basin.NI.ibtracs_wmo.v03r08.csv',
  # url('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r08/wmo/csv/basin/Basin.NI.ibtracs_wmo.v03r08.csv'),
  skip = 1, stringsAsFactors = FALSE)

cyclones <- cyclones[-1, ] 
cyclones[] <- lapply(cyclones, str_trim)
numeric_columns <- c('Season', 'Latitude', 'Longitude', 'Wind.WMO.', 'Pres.WMO.')
cyclones[numeric_columns] <- lapply(cyclones[numeric_columns], as.numeric)
cyclones$ISO_time <- as.POSIXct(cyclones$ISO_time, tz = 'UTC') # not sure about the time zone

cyclones %>% filter(Serial_Num %in% '2014279N11096') -> temp

ggplot(temp, aes(Longitude, Latitude)) + 
  geom_point()


df <- data.frame(A=c(2,3,4,5,6,7,3,7,8,9,2),B=c(3,7,8,9,2,1,2,3,4,5,6),C=c(1,1,1,2,2,1,1,1,1,2,2))
plot(df[,1:2])
xspline(df[,1:2], shape=-0.2, lwd=2)  # play with the shape parameter

library(bezier)
res <- bezier(seq(0, 1, len=100), df[,1:2], deg=nrow(df)-1)
points(res, type="l", col="green", lwd=2)

## Get points
ps <- data.frame(xspline(df[,1:2], shape=-0.2, lwd=2, draw=F))

## Add to your plot
go <- ggplot(df, aes(x=A, y=B, colour = factor(C)), pch = 17) +
  geom_point(size=5) +
  geom_path(data=ps, aes(x, y), col=1)


plot(temp[,c('Longitude', 'Latitude')])
xspline(temp[,c('Longitude', 'Latitude')], shape=-0.2, lwd=2)  # play with the shape parameter
ps <- data.frame(xspline(temp[,c('Longitude', 'Latitude')], shape=-0.2, lwd=2, draw=F))
ggplot(temp, aes(Longitude, Latitude)) + 
  geom_point(color = 'red') + 
  geom_point(data=ps, aes(x, y), col=1, size = 0.1)
