#' ---
#' title: "Visualizing trajectories of Cyclones in Indian Ocean Basin"
#' author: "Sainath Adapa"
#' date: "August 13th, 2015"
#' output:
#'  html_document:
#'    theme: cerulean
#'    fig_height: 10
#'    fig_width: 16
#'    keep_md: yes
#' ---
#' 

#' Thanks to Gaston Sanchez for a very informative Rpub, [Visualizing Hurricane Trajectories](http://rpubs.com/gaston/hurricanes). 
#' I have reused most of his preprocessing code in this script.
#' Source code is at [github.com/sainathadapa](https://github.com/sainathadapa/Scripts/tree/master/plotting-cyclones).
#' 


#' # Preprocessing
#+ echo=TRUE, messages=FALSE, results='hide', warning=FALSE, include=FALSE
library('ggplot2')
library('stringr')
library('dplyr')
library('ggmap')

#+  echo=TRUE, messages=TRUE, results='markup', warning=TRUE, include=TRUE
# Loading storms data for North Indian Ocean Basin
stormsData <- read.csv(
  './Basin.NI.ibtracs_wmo.v03r08.csv',
  # url('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r08/wmo/csv/basin/Basin.NI.ibtracs_wmo.v03r08.csv'),
  skip = 1, stringsAsFactors = FALSE)

# remove variable information
stormsData <- stormsData[-1, ]

# formatting some columns
stormsData[] <- lapply(stormsData, str_trim)
numeric_columns <- c('Season', 'Latitude', 'Longitude', 'Wind.WMO.', 'Pres.WMO.')
stormsData[numeric_columns] <- lapply(stormsData[numeric_columns], as.numeric)
stormsData$ISO_time <- as.POSIXct(stormsData$ISO_time, tz = 'UTC')

# Structure of the data.frame
stormsData %>% str


#' # Cyclones of Bay of Bengal
BayOfBengalMap <- get_googlemap(center = c(lon = 87.5, lat = 15), zoom = 5, 
                                size = c(640, 640), scale = 2, maptype = c("terrain"), 
                                color = "color")

# select storms from Bay of Bengal
substorms <- subset(stormsData, Sub_basin %in% 'BB')

p <- ggmap(BayOfBengalMap) + 
  geom_path(data = stormsData %>% filter(Serial_Num %in% Serial_Num[1]),
            aes(x = Longitude,
                y = Latitude,
                group = Serial_Num,
                color = Wind.WMO.,
                frame = ISO_time,
                cumulative = TRUE),
            linejoin = "round",
            alpha = 0.5,
            size = 0.8) + 
  geom_point(data = stormsData %>% filter(Serial_Num %in% Serial_Num[1]),
             aes(x = Longitude,
                 y = Latitude,
                 group = Serial_Num,
                 size = Wind.WMO.,
                 color = Wind.WMO.,
                 frame = ISO_time
             ),
             alpha = 0.2
  )



library(gganimate)

gg_animate(p)

# TODO: splines
# TODO: talk about the gg layers, thank hadley and others
# TODO: animate everything wrt 0th hour
# TODO: read about how to measure intensity of cyclone
# TODO: make a note that the point size is not representative of the area that got affected
