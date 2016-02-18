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
#' His post gave me the idea of doing a similar thing for Indian Ocean cyclones. 
#' Source code is at [github.com/sainathadapa](https://github.com/sainathadapa/Scripts/tree/master/plotting-cyclones).
#' 


#' # Preprocessing
#+ echo=TRUE, messages=FALSE, results='hide', warning=FALSE, include=FALSE
library('ggplot2')
library('stringr')
library('dplyr')
library('ggmap')
library('gganimate')

#+  echo=TRUE, messages=TRUE, results='markup', warning=TRUE, include=TRUE
#' Loading storms data for North Indian Ocean Basin
cyclones <- read.csv(
  './Basin.NI.ibtracs_wmo.v03r08.csv',
  # url('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r08/wmo/csv/basin/Basin.NI.ibtracs_wmo.v03r08.csv'),
  skip = 1, stringsAsFactors = FALSE)

#' remove the unnecessary row
cyclones <- cyclones[-1, ]

#' Trim the extra space at the beginning and end of the strings
cyclones[] <- lapply(cyclones, str_trim)

#' convert the columns with numerical data
numeric_columns <- c('Season', 'Latitude', 'Longitude', 'Wind.WMO.', 'Pres.WMO.')
cyclones[numeric_columns] <- lapply(cyclones[numeric_columns], as.numeric)

#' parse the dates
cyclones$ISO_time <- as.POSIXct(cyclones$ISO_time, tz = 'UTC') # not sure about the time zone

#' Structure of the data
cyclones %>% str

#' Path of the recent [Hudhud cyclone](https://en.wikipedia.org/wiki/Cyclone_Hudhud) which devastated visakhapatnam
BayOfBengalMap <- get_googlemap(center = c(lon = 88.25, lat = 18.9), zoom = 5, 
                                size = c(640, 640), scale = 2, maptype = c("terrain"), 
                                color = "color")

p <- ggmap(BayOfBengalMap) + 
  geom_path(data = cyclones %>% filter(Serial_Num %in% '2014279N11096'),
            aes(x = Longitude, y = Latitude, frame = ISO_time, cumulative = TRUE)) + 
  geom_point(data = cyclones %>% filter(Serial_Num %in% '2014279N11096'),
             aes(x = Longitude, y = Latitude, frame = ISO_time, color = Wind.WMO.)) +
  scale_color_continuous(low = 'green',high = 'red')

p
# gg_animate(p, saver = 'gif', filename = 'hudhud.gif', ani.width = 1280, ani.height = 1280)

