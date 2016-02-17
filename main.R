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

library(ggalt)







# TODO: animate everything wrt 0th hour
# TODO: read about how to measure intensity of cyclone
# TODO: make a note that the point size is not representative of the area that got affected
# TODO: also more plots (line/bar) are possible showing the intensity (wind of pressure) of cyclones vs time or state or place of contact
# TODO: talk about the gg layers, thank hadley and others

# TODO: splines ----------------------------------------
# - http://stackoverflow.com/questions/11356997/interpolating-a-path-curve-within-r
# - http://stackoverflow.com/questions/7729058/how-can-i-get-a-cubic-bezier-curve-closest-to-given-points
# - https://www.particleincell.com/2012/bezier-splines/
# - https://www.wikiwand.com/en/B%C3%A9zier_curve
# - http://stackoverflow.com/questions/31554390/how-to-smooth-curves-line-graph-in-ggplot

ggmap(BayOfBengalMap) + 
  # geom_path(data = stormsData %>% filter(Serial_Num %in% Serial_Num[1]),
  #           aes(x = Longitude,
  #               y = Latitude,
  #               group = Serial_Num),
  #           linetype="dashed",
  #           alpha = 0.5,
  #           size = 0.8) +
  geom_xspline(data = stormsData %>% filter(Serial_Num %in% Serial_Num[1]),
               aes(x = Longitude,
                   y = Latitude,
                   group = Serial_Num,
                   color = Wind.WMO.),
               spline_shape=1, size=0.5)


# TODO: better map -----------------------------------------
world <- map_data("world")
#> 
#>  # ATTENTION: maps v3.0 has an updated 'world' map.        #
#>  # Many country borders and names have changed since 1990. #
#>  # Type '?world' or 'news(package="maps")'. See README_v3. #
world <- world[world$region != "Antarctica",]
library(maps)
# World in Winkel-Tripel
world <- map_data("world")
world <- world[world$region != "Antarctica",]

gg <- ggplot()
gg <- gg + geom_map(data=world, map=world,
                    aes(x=long, y=lat, map_id=region))
gg <- gg + coord_proj("+proj=wintri")
gg

# U.S.A. Albers-style
usa <- world[world$region == "India",] %>% filter(long > 75 & lat < 25)

gg <- ggplot()
gg <- gg + geom_map(data=usa, map=usa,
                    aes(x=long, y=lat, map_id=region), color = 'red', fill = 'white')
gg <- gg + coord_proj(
  paste0("+proj=merc  +lon_0=80"))
gg

# Showcase Greenland (properly)
greenland <- world[world$region == "Greenland",]

gg <- ggplot()
gg <- gg + geom_map(data=greenland, map=greenland,
                    aes(x=long, y=lat, map_id=region))
gg <- gg + coord_proj(
  paste0("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0",
         " +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
gg