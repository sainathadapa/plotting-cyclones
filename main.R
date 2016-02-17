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
library('ggplot2'); library('stringr');
library('dplyr'); library('ggmap')

#+  echo=TRUE, messages=TRUE, results='markup', warning=TRUE, include=TRUE
# Loading storms data for North Indian Ocean Basin
stormsData <- read.csv(
  url('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r08/wmo/csv/basin/Basin.NI.ibtracs_wmo.v03r08.csv'),
  skip = 1, stringsAsFactors = FALSE)

# remove variable information
stormsData <- stormsData[-1, ]

# formatting some columns
stormsData[] <- lapply(stormsData, str_trim)
numeric_columns <- c('Season', 'Latitude', 'Longitude', 'Wind.WMO.', 'Pres.WMO.')
stormsData[numeric_columns] <- lapply(stormsData[numeric_columns], as.numeric)
stormsData$ISO_time <- as.POSIXct(stormsData$ISO_time, tz = 'Asia/Kolkata')

# Structure of the data.frame
stormsData %>% str

# creating an unique id for every storm
stormsData$ID <- as.factor(paste(stormsData$Season,stormsData$Num,sep = "-"))

# extracting month and converting it to a factor
stormsData %>%
  group_by(ID) %>% 
  mutate(Month = format(ISO_time[1], format = "%B")) %>% 
  ungroup %>% 
  mutate(Month = factor(Month, levels = month.name)) ->
  stormsData


#' # Cyclones of Bay of Bengal
BayOfBengalMap <- get_googlemap(center = c(lon = 87.5, lat = 15), zoom = 5, 
                                size = c(640, 640), scale = 2, maptype = c("terrain"), 
                                color = "color")

# select storms from Bay of Bengal
substorms <- subset(stormsData, Sub_basin %in% 'BB')

plot1 <- ggmap(BayOfBengalMap) + 
  geom_path(data = substorms, aes(x = Longitude, y = Latitude, group = ID, color = Wind.WMO.), linejoin = "round", alpha = 0.5, size = 0.8)

plot2 <- plot1 + facet_wrap(~Month)

suppressWarnings({print(plot1);print(plot2)})


#' # Arabian Sea Cyclones

ArabianSeaMap <- get_googlemap(center = c(lon = 65, lat = 15), zoom = 5, 
                               size = c(640, 640), scale = 2, maptype = c("terrain"), 
                               color = "color")

# select storms from Bay of Bengal
substorms <- subset(stormsData, Sub_basin %in% 'AS')

plot3 <- ggmap(ArabianSeaMap) + 
  geom_path(data = substorms, aes(x = Longitude, y = Latitude,group = ID,color = Wind.WMO.), alpha = 0.5, size = 0.8)

plot4 <- plot3 + facet_wrap(~Month)

suppressWarnings({print(plot3);print(plot4)})
