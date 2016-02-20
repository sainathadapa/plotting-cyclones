library('ggplot2')
library('stringr')
library('dplyr')
library('ggmap')
library('gganimate')

cyclones <- read.csv(
  './Basin.NI.ibtracs_wmo.v03r08.csv',
  # url('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r08/wmo/csv/basin/Basin.NI.ibtracs_wmo.v03r08.csv'),
  skip = 1, stringsAsFactors = FALSE)

cyclones <- cyclones[-1, ]
cyclones[] <- lapply(cyclones, str_trim)
numeric_columns <- c('Season', 'Latitude', 'Longitude', 'Wind.WMO.', 'Pres.WMO.')
cyclones[numeric_columns] <- lapply(cyclones[numeric_columns], as.numeric)
cyclones$ISO_time <- as.POSIXct(cyclones$ISO_time, tz = 'UTC') # not sure about the time zone

google_map <- get_googlemap(center = c(lon = 88.25, lat = 18.9), zoom = 5, 
                                size = c(640, 640), scale = 2, maptype = c("terrain"), 
                                color = "color")

p <- ggmap(google_map) + 
  geom_path(data = cyclones %>% filter(Serial_Num %in% '2014279N11096'),
            aes(x = Longitude, y = Latitude, frame = ISO_time, cumulative = TRUE)) + 
  geom_point(data = cyclones %>% filter(Serial_Num %in% '2014279N11096'),
             aes(x = Longitude, y = Latitude, frame = ISO_time, color = Wind.WMO.)) +
  scale_color_continuous(low = 'green',high = 'red')

p
gg_animate(p)

east_coast <- map_data('world',region = 'India') %>%
  filter(is.na(subregion)) %>% 
  select(long, lat, order) %>% 
  filter(lat < 25, long > 77, long < 90) %>%
  do((function(x) {
    max_lat <- max(x$lat)
    
    extra_row_1 <- data.frame(long = tail(x$long,n=1), lat = max_lat + 1, order = 100000)
    extra_row_2 <- data.frame(long = x$long[1], lat = max_lat + 1, order = 0)
    
    list(x, extra_row_1, extra_row_2) %>% 
      do.call(what = bind_rows, args = .) %>% 
      arrange(order) -> new_x
    
    new_x
  })(.))
  

ggplot() + geom_polygon(data = east_coast,aes(x=long,y=lat)) + coord_cartesian(xlim = c(77,90), ylim = c(8.75,24.5))
