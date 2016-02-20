library('ggplot2')
library('stringr')
library('dplyr')
library('ggmap')
library('gganimate')
library('magrittr')

cyclones <- read.csv(
  './Basin.NI.ibtracs_wmo.v03r08.csv',
  # url('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r08/wmo/csv/basin/Basin.NI.ibtracs_wmo.v03r08.csv'),
  skip = 1, stringsAsFactors = FALSE)

cyclones <- cyclones[-1, ] 
cyclones[] <- lapply(cyclones, str_trim)
numeric_columns <- c('Season', 'Latitude', 'Longitude', 'Wind.WMO.', 'Pres.WMO.')
cyclones[numeric_columns] <- lapply(cyclones[numeric_columns], as.numeric)
cyclones$ISO_time <- as.POSIXct(cyclones$ISO_time, tz = 'UTC') # not sure about the time zone


mainland_india <- map_data('world',region = 'India') %>%
  filter(is.na(subregion))


cyclones$crossed_mainland <- Map(f = function(x, y) sp::point.in.polygon(x, y, mainland_india$long, mainland_india$lat), 
                                 x = cyclones$Longitude, y = cyclones$Latitude) %>% as.numeric

cyclones %>% 
  group_by(Serial_Num) %>% 
  do((function(x) {
    x$num_crossing = diff(x$crossed_mainland) %>% is_greater_than(0) %>% sum
    x
  })(.)) %>% 
  ungroup ->
  cyclones


cyclones %>% 
  filter(num_crossing > 0) %>% 
  group_by(Serial_Num) %>%
  mutate(crossing_after = c(NA_real_,diff(crossed_mainland)), crossing_before = c(diff(crossed_mainland),NA_real_)) %>% 
  filter(crossing_after %in% 1 | crossing_before %in% 1) %>% 
  do((function(x) tail(x,n=2))(.)) %>% 
  do((function(x) {
    data.frame(x1 = x$Longitude[1], x2 = x$Longitude[2], y1 = x$Latitude[1], y2 = x$Latitude[2])
  })(.)) %>% 
  ungroup -> temp

google_map <- get_googlemap(center = c(lon = 88.25, lat = 18.9), zoom = 5, 
                            size = c(640, 640), scale = 2, maptype = c("terrain"), 
                            color = "color")

ggmap(google_map) + 
  geom_segment(data = temp,
             aes(x = x1, xend = x2, y = y1, yend = y2),
             arrow = arrow(length = unit(0.1,"cm")))
