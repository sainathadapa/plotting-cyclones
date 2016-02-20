library('magrittr')
library('dplyr')
library('ggplot2')
library('sp')
library('ggmap')

cyclones <- readRDS('1-cyclones-data-parsed.rds')
splines_data <- readRDS('2-splines-data.rds')

mainland_india <- map_data('world',region = 'India') %>% filter(is.na(subregion)) %>% select(long, lat, order)
bangladesh <- map_data('world', region = 'Bangladesh') %>% filter(is.na(subregion)) %>% select(long, lat, order)
myanmar <- map_data('world', region = 'Myanmar') %>% filter(is.na(subregion)) %>% select(long, lat, order)

bay_of_bengal_cyclones <- cyclones %>% filter(Sub_basin %in% 'BB')

base_plot <- ggplot() +
  geom_polygon(data = mainland_india, aes(x = long, y = lat)) + 
  geom_polygon(data = bangladesh, aes(x = long, y = lat)) + 
  geom_polygon(data = myanmar, aes(x = long, y = lat))


landfall_points <- lapply(unique(bay_of_bengal_cyclones$Serial_Num), function(one_cyclone) {
  
  this_spline_data <- splines_data[[one_cyclone]]
  
  # base_plot + geom_path(data = this_spline_data %>% mutate(slno = 1:nrow(this_spline_data)), aes(x=x,y=y,color=slno))
  # base_plot + geom_path(data = bay_of_bengal_cyclones %>% filter(Serial_Num %in% one_cyclone), aes(x=Longitude,y=Latitude))
  
  crossed_into_india <- Map(f = function(x, y) point.in.polygon(x, y, mainland_india$long, mainland_india$lat), 
                            x = this_spline_data$x,
                            y = this_spline_data$y) %>% as.numeric
  
  crossed_into_bangladesh <- Map(f = function(x, y) point.in.polygon(x, y, bangladesh$long, bangladesh$lat), 
                                 x = this_spline_data$x,
                                 y = this_spline_data$y) %>% as.numeric
  
  crossed_into_myanmar <- Map(f = function(x, y) point.in.polygon(x, y, myanmar$long, myanmar$lat), 
                              x = this_spline_data$x,
                              y = this_spline_data$y) %>% as.numeric
  
  on_land <- (crossed_into_india + crossed_into_bangladesh + crossed_into_myanmar) > 0
  
  if (all(on_land)) return(data.frame(Serial_Num = one_cyclone, x = NA_real_, y = NA_real_))
  
  landfall_point <- which(on_land)[1]
  
  data.frame(Serial_Num = one_cyclone,
             x = this_spline_data$x[landfall_point],
             y = this_spline_data$y[landfall_point])
  
}) %>% 
  do.call(what = rbind, args = .) %>% 
  filter(!is.na(x))

google_map <- get_googlemap(center = c(lon = landfall_points$x %>% range %>% sum %>% divide_by(2),
                                       lat = landfall_points$y %>% range %>% sum %>% divide_by(2)),
                            zoom   = 5,
                            size   = c(640, 640),
                            scale = 2,
                            maptype = "terrain", 
                            color  = "color")

ggmap(google_map, darken = 0.15) + 
  geom_point(data = landfall_points, aes(x = x, y = y), color = 'red')
