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
  geom_polygon(data = mainland_india, aes(x = long, y = lat), fill = gray(0.3)) + 
  geom_polygon(data = bangladesh, aes(x = long, y = lat), fill = gray(0.5)) + 
  geom_polygon(data = myanmar, aes(x = long, y = lat), fill = gray(0.7))


mainland_india_sp <- mainland_india[,c('long', 'lat')] %>% 
  as.matrix %>% 
  Polygon %>% 
  list(.) %>% 
  Polygons(., 'a') %>% 
  list(.) %>% 
  SpatialPolygons()

bangladesh_sp <- bangladesh[,c('long', 'lat')] %>% 
  as.matrix %>% 
  Polygon %>% 
  list(.) %>% 
  Polygons(., 'a') %>% 
  list(.) %>% 
  SpatialPolygons()

myanmar_sp <- myanmar[,c('long', 'lat')] %>% 
  as.matrix %>% 
  Polygon %>% 
  list(.) %>% 
  Polygons(., 'a') %>% 
  list(.) %>% 
  SpatialPolygons()


landfall_points <- lapply(unique(bay_of_bengal_cyclones$Serial_Num), function(one_cyclone) {
  
  # cat(1)
  
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
  if (all(!on_land)) return(data.frame(Serial_Num = one_cyclone, x = NA_real_, y = NA_real_))
  
  if (on_land[1]) on_land[1] <- FALSE
  
  after_landfall_point <- which(on_land)[1]
  before_landfall_point <- after_landfall_point - 1
  
  which_country_landfall <- which(c(crossed_into_india[after_landfall_point],
                                    crossed_into_bangladesh[after_landfall_point],
                                    crossed_into_myanmar[after_landfall_point]) == 1)
  
  country_sp <- list(mainland_india_sp, bangladesh_sp, myanmar_sp)[[which_country_landfall]]
  
  landfall_line <- data.frame( x = c(this_spline_data$x[before_landfall_point], this_spline_data$x[after_landfall_point]),
                               y = c(this_spline_data$y[before_landfall_point], this_spline_data$y[after_landfall_point]))
  
  landfall_line_sp <- landfall_line %>%
    as.matrix %>% 
    Line(.) %>% 
    Lines(., ID = 'landfall') %>% 
    list(.) %>% 
    SpatialLines()
  
  possible_landfall_points <- rgeos::gIntersection(country_sp, landfall_line_sp)
  
  if (is.null(possible_landfall_points)) browser()
  
  possible_landfall_points <- possible_landfall_points@lines[[1]]@Lines[[1]]@coords %>% as.data.frame
  
  ans <- setdiff(possible_landfall_points %>% mutate(x = round(x, digits = 5), y = round(y, digits = 5)),
                 landfall_line %>% mutate(x = round(x, digits = 5), y = round(y, digits = 5))) %>% 
    mutate(Serial_Num = one_cyclone)
  
  if (nrow(ans) != 1) browser()
  
  ans  
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
