library('magrittr')
library('dplyr')
library('ggplot2')
library('sp')
library('ggmap')

cyclones <- readRDS('1-cyclones-data-parsed.rds')

# Loading map data -------------------------------------------------------
bay_of_bengal_cyclones <- cyclones %>%
  filter(Sub_basin %in% 'BB')

mainland_india <- map_data('world',region = 'India') %>%
  filter(is.na(subregion)) %>%
  select(long, lat, order)
bangladesh <- map_data('world', region = 'Bangladesh') %>%
  filter(is.na(subregion)) %>%
  select(long, lat, order)
myanmar <- map_data('world', region = 'Myanmar') %>%
  filter(is.na(subregion)) %>%
  select(long, lat, order)

# Creating spacial polygons of the coasts for later use ------------------
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

# For each cyclone, finding the landfall point ---------------------------
landfall_points <- lapply(unique(bay_of_bengal_cyclones$Serial_Num), function(one_cyclone) {
  
  this_spline_data <- splines_data[[one_cyclone]]
  
  
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

saveRDS(landfall_points, file = '4-landfall-points-data.rds')

# Plotting the landfall locations ----------------------------------------
google_map <- get_googlemap(center = c(lon = landfall_points$x %>% range %>% sum %>% divide_by(2),
                                       lat = landfall_points$y %>% range %>% sum %>% divide_by(2)),
                            zoom   = 5,
                            size   = c(640, 640),
                            scale = 2,
                            maptype = "terrain", 
                            color  = "color")

png(filename = 'landfall-locations-small.png', width = 700, height = 700, type = 'cairo-png')
ggmap(google_map, darken = 0.25) + 
  geom_point(data = landfall_points, aes(x = x, y = y), color = 'red') +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

png(filename = 'landfall-locations-large.png', width = 1400, height = 1400, type = 'cairo-png')
ggmap(google_map, darken = 0.25) + 
  geom_point(data = landfall_points, aes(x = x, y = y), color = 'red') +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
dev.off()

# Plotting the landfall locations with max wind speed --------------------
cyclones %>% 
  group_by(Serial_Num) %>% 
  summarise(max_wind = max(Wind.WMO.), min_pres = min(Pres.WMO.)) %>% 
  ungroup -> cyclones_wind_pres_data

inner_join(cyclones_wind_pres_data, landfall_points) -> to_plot

png(filename = 'cyclone-intensity-on-map-small.png', width = 700, height = 600, type = 'cairo-png')
ggmap(google_map, darken = 0.25) + 
  geom_point(data = to_plot, aes(x = x, y = y, size = max_wind), color = 'red', alpha = 0.25) +
  theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_size_continuous(limits = c(0, 150), name = 'Max Wind (kt)')
dev.off()

png(filename = 'cyclone-intensity-on-map-large.png', width = 1000, height = 1000, type = 'cairo-png')
ggmap(google_map, darken = 0.25) + 
  geom_point(data = to_plot, aes(x = x, y = y, size = max_wind), color = 'red', alpha = 0.25) +
  theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_size_continuous(limits = c(0, 150), name = 'Max Wind (kt)')
dev.off()