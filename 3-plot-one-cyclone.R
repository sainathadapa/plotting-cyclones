library('magrittr')
library('dplyr')
library('ggmap')
library('viridis')

cyclones <- readRDS('1-cyclones-data-parsed.rds')
splines_data <- readRDS('2-splines-data.rds')

cyclones %>%
  filter(Name %in% "HUD HUD:HUDHUD") ->
  one_cyclone

one_cyclone_spline_data <- splines_data[[one_cyclone$Serial_Num[1]]]


google_map <- get_googlemap(center = c(lon = one_cyclone$Longitude %>% range %>% sum %>% divide_by(2),
                                       lat = one_cyclone$Latitude  %>% range %>% sum %>% divide_by(2)),
                            zoom   = 5,
                            size   = c(640, 640),
                            scale = 2,
                            maptype = "terrain", 
                            color  = "color")

ggmap(google_map, darken = 0.15) + 
  geom_path(data = one_cyclone_spline_data, aes(x = x, y = y), linetype = 'twodash', alpha = 1) + 
  geom_point(data = one_cyclone, aes(x = Longitude, y = Latitude, color = Pres.WMO., size = Wind.WMO.)) +
  scale_color_viridis(name = 'Pressure (mb)', option = 'C') +
  scale_size_continuous(name = 'Wind (kt)')

gganimate::gg_animate(ggmap(google_map, darken = 0.15) + 
  geom_path(data = one_cyclone_spline_data, aes(x = x, y = y), linetype = 'twodash', alpha = 1) + 
  geom_point(data = one_cyclone, aes(x = Longitude, y = Latitude, color = Pres.WMO., size = Wind.WMO., frame = ISO_time)) +
  scale_color_viridis(name = 'Pressure (mb)', option = 'C') +
  scale_size_continuous(name = 'Wind (kt)'),
  interval = 0.25)

