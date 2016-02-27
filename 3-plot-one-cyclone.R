library('magrittr')
library('dplyr')
library('ggmap')
library('viridis')
library('gganimate')

cyclones <- readRDS('1-cyclones-data-parsed.rds')
splines_data <- readRDS('2-splines-data.rds')

cyclones %>%
  filter(Name %in% "HUD HUD:HUDHUD") ->
  one_cyclone

one_cyclone_spline_data <- splines_data[[one_cyclone$Serial_Num[1]]]

longitude_center <- one_cyclone$Longitude %>% range %>% sum %>% divide_by(2)
latitude_center <- one_cyclone$Latitude %>% range %>% sum %>% divide_by(2)

google_map <- 
  get_googlemap(center = c(lon = longitude_center,
                           lat = latitude_center),
                zoom   = 5,
                size   = c(640, 640),
                scale  = 1,
                maptype = "terrain", 
                color  = "color")

gg <- ggmap(google_map, darken = 0.5) + 
  geom_path(data = one_cyclone_spline_data,
            aes(x = x, y = y),
            linetype = 'twodash',
            alpha = 1) + 
  geom_point(data = one_cyclone,
             aes(x = Longitude,
                 y = Latitude,
                 color = Pres.WMO.,
                 size = Wind.WMO.,
                 frame = ISO_time)) +
  theme_bw(base_size = 18) +
  scale_color_viridis(name = 'Pressure (mb)',
                      option = 'C') +
  scale_size_continuous(name = 'Wind (kt)') + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

png(filename = 'hudhud-static-small.png', width = 700, height = 555, type = 'cairo-png',bg = "transparent")
print(gg)
dev.off()


png(filename = 'hudhud-static-large.png', width = 1500, height = 1150, type = 'cairo-png')
print(gg)
dev.off()

gg_animate(gg, filename = 'hudhud-animat.gif', interval = 0.25, ani.width = 700, ani.height = 555)
