# world map
wm = map_data("world")

map1 = ggplot(cyclones, aes(x = Longitude, y = Latitude, group = ID)) + 
geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
fill = "gray25", colour = "gray10", size = 0.2) + geom_path(data = substorms, 
aes(group = ID, colour = Wind.WMO.), alpha = 0.5, size = 0.8) + xlim(-138, 
-20) + ylim(3, 55) + labs(x = "", y = "", colour = "Wind \n(knots)") + opts(panel.background = theme_rect(fill = "gray10", 
colour = "gray30"), title = "Hurricane Trajectories 1999 - 2010", axis.text.x = theme_blank(), 
axis.text.y = theme_blank(), axis.ticks = theme_blank(), panel.grid.major = theme_blank(), 
panel.grid.minor = theme_blank())

# show me the map
map1

ggplot() + geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "gray25", colour = "gray10", size = 0.2) + 
  coord_cartesian(xlim = c(75,95), ylim = c(5,25)) + 
  geom_path(data = cyclones %>% filter(Serial_Num %in% '2014279N11096'),
            aes(x = Longitude, y = Latitude, frame = ISO_time, cumulative = TRUE)) + 
  geom_point(data = cyclones %>% filter(Serial_Num %in% '2014279N11096'),
             aes(x = Longitude, y = Latitude, frame = ISO_time, color = Wind.WMO.)) +
  scale_color_continuous(low = 'green',high = 'red') + theme_bw()
  
ggplot() + geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "gray25", colour = "gray10", size = 0.2) + 
  coord_cartesian(xlim = c(75,95), ylim = c(5,25)) + 
  geom_path(data = cyclones %>% filter(Season %in% 2013),
            aes(x = Longitude, y = Latitude, frame = ISO_time, cumulative = TRUE, group = Serial_Num)) + 
  geom_point(data = cyclones %>% filter(Season %in% 2013),
             aes(x = Longitude, y = Latitude, frame = ISO_time, color = Wind.WMO., group  = Serial_Num)) +
  scale_color_continuous(low = 'green',high = 'red') +
  theme_bw()


p <- ggplot() + geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "gray25", colour = "gray10", size = 0.2) + 
  coord_cartesian(xlim = c(75,95), ylim = c(5,25)) + 
  geom_path(data = cyclones,
            aes(x = Longitude, y = Latitude, frame = Season, group = Serial_Num)) + 
  geom_point(data = cyclones,
             aes(x = Longitude, y = Latitude, frame = Season, color = Wind.WMO., group  = Serial_Num)) +
  scale_color_continuous(low = 'green',high = 'red') +
  theme_bw()

gg_animate(p)
