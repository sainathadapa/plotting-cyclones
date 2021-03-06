library('magrittr')
library('dplyr')
library('ggplot2')
library('segmented')

east_coast_fit <- readRDS('5-east-coast-segmented-model.rds')

landfall_points <- readRDS('4-landfall-points-data.rds')

cyclones <- readRDS('1-cyclones-data-parsed.rds')

cyclones %>% 
  group_by(Serial_Num) %>% 
  summarise(max_wind = max(Wind.WMO.), min_pres = min(Pres.WMO.)) %>% 
  ungroup -> cyclones_wind_pres_data

east_coast_fit_breakpoints <- data.frame(x = c(77.517578125, 79.5587651654547, 80.4305507380629, 88.0847754421326, 89.0558624267578))
east_coast_fit_breakpoints$y <- as.numeric(predict(east_coast_fit, east_coast_fit_breakpoints))

landfall_points %>% 
  filter(x > min(east_coast_fit_breakpoints$x), x < max(east_coast_fit_breakpoints$x)) -> 
  landfall_points

landfall_points$fit_y <- as.numeric(predict(east_coast_fit, data.frame(x = landfall_points$x)))

east_coast_fit_breakpoints <- east_coast_fit_breakpoints %>% mutate(dist = sqrt(((lag(y) - y) ^ 2) + ((lag(x) - x) ^ 2)))
east_coast_fit_breakpoints$dist[1] <- 0
east_coast_fit_breakpoints$cumdist <- cumsum(east_coast_fit_breakpoints$dist)

landfall_points_dist <- lapply(seq_len(nrow(landfall_points)), function(i) {
  this_point <- landfall_points[i,]
  
  breakpoint_num <- max(which(this_point$x > east_coast_fit_breakpoints$x))
  
  this_point %>%
    mutate(dist = east_coast_fit_breakpoints$cumdist[breakpoint_num] + sqrt(
      ((x - east_coast_fit_breakpoints$x[breakpoint_num]) ^ 2) +
        ((fit_y - east_coast_fit_breakpoints$y[breakpoint_num]) ^ 2)
    ))
}) %>% do.call(what = rbind, args = .)

cities <- data.frame(name = c('Chennai', 'Visakhapatnam', 'Kolkata'), x = c(80.2707, 83.2186, 88.3667))
cities$fit_y <- as.numeric(predict(east_coast_fit, cities))
cities_dist <- lapply(seq_len(nrow(cities)), function(i) {
  this_point <- cities[i,]
  
  breakpoint_num <- max(which(this_point$x > east_coast_fit_breakpoints$x))
  
  this_point %>%
    mutate(dist = east_coast_fit_breakpoints$cumdist[breakpoint_num] + sqrt(
      ((x - east_coast_fit_breakpoints$x[breakpoint_num]) ^ 2) +
        ((fit_y - east_coast_fit_breakpoints$y[breakpoint_num]) ^ 2)
    ))
}) %>% do.call(what = rbind, args = .)

state_border_points <- data.frame(x = c(80.269165,84.787292,87.481689,89.106293))
state_border_points$y <- as.numeric(predict(east_coast_fit, state_border_points))

state_dist <- lapply(seq_len(nrow(state_border_points)), function(i) {
  this_point <- state_border_points[i,]
  
  breakpoint_num <- max(which(this_point$x > east_coast_fit_breakpoints$x))
  
  this_point %>%
    mutate(dist = east_coast_fit_breakpoints$cumdist[breakpoint_num] + sqrt(
      ((x - east_coast_fit_breakpoints$x[breakpoint_num]) ^ 2) +
        ((y - east_coast_fit_breakpoints$y[breakpoint_num]) ^ 2)
    ))
}) %>% do.call(what = rbind, args = .)


data_to_plot <- landfall_points_dist %>% 
  select(Serial_Num, dist) %>% 
  inner_join(cyclones_wind_pres_data)

ggthemr::ggthemr('flat')

mid_points <- data.frame( x = (state_dist$dist + c(0, state_dist$dist[-length(state_dist$dist)]))/2, y = c('Tamil Nadu', 'Andhra Pradesh', 'Orissa', 'West Bengal'))


png(filename = 'cyclone-intensity-small.png', width = 900, height = 450, type = 'cairo-png',bg = "transparent")
ggplot(data_to_plot) +
  geom_point(aes(dist, max_wind)) +
  geom_segment(aes(x = dist, xend = dist, y = 0, yend = max_wind), alpha = 0.5) +
  coord_cartesian(xlim = c(0, max(east_coast_fit_breakpoints$cumdist)), ylim = c(0, 150), expand = FALSE) + 
  geom_vline(data = state_dist, aes(xintercept = dist), color = 'red', linetype = 'dotted') +
  geom_text(data = mid_points, aes(x=x,y=145, label = y), fontface = "bold") +
  geom_point(data = cities_dist, aes(x = dist, y = 0), color = 'red') +
  scale_x_continuous(breaks = cities_dist$dist, labels = cities_dist$name) +
  scale_y_continuous(name = 'Max Wind (kt)') + 
  # theme_light(base_size = 15) +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank())
dev.off()
  
png(filename = 'cyclone-intensity-large.png', width = 1500, height = 750, type = 'cairo-png',bg = "transparent")
ggplot(data_to_plot) +
  geom_point(aes(dist, max_wind)) +
  geom_segment(aes(x = dist, xend = dist, y = 0, yend = max_wind), alpha = 0.5) +
  coord_cartesian(xlim = c(0, max(east_coast_fit_breakpoints$cumdist)), ylim = c(0, 150), expand = FALSE) + 
  geom_vline(data = state_dist, aes(xintercept = dist), color = 'red', linetype = 'dotted') +
  geom_text(data = mid_points, aes(x=x,y=145, label = y), fontface = "bold") +
  geom_point(data = cities_dist, aes(x = dist, y = 0), color = 'red') +
  scale_x_continuous(breaks = cities_dist$dist, labels = cities_dist$name) +
  scale_y_continuous(name = 'Max Wind (kt)') + 
  # theme_light(base_size = 15) +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank())
dev.off()

