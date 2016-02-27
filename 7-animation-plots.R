library('magrittr')
library('dplyr')
library('ggmap')
library('lubridate')


cyclones <- readRDS('1-cyclones-data-parsed.rds')
splines_data <- readRDS('2-splines-data.rds')

base_plot <- ggplot() +
  geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group), fill = gray(0.7)) +
  theme_bw() + 
  coord_cartesian(xlim = c(77,100), ylim = c(2,25), expand = FALSE)

bay_of_bengal_cyclones <- cyclones %>% filter(Sub_basin %in% 'BB')
bay_of_bengal_cyclones %>% 
  group_by(Serial_Num) %>% 
  mutate(relative_time = ISO_time - as.numeric(floor_date(ISO_time, 'year'))) %>% 
  ungroup -> bay_of_bengal_cyclones

bay_of_bengal_cyclones$month <- as.numeric(format(bay_of_bengal_cyclones$ISO_time, format = '%m'))

bay_of_bengal_cyclones$day <- as.numeric(format(bay_of_bengal_cyclones$ISO_time, format = '%d'))

bay_of_bengal_cyclones$hour <- as.numeric(format(bay_of_bengal_cyclones$ISO_time, format = '%H'))
bay_of_bengal_cyclones$year <- as.numeric(format(bay_of_bengal_cyclones$ISO_time, format = '%Y'))

library(GGally)

for (this_day in 1:31) {
  for (this_hour in seq(from = 0, to = 21, by = 3)) {
    
    plot_list <- lapply(1:12, function(this_month) {
      base_plot +
        geom_path(data = bay_of_bengal_cyclones %>%
                    group_by(Serial_Num) %>% 
                    do((function(x) {
                      
                      threshold_time <- try(as.POSIXct(paste0(x$year[1], '-', this_month, '-', this_day, ' ', this_hour, ':00:00'), tz = 'UTC'), silent = TRUE)
                      
                      if (is(threshold_time, 'try-error')) return(x[0,] %>% mutate(alpha_var = 1))
                      
                      ans <- x %>%
                        filter(ISO_time <= threshold_time) %>% 
                        filter(max(day) %in% this_day) %>% 
                        filter(max(month) %in% this_month) %>% 
                        mutate(alpha_var = (as.numeric(ISO_time) - as.numeric(min(ISO_time)))/(as.numeric(max(ISO_time)) - as.numeric(min(ISO_time)))) %>% 
                        mutate(alpha_var = replace(alpha_var, is.nan(alpha_var), 1))
                      
                      ans
                    })(.)) %>% 
                    ungroup,
                  aes(x = Longitude, y = Latitude, group = Serial_Num, color = Serial_Num, alpha = alpha_var))
    })
    
    png(paste0(sprintf("%02d", this_day),'-',sprintf("%02d", this_hour),'.png'), width = 1920, height = 1080)
    print(ggmatrix(ncol = 4, nrow = 3, plots = plot_list))
    dev.off() 
    
  }
}




