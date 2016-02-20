library('magrittr')
library('dplyr')
library('ggplot2')


mainland_india <- map_data('world',region = 'India') %>% filter(is.na(subregion)) %>% select(long, lat, order)

ggplot() + geom_polygon(data = mainland_india, aes(x = long, y = lat), fill = gray(0.3))

east_coast <- mainland_india %>% filter(long < 90, long > 77.5, lat < 22.5)

ggplot() + geom_path(data = east_coast, aes(x = long, y = lat))


x <- east_coast$long
y <- east_coast$lat

lin.mod <- lm(y~x)
library(segmented)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi = c(80,82,87))
plot(x, y, type = 'l')
plot(segmented.mod, add = TRUE)

saveRDS(segmented.mod, file = '5-east-coast-segmented-model.rds')
