

# TODO: make a note that the point size is not representative of the area that got affected
# TODO: also more plots (line/bar) are possible showing the intensity (wind of pressure) of cyclones vs time or state or place of contact
# TODO: talk about the gg layers, thank hadley and others

# TODO: splines ----------------------------------------
# - http://stackoverflow.com/questions/11356997/interpolating-a-path-curve-within-r
# - http://stackoverflow.com/questions/7729058/how-can-i-get-a-cubic-bezier-curve-closest-to-given-points
# - https://www.particleincell.com/2012/bezier-splines/
# - https://www.wikiwand.com/en/B%C3%A9zier_curve
# - http://stackoverflow.com/questions/31554390/how-to-smooth-curves-line-graph-in-ggplot

ggmap(BayOfBengalMap) + 
  # geom_path(data = stormsData %>% filter(Serial_Num %in% Serial_Num[1]),
  #           aes(x = Longitude,
  #               y = Latitude,
  #               group = Serial_Num),
  #           linetype="dashed",
  #           alpha = 0.5,
  #           size = 0.8) +
  geom_xspline(data = stormsData %>% filter(Serial_Num %in% Serial_Num[1]),
               aes(x = Longitude,
                   y = Latitude,
                   group = Serial_Num,
                   color = Wind.WMO.),
               spline_shape=1, size=0.5)


# TODO: better map -----------------------------------------
world <- map_data("world")
#> 
#>  # ATTENTION: maps v3.0 has an updated 'world' map.        #
#>  # Many country borders and names have changed since 1990. #
#>  # Type '?world' or 'news(package="maps")'. See README_v3. #
world <- world[world$region != "Antarctica",]
library(maps)
# World in Winkel-Tripel
world <- map_data("world")
world <- world[world$region != "Antarctica",]

gg <- ggplot()
gg <- gg + geom_map(data=world, map=world,
                    aes(x=long, y=lat, map_id=region))
gg <- gg + coord_proj("+proj=wintri")
gg

# U.S.A. Albers-style
usa <- world[world$region == "India",] %>% filter(long > 75 & lat < 25)

gg <- ggplot()
gg <- gg + geom_map(data=usa, map=usa,
                    aes(x=long, y=lat, map_id=region), color = 'red', fill = 'white')
gg <- gg + coord_proj(
  paste0("+proj=merc  +lon_0=80"))
gg

# Showcase Greenland (properly)
greenland <- world[world$region == "Greenland",]

gg <- ggplot()
gg <- gg + geom_map(data=greenland, map=greenland,
                    aes(x=long, y=lat, map_id=region))
gg <- gg + coord_proj(
  paste0("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0",
         " +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
gg


# TODO: animate everything wrt 0th hour
# TODO: read about how to measure intensity of cyclone