cyclones <- readRDS('1-cyclones-data-parsed.rds')

splines_data <-
  by(data = cyclones, INDICES = cyclones$Serial_Num, simplify = FALSE,
     FUN = function(x) {
       long_lat <- x[,c('Longitude', 'Latitude')]
       plot(long_lat)
       spline_data <- data.frame(xspline(long_lat, shape = -0.2,
                                         lwd = 2,  draw = FALSE))                        
       spline_data
     })


saveRDS(splines_data, '2-splines-data.rds')