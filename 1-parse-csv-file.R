options(stringsAsFactors = FALSE)
library('stringr')

cyclones <- read.csv(
  './Basin.NI.ibtracs_wmo.v03r08.csv',
  # url('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r08/wmo/csv/basin/Basin.NI.ibtracs_wmo.v03r08.csv'),
  skip = 1, stringsAsFactors = FALSE)

cyclones <- cyclones[-1, ] 

# Trimming the white space at both ends of the strings
cyclones[] <- lapply(cyclones, str_trim)

# Converting numerical columns
numeric_columns <- c('Season', 'Latitude', 'Longitude', 'Wind.WMO.', 'Pres.WMO.')
cyclones[numeric_columns] <- lapply(cyclones[numeric_columns], as.numeric)

# Note: not sure about the time zone
cyclones$ISO_time <- as.POSIXct(cyclones$ISO_time, tz = 'UTC')

saveRDS(cyclones, '1-cyclones-data-parsed.rds')
