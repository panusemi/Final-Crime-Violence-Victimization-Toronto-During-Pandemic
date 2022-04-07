# Obtain and clean data

# Open Data Toronto

library(opendatatoronto)
library(dplyr)

# get package
package <- show_package("fc4d95a6-591f-411f-af17-327e6c5d03c7")
package

# get all resources for this package
resources <- list_package_resources("fc4d95a6-591f-411f-af17-327e6c5d03c7")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()

write.csv(data, "crime_data.csv")


# General Social Survey- Victimization 
