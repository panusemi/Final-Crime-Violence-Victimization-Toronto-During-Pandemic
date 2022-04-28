#### Preamble ####
# Purpose: Prepare the crime data
# Author: Emily Panus
# Data: 27 April 2022
# Contact: emily.panus@mail.utoronto.ca
# Pre-requisites: 
# - Need to have obtain/downloaded the data and saved it to inputs/data

# Open Data Toronto Crime Rates 
library(opendatatoronto)
library(dplyr)
library(tidyverse)
library(tidyr)
library(sf)

# get package
package <- show_package("fc4d95a6-591f-411f-af17-327e6c5d03c7")
package

# get all resources for this package
resources <- list_package_resources("fc4d95a6-591f-411f-af17-327e6c5d03c7")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
crime_rates_data <- filter(datastore_resources, row_number()==1) %>% get_resource()

crime_rates_data <- crime_rates_data %>% st_drop_geometry()

View(crime_rates_data)

write.csv(crime_rates_data, "crime_rates_data.csv")



# Open Data Toronto Crime Indicators 
library(opendatatoronto)
library(dplyr)

# get package
package <- show_package("247788f6-ca20-42e8-b00f-894ac43053e5")
package

resources <- list_package_resources("247788f6-ca20-42e8-b00f-894ac43053e5")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'shp'))

# load the first datastore resource as a sample
crime_indicator_data <- filter(datastore_resources, row_number()==1) %>% get_resource()

write.csv(crime_indicator_data, "crime_indicator_data.csv")




# Open Data Toronto Hate Crime

# get package
package <- show_package("police-annual-statistical-report-miscellaneous-data")
package

# get all resources for this package
resources <- list_package_resources("police-annual-statistical-report-miscellaneous-data")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
hate_crime_data <- filter(datastore_resources, row_number()==1) %>% get_resource()

write.csv(hate_crime_data, "hate_crime_data.csv")



# General Social Survey- Victimization 
victimization_gss <- read.csv("C:/Users/Emily's Laptop/Desktop/sta304-final/gss-12M0018-E-2014-c-28-vmf (1)/gss-12M0018-E-2014-c-28-vmf_F1.csv")

victimization_gss <- victimization_gss %>% 
  select(CIP_10, DIS_15,  DIS_20, DTS_30, MSHSD, MSPER, MSVIC, TOTVIC) %>% 
  mutate(CIP_10 = case_when(
    CIP_10 == 1 ~ "Great deal",
    CIP_10 == 2 ~ "Some",
    CIP_10 == 3 ~ "Not very much",
    CIP_10 == 4 ~ "None", 
    CIP_10 == 6 ~ "Valid skip",
    CIP_10 == 7 ~ "Don't know",
    CIP_10 == 8 ~ "Refusal",
    CIP_10 == 9 ~ "Not stated"),
    DIS_15 = case_when(
      DIS_15 == 1 ~ "Yes",
      DIS_15 == 2 ~ "No",
      DIS_15 == 6 ~ "Valid skip",
      DIS_15 == 7 ~ "Don't know", 
      DIS_15 == 8 ~ "Refusal",
      DIS_15 == 9 ~ "Not stated"), 
    DIS_20 = case_when(
      DIS_20 == 1 ~ "Yes",
      DIS_20 == 2 ~ "No",
      DIS_20 == 6 ~ "Valid skip",
      DIS_20 == 7 ~ "Don't know", 
      DIS_20 == 8 ~ "Refusal",
      DIS_20 == 9 ~ "Not stated"), 
    DTS_30 = case_when(
      DTS_30 == 1 ~ "Yes",
      DTS_30 == 2 ~ "No",
      DTS_30 == 6 ~ "Valid skip",
      DTS_30 == 7 ~ "Don't know", 
      DTS_30 == 8 ~ "Refusal",
      DTS_30 == 9 ~ "Not stated"),
    MSVIC = case_when(
      MSVIC == 101 ~ "Sexual assualt",
      MSVIC == 202 ~ "Robbery",
      MSVIC == 203 ~ "Attempted robbery",
      MSVIC == 304 ~ "Assualt", 
      MSVIC == 405 ~ "Break and enter",
      MSVIC == 406 ~ "Attempted break and enter",
      MSVIC == 507 ~ "Motor vehicle theft",
      MSVIC == 508 ~ "Part of motor vehicle theft", 
      MSVIC == 509 ~ "Attempted motor vehicle theft",
      MSVIC == 609 ~ "Theft of personal property",
      MSVIC == 610 ~ "Attempted theft of personal property",
      MSVIC == 711 ~ "Theft of household property",
      MSVIC == 712 ~ "Attempted theft of household property",
      MSVIC == 813 ~ "Vandalism",
      MSVIC == 994 ~ "Not asked-crime not reported",
      MSVIC == 995 ~ "Unclassifiable",
      MSVIC == 996 ~ "Valid skip",
      MSVIC == 997 ~ "Don't know", 
      MSVIC == 998 ~ "Refusal",
      MSVIC == 999 ~ "Not stated"), 
    TOTVIC = case_when(
      TOTVIC == 0 ~ "Not victimized",
      TOTVIC == 1 ~ "Victimized one time",
      TOTVIC == 2 ~ "Victimized two times",
      TOTVIC == 3 ~ "Victimized three or more times", 
      TOTVIC == 6 ~ "Valid skip",
      TOTVIC == 7 ~ "Don't know",
      TOTVIC == 8 ~ "Refusal",
      TOTVIC == 9 ~ "Not stated"))

write.csv(victimization_gss, "victimization_gss.csv")
