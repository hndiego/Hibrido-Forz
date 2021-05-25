####
#comment done in fco's git account
setwd("~/Desktop/BP") # set directory

library(data.table)
library(dplyr)
library(ncdf4)
library(xml2)
library(ecmwfr)
library(magrittr)

rm(list = ls())

####

## PRECIPITACION

# set a key to the keychain
wf_set_key(user = "28041",
           key = "2c19eea2-8760-4e86-9461-3c12789c30d3",
           service = "cds")

## vars: "2m_temperature", "total_precipitation"

request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("total_precipitation"),
  # year = c("2020"),
  # month = c("04", "05", "06", "07", "08", "09", "10", "11", "12"),
  year = c("2020", "2021"),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
          "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
          "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00",
           "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00",
           "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(-34, -72, -37, -70),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "era5 actual/era5_delta-actual_precip.nc"
)

# If you have stored your user login information
# in the keyring by calling cds_set_key you can
# call:
file <- wf_request(user     = "28041",   # user ID (for authentification)
                   request  = request,  # the request
                   transfer = TRUE,     # download the file
                   path     = ".")      # store data in current working directory

####

## TEMPERATURA

# set a key to the keychain
#wf_set_key(user = "28041",
#           key = "2c19eea2-8760-4e86-9461-3c12789c30d3",
#            service = "cds")

## vars: "2m_temperature", "total_precipitation"
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature"),
  # year = c("2020"),
  # month = c("04", "05", "06", "07", "08", "09", "10", "11", "12"),
  year = c("2020", "2021"),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
          "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
          "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00",
           "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00",
           "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(-34, -72, -37, -70),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "era5 actual/era5_delta-actual_temp.nc"
)

# If you have stored your user login information
# in the keyring by calling cds_set_key you can
# call:
file <- wf_request(user     = "28041",   # user ID (for authentification)
                   request  = request,  # the request
                   transfer = TRUE,     # download the file
                   path     = ".")      # store data in current working directory

####
