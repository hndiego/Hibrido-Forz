####

setwd("~/Desktop/BP")

library(abind)
library(data.table)
library(dplyr)
library(ncdf4)
library(magrittr)

rm(list = ls())

####

allNA <- function(x){all(is.na(x))}

## PRECIPITACION

nc_pr      <- "era5 actual/era5_delta-actual_precip.nc" %>% nc_open
nc_prVar   <- ncvar_get(nc_pr, "tp")
#nc_prVar1  <- ncvar_get(nc_pr, "tp_0001")
#bool1      <- ! apply(nc_prVar1, 3, allNA)
#nc_prVar5  <- ncvar_get(nc_pr, "tp_0005")
#bool5      <- ! apply(nc_prVar5, 3, allNA)
#nc_prVar   <- abind(nc_prVar1[ , , bool1], nc_prVar5[ , , bool5], along = 3)
rm(nc_prVar1, bool1, nc_prVar5, bool5)
nc_prLon   <- ncvar_get(nc_pr, "longitude")
nc_prLat   <- ncvar_get(nc_pr, "latitude")
nc_prTime  <- ncvar_get(nc_pr, "time")
nc_close(nc_pr)

nc2_prVar  <- apply(nc_prVar, c(1, 2, 4), mean, na.rm = T)
#nc2_prVar  <- nc_prVar
nc2DimTime <- ncdim_def(name = "time", units = "hours since 1900-01-01 00:00:00.0", vals = nc_prTime,
                        unlim = T, create_dimvar = T, calendar = "gregorian", longname = "time")
nc2DimLon  <- ncdim_def(name = "longitude", units = "degrees_east", vals = nc_prLon, create_dimvar = T, longname = "longitude")
nc2DimLat  <- ncdim_def(name = "latitude", units = "degrees_north", vals = nc_prLat, create_dimvar = T, longname = "latitude")
nc2VarPR   <- ncvar_def(name = "tp", units = "m", dim = list(nc2DimLon, nc2DimLat, nc2DimTime), longname = "Total precipitation")
nc2        <- nc_create("era5 actual/era5_actual_precip_pre-cdo.nc", nc2VarPR)
ncvar_put(nc2, nc2VarPR, nc2_prVar, start = c(1, 1, 1), count = c(length(nc_prLon), length(nc_prLat), length(nc_prTime)))
nc_sync(nc2)
nc_close(nc2)

rm(nc_pr, nc_prVar, nc_prLon, nc_prLat, nc_prTime,
   nc2_prVar, nc2DimTime, nc2DimLon, nc2DimLat, nc2VarPR, nc2)

## TEMPERATURA

nc_tm     <- "era5 actual/era5_delta-actual_temp.nc" %>% nc_open
#nc_tmVar1  <- ncvar_get(nc_tm, "t2m_0001")
#bool1      <- ! apply(nc_tmVar1, 3, allNA)
#nc_tmVar5  <- ncvar_get(nc_tm, "t2m_0005")
#bool5      <- ! apply(nc_tmVar5, 3, allNA)
#nc_tmVar   <- abind(nc_tmVar1[ , , bool1], nc_tmVar5[ , , bool5], along = 3)
rm(nc_tmVar1, bool1, nc_tmVar5, bool5)
nc_tmVar  <- ncvar_get(nc_tm, "t2m")
nc_tmLon  <- ncvar_get(nc_tm, "longitude")
nc_tmLat  <- ncvar_get(nc_tm, "latitude")
nc_tmTime <- ncvar_get(nc_tm, "time")
nc_close(nc_tm)

nc2_tmVar  <- apply(nc_tmVar, c(1, 2, 4), mean, na.rm = T)
#nc2_tmVar  <- nc_tmVar
nc2DimTime <- ncdim_def(name = "time", units = "hours since 1900-01-01 00:00:00.0", vals = nc_tmTime,
                        unlim = T, create_dimvar = T, calendar = "gregorian", longname = "time")
nc2DimLon  <- ncdim_def(name = "longitude", units = "degrees_east", vals = nc_tmLon, create_dimvar = T, longname = "longitude")
nc2DimLat  <- ncdim_def(name = "latitude", units = "degrees_north", vals = nc_tmLat, create_dimvar = T, longname = "latitude")
nc2VarTM   <- ncvar_def(name = "t2m", units = "K", dim = list(nc2DimLon, nc2DimLat, nc2DimTime), longname = "2 metre temperature")
nc2        <- nc_create("era5 actual/era5_actual_temp_pre-cdo.nc", nc2VarTM)
ncvar_put(nc2, nc2VarTM, nc2_tmVar, start = c(1, 1, 1), count = c(length(nc_tmLon), length(nc_tmLat), length(nc_tmTime)))
nc_sync(nc2)
nc_close(nc2)

rm(nc_tm, nc_tmVar, nc_tmLon, nc_tmLat, nc_tmTime,
   nc2_tmVar, nc2DimTime, nc2DimLon, nc2DimLat, nc2VarTM, nc2)

####
  
## AHORA ESTO SE PASA A CDO ##
## CORRECCION UTC & PASO A SERIES DIARIAS ##

####
  