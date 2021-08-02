####

setwd("~/Desktop/BP")

library(dplyr)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)
library(R.utils)
library(magrittr)

rm(list = ls())

####

## EDIT: EXT CR2MET ABR/2020
## SE USA CR2MET V2.0 (SIN CORRECCION POR UNDERCATCH)

####

## Data bruta pr-ERA5 (res horaria, 1h forecast) en formato de descarga de UCAR/NCAR 
## Data bruta ERA5 es 4D [lon, lat, leadtime_forecast, time_forecast] 
## Lo siguiente pasa netcdf de 4D a 3D ([lon, lat, time])
## Luego se usan comandos CDO (por fuera, en la consola!)
## CDO: concatenar y llevar a resolucion diaria (precip desplazar en -1 hora)
## Desplazar según tiempo local! (-4h?)

## Reshape: convective precipitation (CP)
Dirs      <- dir("era5 2020")
Dirs      <- Dirs[! Dirs %in% c("cp", "lsp", "t2m")]
for (dir in Dirs){
  ncPath  <- dir
  ncFile  <- paste0("era5 2020/", ncPath) %>% nc_open
  ncFch   <- ncvar_get(ncFile, "forecast_hour")
  ncFcit  <- ncvar_get(ncFile, "forecast_initial_time")
  ncPR    <- ncvar_get(ncFile, "CP")
  ncLon   <- ncvar_get(ncFile, "longitude")
  ncLat   <- ncvar_get(ncFile, "latitude")
  nc_close(ncFile)
  nc2Fcit    <- lapply(ncFcit, function(it) it + ncFch) %>% unlist
  nc2PR      <- wrap(ncPR, map = list(1, 2, NA))
  nc2DimFcit <- ncdim_def(name = "forecast_initial_time", units = "hours since 1900-01-01 00:00:00", vals = nc2Fcit,
                          unlim = T, create_dimvar = T, calendar = "gregorian", longname = "forecast initial time")
  nc2DimLon  <- ncdim_def(name = "longitude", units = "degrees_east", vals = ncLon, create_dimvar = T, longname = "longitude")
  nc2DimLat  <- ncdim_def(name = "latitude", units = "degrees_north", vals = ncLat, create_dimvar = T, longname = "latitude")
  nc2VarPR   <- ncvar_def(name = "CP", units = "m", dim = list(nc2DimLon, nc2DimLat, nc2DimFcit), longname = "Convective precipitation")
  nc2        <- nc_create(paste0("era5 2020/cp/", ncPath), nc2VarPR)
  ncvar_put(nc2, nc2VarPR, nc2PR, start = c(1, 1, 1), count = c(length(ncLon), length(ncLat), length(nc2Fcit)))
  nc_sync(nc2)
  nc_close(nc2)
  rm(dir, ncPath, ncFile, ncFch, ncFcit, ncPR, ncLat, ncLon, nc2Fcit, nc2PR, nc2DimFcit, nc2DimLat, nc2DimLon, nc2VarPR, nc2)
}
rm(Dirs)

## Reshape: large scale precipitation (LSP)
Dirs      <- dir("era5 2020")
Dirs      <- Dirs[! Dirs %in% c("cp", "lsp", "t2m")]
for (dir in Dirs){
  ncPath  <- dir
  ncFile  <- paste0("era5 2020/", ncPath) %>% nc_open
  ncFch   <- ncvar_get(ncFile, "forecast_hour")
  ncFcit  <- ncvar_get(ncFile, "forecast_initial_time")
  ncPR    <- ncvar_get(ncFile, "LSP")
  ncLon   <- ncvar_get(ncFile, "longitude")
  ncLat   <- ncvar_get(ncFile, "latitude")
  nc_close(ncFile)
  nc2Fcit    <- lapply(ncFcit, function(it) it + ncFch) %>% unlist
  nc2PR      <- wrap(ncPR, map = list(1, 2, NA))
  nc2DimFcit <- ncdim_def(name = "forecast_initial_time", units = "hours since 1900-01-01 00:00:00", vals = nc2Fcit,
                          unlim = T, create_dimvar = T, calendar = "gregorian", longname = "forecast initial time")
  nc2DimLon  <- ncdim_def(name = "longitude", units = "degrees_east", vals = ncLon, create_dimvar = T, longname = "longitude")
  nc2DimLat  <- ncdim_def(name = "latitude", units = "degrees_north", vals = ncLat, create_dimvar = T, longname = "latitude")
  nc2VarPR   <- ncvar_def(name = "LSP", units = "m", dim = list(nc2DimLon, nc2DimLat, nc2DimFcit), longname = "Large-scale precipitation")
  nc2        <- nc_create(paste0("era5 2020/lsp/", ncPath), nc2VarPR)
  ncvar_put(nc2, nc2VarPR, nc2PR, start = c(1, 1, 1), count = c(length(ncLon), length(ncLat), length(nc2Fcit)))
  nc_sync(nc2)
  nc_close(nc2)
  rm(dir, ncPath, ncFile, ncFch, ncFcit, ncPR, ncLat, ncLon, nc2Fcit, nc2PR, nc2DimFcit, nc2DimLat, nc2DimLon, nc2VarPR, nc2)
}
rm(Dirs)

####

## En CDO se llevan variables a escala diaria

####

