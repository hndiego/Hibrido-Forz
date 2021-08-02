####

setwd("~/Desktop/BP")

library(dplyr)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)
library(magrittr)

rm(list = ls())

####

## EDIT: EXT CR2MET ABR/2020
## SE USA CR2MET V2.0 (SIN CORRECCION POR UNDERCATCH)

####

## Netcdfs fueron trabajados en CDO!

####

## PRECIPITACION

## Extrae pr cr2met-undercatch
## Precaución con coords... ¿lat invertida?

nc_cr2u  <- "forzantes_escala_cuenca/CR2MET_pr_v2.0_day_1979_2020_005deg.nc" %>% nc_open
cr2u_lon <- nc_cr2u %>% ncvar_get("lon")
cr2u_lat <- nc_cr2u %>% ncvar_get("lat")
cr2u_t   <- nc_cr2u %>% ncvar_get("time") %>% `+`(as.POSIXct("1979-01-01 00:00:00", tz = "UTC")) %>% as.Date
# Acota región 72 @ 70 °W, 34 @ 37 °S
bool_lon <- cr2u_lon <= -70 & cr2u_lon >= -72
bool_lat <- cr2u_lat <= -34 & cr2u_lat >= -37
slon     <- which(bool_lon %in% T)[1]
nlon     <- sum(bool_lon %in% T)
slat     <- which(bool_lat %in% T)[1]
nlat     <- sum(bool_lat %in% T)
cr2u_lon <- cr2u_lon[bool_lon]
cr2u_lat <- cr2u_lat[bool_lat]
cr2u_pr  <- nc_cr2u %>% ncvar_get("pr", start = c(slon, slat, 1), count = c(nlon, nlat, -1))
nc_close(nc_cr2u)
rm(nc_cr2u, bool_lon, bool_lat, slon, slat, nlon, nlat)
rm(cr2u_lat, cr2u_lon, cr2u_t)

## Extrae pr era5

nc_lsp <- "forzantes_escala_cuenca/era5_lsp_1979-2020_day.nc" %>% nc_open
e5_lon <- nc_lsp %>% ncvar_get("longitude") %>% `-`(360)  # Este a Oeste
e5_lat <- nc_lsp %>% ncvar_get("latitude")
e5pr_t <- as.POSIXct("1900-01-01 00:00", tz = "GMT") %>%
  `+`(nc_lsp %>% ncvar_get("forecast_initial_time") %>% as.integer %>% hours) %>% as.Date
e5_lsp <- nc_lsp %>% ncvar_get("LSP")
nc_close(nc_lsp)
nc_cp  <- "forzantes_escala_cuenca/era5_cp_1979-2020_day.nc" %>% nc_open
e5_cp  <- nc_cp %>% ncvar_get("CP")
nc_close(nc_cp)
# Suma precip y convierte m a mm, tp = lsp + cp
e5_pr  <- 1000 * (e5_lsp + e5_cp)
rm(nc_lsp, nc_cp, e5_lsp, e5_cp)
  
####

## TEMPERATURA

## Extrae tm cr2met
## Precaución con coords... ¿lat invertida?

nc_cr2   <- "forzantes_escala_cuenca/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc" %>% nc_open
cr2_lon  <- nc_cr2 %>% ncvar_get("lon")
cr2_lat  <- nc_cr2 %>% ncvar_get("lat")
## EDIT: VIENE MAL EL VECTOR DE DIAS
## SE REPITE 01-02-2021 (DUPLICADO)... EL LARGO COINCIDE PORQUE COMPENSA EL BISIESTO
## SE CORRIGE MANUALMENTE
#cr2_t    <- nc_cr2 %>% ncvar_get("time") %>% `+`(as.POSIXct("1979-01-01 00:00:00", tz = "UTC")) %>% as.Date
cr2_t    <- seq(from = as.Date("1979-01-01"), to = as.Date("2020-04-30"), by = "day")
## FIN EDIT
# Acota región 72 @ 70 °W, 34 @ 37 °S
bool_lon <- cr2_lon <= -70 & cr2_lon >= -72
bool_lat <- cr2_lat <= -34 & cr2_lat >= -37
slon     <- which(bool_lon %in% T)[1]
nlon     <- sum(bool_lon %in% T)
slat     <- which(bool_lat %in% T)[1]
nlat     <- sum(bool_lat %in% T)
cr2_lon  <- cr2_lon[bool_lon]
cr2_lat  <- cr2_lat[bool_lat]
cr2_tm   <- nc_cr2 %>% ncvar_get("t2m", start = c(slon, slat, 1), count = c(nlon, nlat, -1))
nc_close(nc_cr2)
rm(nc_cr2, bool_lon, bool_lat, slon, slat, nlon, nlat)
  
## Extrae tm era5

nc_tm  <- "forzantes_escala_cuenca/era5_t2m_1979-2020_day.nc" %>% nc_open
e5_lon <- nc_tm %>% ncvar_get("longitude") %>% `-`(360)  # Este a Oeste
e5_lat <- nc_tm %>% ncvar_get("latitude")
e5tm_t   <- as.POSIXct("1900-01-01 00:00", tz = "GMT") %>%
  `+`(nc_tm %>% ncvar_get("time") %>% as.integer %>% hours) %>% as.Date
e5_tm  <- nc_tm %>% ncvar_get("VAR_2T") %>% `-`(273.15)  # Kelvin a Celsius
nc_close(nc_tm)
rm(nc_tm)

####

save.image("~/Desktop/BP/ws_EscalaCuenca2.RData")
