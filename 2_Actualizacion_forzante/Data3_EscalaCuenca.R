####

setwd("~/Desktop/BP v2")

library(dplyr)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(magrittr)

rm(list = ls())

####

## Netcdfs fueron trabajados en CDO!

####

## Extrae precipitacion era5
nc_pr  <- "era5 actual/era5_actual_precip_daily.nc" %>% nc_open
e5_lon <- nc_pr %>% ncvar_get("longitude")# %>% `-`(360)  # Este a Oeste
e5_lat <- nc_pr %>% ncvar_get("latitude")
e5pr_t <- as.Date(ncvar_get(nc_pr, "time") * 3600 + as.POSIXct("1900-01-01 00:00", tz = "GMT"))
#e5pr_t <- ncvar_get(nc_pr, "time") * 3600 + as.POSIXct("1900-01-01 00:00", tz = "GMT")
e5_pr  <- 1000 * ncvar_get(nc_pr, "tp")
nc_close(nc_pr)
rm(nc_pr)

## Extrae temperatura era5
nc_tm  <- "era5 actual/era5_actual_temp_daily.nc" %>% nc_open
e5_lon <- nc_tm %>% ncvar_get("longitude")# %>% `-`(360)  # Este a Oeste
e5_lat <- nc_tm %>% ncvar_get("latitude")
e5tm_t <- as.Date(ncvar_get(nc_tm, "time") * 3600 + as.POSIXct("1900-01-01 00:00", tz = "GMT"))
#e5tm_t <- ncvar_get(nc_tm, "time") * 3600 + as.POSIXct("1900-01-01 00:00", tz = "GMT")
e5_tm  <- ncvar_get(nc_tm, "t2m") - 273.15  # Kelvin a Celsius
nc_close(nc_tm)
rm(nc_tm)

####

## Formato string

e5_lon  <- e5_lon %>% round(2) %>% format(nsmall = 2)
e5_lat  <- e5_lat %>% round(2) %>% format(nsmall = 2)

## Adquiere fracciones (ponderadores por cuenca) desde analisis en QGIS
## Esto para agregar pixeles de cr2met/era5 a escala de cuenca!

setwd("~/Desktop/BP v2/forzantes_escala_cuenca")

## GRILLA: ERA5

fracs <- rbind(data.table(cbind(read.table("era5_inter_Achibueno.csv", header = T, sep = ",", as.is = T), cuenca = "Achibueno")),
               data.table(cbind(read.table("era5_inter_Ancoa.csv", header = T, sep = ",", as.is = T), cuenca = "Ancoa")),
               data.table(cbind(read.table("era5_inter_Longavi.csv", header = T, sep = ",", as.is = T), cuenca = "Longavi")),
               data.table(cbind(read.table("era5_inter_Lontue.csv", header = T, sep = ",", as.is = T), cuenca = "Lontue")),
               data.table(cbind(read.table("era5_inter_Maule.csv", header = T, sep = ",", as.is = T), cuenca = "Maule")),
               data.table(cbind(read.table("era5_inter_Melado.csv", header = T, sep = ",", as.is = T), cuenca = "Melado"))) %>%
  .[ , c("x", "y", "frac_inter", "cuenca")]

fracs$x     <- fracs$x %>% round(2) %>% format(nsmall = 2)
fracs$y     <- fracs$y %>% round(2) %>% format(nsmall = 2)
fracs$pixel <- paste(fracs$x, fracs$y)

## Corrige factores por error de redondeo

fracs <- fracs %>% group_by(cuenca) %>% mutate(frac_tot = sum(frac_inter)) %>% mutate(frac_inter = frac_inter / frac_tot) %>%
  ungroup %>% .[, c("x", "y", "frac_inter", "cuenca", "pixel")]

## Identifica coordenadas de pixeles

Cuencas <- fracs$cuenca %>% unique
Pixeles <- fracs$pixel %>% unique

## Df data

lpix_pr <- vector("list", length(Pixeles)) %>% setNames(Pixeles)
lpix_tm <- vector("list", length(Pixeles)) %>% setNames(Pixeles)
for (Pix in Pixeles){
  boolx <- e5_lon %in% (Pix %>% strsplit(split = " ") %>% unlist %>% .[1])
  booly <- e5_lat %in% (Pix %>% strsplit(split = " ") %>% unlist %>% .[2])
  lpix_pr[[Pix]] <- data.table(pixel = Pix, date = e5pr_t, pr = e5_pr[boolx, booly, ])
  lpix_tm[[Pix]] <- data.table(pixel = Pix, date = e5tm_t, tm = e5_tm[boolx, booly, ])
}
df_pr <- do.call(rbind, lpix_pr)
rownames(df_pr) <- NULL
df_tm <- do.call(rbind, lpix_tm)
rownames(df_tm) <- NULL
rm(Pix, boolx, booly, lpix_pr, lpix_tm)

## Agrega a nivel cuenca

lpix_pr <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
lpix_tm <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
for (Cuenca in Cuencas){
  Subpixs    <- fracs %>% filter(cuenca %in% Cuenca) %$% pixel
  lsubpix_pr <- vector("list", length(Subpixs)) %>% setNames(Subpixs)
  lsubpix_tm <- vector("list", length(Subpixs)) %>% setNames(Subpixs)
  for (Subpix in Subpixs){
    pond                 <- filter(fracs, cuenca %in% Cuenca & pixel %in% Subpix)$frac_inter
    lsubpix_pr[[Subpix]] <- filter(df_pr, pixel %in% Subpix)$pr * pond
    lsubpix_tm[[Subpix]] <- filter(df_tm, pixel %in% Subpix)$tm * pond
  }
  prcuenca <- Reduce(`+`, lsubpix_pr)
  tmcuenca <- Reduce(`+`, lsubpix_tm)
  lpix_pr[[Cuenca]] <- data.table(cuenca = Cuenca, date = e5pr_t, pr = prcuenca)
  lpix_tm[[Cuenca]] <- data.table(cuenca = Cuenca, date = e5tm_t, tm = tmcuenca)
}
df_pr_e5 <- do.call(rbind, lpix_pr)
rownames(df_pr_e5) <- NULL
df_tm_e5 <- do.call(rbind, lpix_tm)
rownames(df_tm_e5) <- NULL
rm(lpix_pr, lpix_tm, Cuenca, Subpixs, Subpix, lsubpix_pr, lsubpix_tm, pond, prcuenca, tmcuenca,
   Cuencas, Pixeles, df_pr, df_tm, fracs)

####

primerdia <- as.Date("2021-04-01")
ultimodia <- as.Date("2021-07-29")  # En general ultimo dia esta truncado... verificar!

actual_pr <- filter(df_pr_e5, date >= primerdia & date <= ultimodia)
actual_tm <- filter(df_tm_e5, date >= primerdia & date <= ultimodia)

save(actual_pr, actual_tm, file = "~/Desktop/BP v2/era5 actual/actual_era5.RData")

####  
