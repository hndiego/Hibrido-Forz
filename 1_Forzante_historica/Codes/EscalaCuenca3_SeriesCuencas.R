  ####

library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)
library(magrittr)

rm(list = ls())

####

load("~/Desktop/BP/ws_EscalaCuenca2.RData")

## Formato string

cr2_lon <- cr2_lon %>% round(3) %>% format(nsmall = 3)
cr2_lat <- cr2_lat %>% round(3) %>% format(nsmall = 3)
e5_lon  <- e5_lon %>% round(2) %>% format(nsmall = 2)
e5_lat  <- e5_lat %>% round(2) %>% format(nsmall = 2)

####

## Adquiere fracciones (ponderadores por cuenca) desde analisis en QGIS
## Esto para agregar pixeles de cr2met/era5 a escala de cuenca!

setwd("~/Desktop/BP/forzantes_escala_cuenca")

####

## GRILLA: CR2MET

fracs <- rbind(data.table(cbind(read.table("cr2met_inter_Achibueno.csv", header = T, sep = ",", as.is = T), cuenca = "Achibueno")),
               data.table(cbind(read.table("cr2met_inter_Ancoa.csv", header = T, sep = ",", as.is = T), cuenca = "Ancoa")),
               data.table(cbind(read.table("cr2met_inter_Longavi.csv", header = T, sep = ",", as.is = T), cuenca = "Longavi")),
               data.table(cbind(read.table("cr2met_inter_Lontue.csv", header = T, sep = ",", as.is = T), cuenca = "Lontue")),
               data.table(cbind(read.table("cr2met_inter_Maule.csv", header = T, sep = ",", as.is = T), cuenca = "Maule")),
               data.table(cbind(read.table("cr2met_inter_Melado.csv", header = T, sep = ",", as.is = T), cuenca = "Melado"))) %>%
  .[ , c("x", "y", "frac_inter", "cuenca")]

fracs$x     <- fracs$x %>% round(3) %>% format(nsmall = 3)
fracs$y     <- fracs$y %>% round(3) %>% format(nsmall = 3)
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
  boolx <- cr2_lon %in% (Pix %>% strsplit(split = " ") %>% unlist %>% .[1])
  booly <- cr2_lat %in% (Pix %>% strsplit(split = " ") %>% unlist %>% .[2])
  lpix_pr[[Pix]] <- data.table(pixel = Pix, date = cr2_t, pr = cr2u_pr[boolx, booly, ])
  lpix_tm[[Pix]] <- data.table(pixel = Pix, date = cr2_t, tm = cr2_tm[boolx, booly, ])
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
    pond                 <- fracs %>% filter(cuenca %in% Cuenca & pixel %in% Subpix) %$% frac_inter
    lsubpix_pr[[Subpix]] <- (df_pr %>% filter(pixel %in% Subpix) %$% pr) * pond
    lsubpix_tm[[Subpix]] <- (df_tm %>% filter(pixel %in% Subpix) %$% tm) * pond
  }
  prcuenca <- Reduce(`+`, lsubpix_pr)
  tmcuenca <- Reduce(`+`, lsubpix_tm)
  lpix_pr[[Cuenca]] <- data.table(cuenca = Cuenca, date = cr2_t, pr = prcuenca)
  lpix_tm[[Cuenca]] <- data.table(cuenca = Cuenca, date = cr2_t, tm = tmcuenca)
}
df_pr_cr2 <- do.call(rbind, lpix_pr)
rownames(df_pr_cr2) <- NULL
df_tm_cr2 <- do.call(rbind, lpix_tm)
rownames(df_tm_cr2) <- NULL
rm(lpix_pr, lpix_tm, Cuenca, Subpixs, Subpix, lsubpix_pr, lsubpix_tm, pond, prcuenca, tmcuenca,
   Cuencas, Pixeles, df_pr, df_tm, fracs)

####

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
    pond                 <- fracs %>% filter(cuenca %in% Cuenca & pixel %in% Subpix) %$% frac_inter
    lsubpix_pr[[Subpix]] <- (df_pr %>% filter(pixel %in% Subpix) %$% pr) * pond
    lsubpix_tm[[Subpix]] <- (df_tm %>% filter(pixel %in% Subpix) %$% tm) * pond
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

## EDIT: unifica horizonte temporal
df_pr_e5 <- filter(df_pr_e5, date %in% cr2_t)
df_tm_e5 <- filter(df_tm_e5, date %in% cr2_t)

save.image("~/Desktop/BP/ws_EscalaCuenca3_SeriesCuenca.RData")
