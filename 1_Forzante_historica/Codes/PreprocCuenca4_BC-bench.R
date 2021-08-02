####  

setwd("~/Desktop/BP v2")

library(data.table)
library(dplyr)
library(reshape2)
library(MBC)
library(crch)
library(hydroGOF)
library(magrittr)

rm(list = ls())

####

## Data cr2met y era5
## Consolidado verificacion cruzada! deja-53fuera 

load("ws_PreprocCuenca2_Data.RData")

PeriodosVal  <- list(c(1979, 1981), c(1982, 1984), c(1985, 1987), c(1988, 1990), c(1991, 1993), c(1994, 1996), c(1997, 1999),
                     c(2000, 2002), c(2003, 2005), c(2006, 2008), c(2009, 2011), c(2012, 2014), c(2015, 2017), c(2018, 2020))
PeriodosName <- sapply(PeriodosVal, function(per) paste(per[1], per[2], sep = "-"))
PeriodosVal  <- setNames(PeriodosVal, PeriodosName)

####

## Crea funcion truncar a cero/minimo (util despues para precip)

tr_0 <- function(x, min){
  x[x < min] <- 0
  return(x)
}

tr_min <- function(x, min){
  x[x < min] <- min
  return(x)
}

## Crea funcion qdm para usar en precipitacion!
## por restriccion de schaake shuffle, todas las series son de igual largo

pr_lrBC <- function(obscal, modcal, modval, umbral){
  obscal <- tr_0(jitter(obscal, 0.1 * umbral), 0)
  modcal <- tr_0(jitter(modcal, 0.1 * umbral), 0)
  modval <- tr_0(jitter(modval, 0.1 * umbral), 0)
  boolpr <- modval > 0
  if (sum(boolpr) < 2){  # Con 0 o 1 dias con precip no se puede usar QDM
    res           <- tr_0(modval, umbral)
    res[! boolpr] <- 0
    return(res)
  } else{
    res.bc        <- QDM(obscal[boolpr], modcal[boolpr], modval[boolpr], ratio = T, trace = umbral)$mhat.p
    res           <- modval
    res[! boolpr] <- 0
    res[boolpr]   <- res.bc
    return(res)
  }
} 

####

## Correccion de sesgo
## Estratificada por mes

## genera series diarias de largo de todo el periodo de verificacion
## se generan tantas series como i_ens
## la primera iteración son los días más similares
## la segunda iteración son los segundos días más similares
## ... hasta N

## Precipitacion: modelo qdm
## Temperatura: modelo qdm
  
list_BC_val <- vector("list", length(PeriodosName)) %>% setNames(PeriodosName)
for (Per in PeriodosName){
  # Data por periodo de validacion
  df_obs_cal <- list_obs_cal[[Per]]
  df_obs_val <- list_obs_val[[Per]]
  df_e5_cal  <- list_e5_cal[[Per]]
  df_e5_val  <- list_e5_val[[Per]]
  ## Coordenadas
  Meses   <- df_obs_val$date %>% format("%m") %>% unique
  Agnos   <- df_obs_val$date %>% format("%Y") %>% unique
  Cuencas <- df_obs_val$cuenca %>% as.character %>% unique
  Vars    <- df_obs_val$variable %>% as.character %>% unique
  ## Correccion de sesgo estratificada por mes
  umbr   <- 0.1  # Umbral ceros precipitacion (mm)
  bc_val <- vector("list", length(Meses)) %>% setNames(Meses) %>% list %>% rep(length(Vars)) %>% setNames(Vars)
  BC_val <- vector("list", length(Meses)) %>% setNames(Meses)
  for (Mes in Meses){
    # Achica dataframes
    df_cal                <- data.table(df_e5_cal[df_e5_cal$date %>% format("%m") %in% Mes, ])
    df_val                <- data.table(df_e5_val[df_e5_val$date %>% format("%m") %in% Mes, ])
    obs_cal               <- data.table(df_obs_cal[df_obs_cal$date %>% format("%m") %in% Mes, ])
    # Correccion de sesgo en dias de periodo de validacion
    DiasVal               <- df_val$date %>% as.character %>% unique
    # Precipitacion
    pr_mod_cal            <- lapply(Cuencas, function(Cuenca)
      df_cal$value[df_cal$variable %in% "pr" & df_cal$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    pr_obs_cal            <- lapply(Cuencas, function(Cuenca)
      obs_cal$value[obs_cal$variable %in% "pr" & obs_cal$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    pr_mod_val            <- lapply(Cuencas, function(Cuenca)
      df_val$value[df_val$variable %in% "pr" & df_val$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    bc_val[["pr"]][[Mes]] <- lapply(Cuencas, function(Cuenca)
      pr_lrBC(pr_obs_cal[[Cuenca]], pr_mod_cal[[Cuenca]], pr_mod_val[[Cuenca]], umbr) %>%
        data.table(cuenca = Cuenca, date = as.Date(DiasVal), variable = "pr", value = .)) %>% do.call(rbind, .)
    # Temperatura
    tm_mod_cal            <- lapply(Cuencas, function(Cuenca)
      df_cal$value[df_cal$variable %in% "tm" & df_cal$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    tm_obs_cal            <- lapply(Cuencas, function(Cuenca)
      obs_cal$value[obs_cal$variable %in% "tm" & obs_cal$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    tm_mod_val            <- lapply(Cuencas, function(Cuenca)
      df_val$value[df_val$variable %in% "tm" & df_val$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    bc_val[["tm"]][[Mes]] <- lapply(Cuencas, function(Cuenca)
      QDM(tm_obs_cal[[Cuenca]], tm_mod_cal[[Cuenca]], tm_mod_val[[Cuenca]])$mhat.p %>%
        data.table(cuenca = Cuenca, date = as.Date(DiasVal), variable = "tm", value = .)) %>% do.call(rbind, .)
    # Consolida
    BC_val[[Mes]]         <- rbind(bc_val[["pr"]][[Mes]], bc_val[["tm"]][[Mes]])
    print(Per)
    print(Mes)
    print(Sys.time())
  }
  list_BC_val[[Per]] <- BC_val
  rm(Mes, df_cal, df_val, obs_cal, DiasVal, pr_mod_cal, pr_obs_cal, pr_mod_val, tm_mod_cal, tm_obs_cal, tm_mod_val,
     bc_val, BC_val, Per, df_obs_cal, df_obs_val, df_e5_cal, df_e5_val, Meses, Agnos, Cuencas, Vars)
  gc()
}

saveRDS(list_BC_val, "ResBC_VerifCons_Bench.RData")

####

