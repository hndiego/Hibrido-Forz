####
  
setwd("~/Desktop/BP v2")

library(data.table)
library(dplyr)
library(reshape2)
library(ECBC)
library(MBC)
library(crch)
library(hydroGOF)
library(magrittr)

rm(list = ls())

####

## Data cr2met y era5
## Consolidado verificacion cruzada! deja-3-fuera 

load("ws_PreprocCuenca2_Data.RData")

PeriodosVal  <- list(c(1979, 1981), c(1982, 1984), c(1985, 1987), c(1988, 1990), c(1991, 1993), c(1994, 1996), c(1997, 1999),
                     c(2000, 2002), c(2003, 2005), c(2006, 2008), c(2009, 2011), c(2012, 2014), c(2015, 2017), c(2018, 2020))
PeriodosName <- sapply(PeriodosVal, function(per) paste(per[1], per[2], sep = "-"))
PeriodosVal  <- PeriodosVal %>% setNames(PeriodosName)

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
  if (sum(boolpr) < 2){  # Con 0 o 1 dias con precip no se puede usar QDM...
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

## ITERA PARA CADA PERIODO DE VALIDACION

## Correccion de sesgo
## Estratificada por mes

## genera series diarias de largo de todo el periodo de verificacion
## se generan tantas series como i_ens
## la primera iteracion son los dias mas similares
## la segunda iteracion son los segundos dias mas similares
## ... hasta N

## Precipitacion: modelo qdm + ss
## Temperatura: modelo qdm + ss

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
  N         <- 40  # MIEMBROS DE ENSEMBLE: POR ANALISIS EXPLORATORIOS SE CONCLUYE N=40
  umbr      <- 0.1  # Umbral ceros precipitacion (mm)
  bc_val    <- vector("list", length(Meses)) %>% setNames(Meses) %>% list %>% rep(length(Vars)) %>% setNames(Vars)
  bc_val_ss <- vector("list", length(Meses)) %>% setNames(Meses) %>% list %>% rep(length(Vars)) %>% setNames(Vars)
  BC_val    <- vector("list", length(Meses)) %>% setNames(Meses)
  for (Mes in Meses){
    # Achica dataframes
    df_cal                <- data.table(df_e5_cal[df_e5_cal$date %>% format("%m") %in% Mes, ])
    df_val                <- data.table(df_e5_val[df_e5_val$date %>% format("%m") %in% Mes, ])
    obs_cal               <- data.table(df_obs_cal[df_obs_cal$date %>% format("%m") %in% Mes, ])
    # Correccion de sesgo en dias de periodo de validacion
    DiasVal               <- df_val$date %>% as.character %>% unique
    DiasSim               <- lapply(1:N, function(i_ens) sapply(DiasVal, function(DiaVal)
      Res_Mes[[Mes]][[DiaVal]]$date[i_ens]) %>% setNames(DiasVal)) %>% setNames(1:N)
    # Precipitacion
    pr_mod_cal            <- lapply(1:N, function(i_ens) lapply(Cuencas, function(Cuenca)
      df_cal[df_cal$variable %in% "pr" & df_cal$cuenca %in% Cuenca, ] %>%
        right_join(data.table(date = as.Date(DiasSim[[i_ens]])), by = "date") %$% value) %>% setNames(Cuencas)) %>% setNames(1:N)
    pr_obs_cal            <- lapply(1:N, function(i_ens) lapply(Cuencas, function(Cuenca)
      obs_cal[obs_cal$variable %in% "pr" & obs_cal$cuenca %in% Cuenca, ] %>%
        right_join(data.table(date = as.Date(DiasSim[[i_ens]])), by = "date") %$% value) %>% setNames(Cuencas)) %>% setNames(1:N)
    pr_mod_val            <- lapply(Cuencas, function(Cuenca)
      df_val$value[df_val$variable %in% "pr" & df_val$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    bc_val[["pr"]][[Mes]] <- lapply(1:N, function(i_ens) lapply(Cuencas, function(Cuenca)
      pr_lrBC(pr_obs_cal[[i_ens]][[Cuenca]], pr_mod_cal[[i_ens]][[Cuenca]], pr_mod_val[[Cuenca]], umbr)) %>% setNames(Cuencas))
    # Temperatura
    tm_mod_cal            <- lapply(1:N, function(i_ens) lapply(Cuencas, function(Cuenca)
      df_cal[df_cal$variable %in% "tm" & df_cal$cuenca %in% Cuenca, ] %>%
        right_join(data.table(date = as.Date(DiasSim[[i_ens]])), by = "date") %$% value) %>% setNames(Cuencas)) %>% setNames(1:N)
    tm_obs_cal            <- lapply(1:N, function(i_ens) lapply(Cuencas, function(Cuenca)
      obs_cal[obs_cal$variable %in% "tm" & obs_cal$cuenca %in% Cuenca, ] %>%
        right_join(data.table(date = as.Date(DiasSim[[i_ens]])), by = "date") %$% value) %>% setNames(Cuencas)) %>% setNames(1:N)
    tm_mod_val            <- lapply(Cuencas, function(Cuenca)
      df_val$value[df_val$variable %in% "tm" & df_val$cuenca %in% Cuenca]) %>% setNames(Cuencas)
    bc_val[["tm"]][[Mes]] <- lapply(1:N, function(i_ens) lapply(Cuencas, function(Cuenca)
      QDM(tm_obs_cal[[i_ens]][[Cuenca]], tm_mod_cal[[i_ens]][[Cuenca]], tm_mod_val[[Cuenca]])$mhat.p) %>% setNames(Cuencas))
    # Schaake Shuffle
    bc_val_ss[["pr"]][[Mes]] <- lapply(Cuencas, function(Cuenca) lapply(seq_along(DiasVal), function(iDia)
      sapply(1:N, function(i_ens) pr_obs_cal[[i_ens]][[Cuenca]][iDia]) %>%
        Schaake.Shuffle(sapply(1:N, function(i_ens) bc_val[["pr"]][[Mes]][[i_ens]][[Cuenca]][[iDia]])) %>%
        data.table(cuenca = Cuenca, date = as.Date(DiasVal[iDia]), variable = "pr", value = ., i_ens = 1:N)) %>%
        do.call(rbind, .)) %>% do.call(rbind, .) %>% group_by(i_ens, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
    bc_val_ss[["tm"]][[Mes]] <- lapply(Cuencas, function(Cuenca) lapply(seq_along(DiasVal), function(iDia)
      sapply(1:N, function(i_ens) tm_obs_cal[[i_ens]][[Cuenca]][iDia]) %>%
        Schaake.Shuffle(sapply(1:N, function(i_ens) bc_val[["tm"]][[Mes]][[i_ens]][[Cuenca]][[iDia]])) %>%
        data.table(cuenca = Cuenca, date = as.Date(DiasVal[iDia]), variable = "tm", value = ., i_ens = 1:N)) %>%
        do.call(rbind, .)) %>% do.call(rbind, .) %>% group_by(i_ens, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
    # Consolida
    BC_val[[Mes]]         <- rbind(bc_val_ss[["pr"]][[Mes]], bc_val_ss[["tm"]][[Mes]])
    print(Per)
    print(Mes)
    print(Sys.time())
  }
  list_BC_val[[Per]] <- BC_val
  rm(Mes, df_cal, df_val, obs_cal, DiasVal, DiasSim, pr_mod_cal, pr_obs_cal, pr_mod_val, tm_mod_cal, tm_obs_cal, tm_mod_val,
     bc_val, bc_val_ss, BC_val, Per, df_obs_cal, df_obs_val, df_e5_cal, df_e5_val, Meses, Agnos, Cuencas, Vars)
  gc()
}

saveRDS(list_BC_val, "ResBC_VerifCons_Ens.RData")

####
