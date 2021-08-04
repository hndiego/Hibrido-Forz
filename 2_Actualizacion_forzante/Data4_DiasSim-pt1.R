####

setwd("~/Desktop/BP v2")

library(lubridate)
library(data.table)
library(dplyr)
library(reshape2)
library(bestNormalize)
library(magrittr)

rm(list = ls())

####

## SimSchaake usa dias "similares" basado en dias anteriores de reanalisis
## Para esto se debe definir una metrica de similitud
## En lo siguiente se usan dos variables (pr, tm) y cinco locaciones (cuencas no anidadas)
## Tambien se compatibilizan los periodos recurrentes para ambas variables...

## Verificacion tipo leave-3-out
## Bloques de 3 agnos periodo 1979-2020 (n = 42)
## Analisis de estratifican por mes!

PeriodosVal  <- list(c(1979, 1981), c(1982, 1984), c(1985, 1987), c(1988, 1990), c(1991, 1993), c(1994, 1996), c(1997, 1999),
                     c(2000, 2002), c(2003, 2005), c(2006, 2008), c(2009, 2011), c(2012, 2014), c(2015, 2017), c(2018, 2020))
PeriodosName <- sapply(PeriodosVal, function(per) paste(per[1], per[2], sep = "-"))
PeriodosVal  <- setNames(PeriodosVal, PeriodosName)

#for (Per in PeriodosName[1]){  
Per <- PeriodosName[1] # Para validar las series "actuales" solo se usa el periodo mas antiguo...

## Elimina cuenca del Melado del analisis
## Melado anidada a Maule, podria distorsionar la busqueda de similares
coord_nona <- c("Achibueno", "Ancoa", "Longavi", "Lontue", "Maule")

load("ws_EscalaCuenca3_SeriesCuenca.RData")
load("era5 actual/actual_era5.RData")

agnos_val <- seq(from = PeriodosVal[[Per]][1], to =  PeriodosVal[[Per]][2])
agnos_cal <- seq(from = 1979, to = 2020)[! seq(from = 1979, to = 2020) %in% agnos_val]

df_obspr_cal <- df_pr_cr2[df_pr_cr2$date %>% format("%Y") %in% agnos_cal &
                            df_pr_cr2$cuenca %in% coord_nona, ]
df_obstm_cal <- df_tm_cr2[df_tm_cr2$date %>% format("%Y") %in% agnos_cal &
                            df_tm_cr2$cuenca %in% coord_nona, ]
df_obs_cal   <- cbind(df_obspr_cal, tm = df_obstm_cal$tm) %>% reshape2::melt(id = c("cuenca", "date"))

df_obspr_val <- df_pr_cr2[df_pr_cr2$date %>% format("%Y") %in% agnos_val &
                            df_pr_cr2$cuenca %in% coord_nona, ]
df_obstm_val <- df_tm_cr2[df_tm_cr2$date %>% format("%Y") %in% agnos_val &
                            df_tm_cr2$cuenca %in% coord_nona, ]
df_obs_val   <- cbind(df_obspr_val, tm = df_obstm_val$tm) %>% reshape2::melt(id = c("cuenca", "date"))

df_e5pr_cal <- df_pr_e5[df_pr_e5$date %>% format("%Y") %in% agnos_cal &
                          df_pr_e5$cuenca %in% coord_nona, ]
df_e5tm_cal <- df_tm_e5[df_tm_e5$date %>% format("%Y") %in% agnos_cal &
                          df_tm_e5$cuenca %in% coord_nona, ]
df_e5_cal   <- cbind(df_e5pr_cal, tm = df_e5tm_cal$tm) %>% reshape2::melt(id = c("cuenca", "date"))

df_e5pr_val <- actual_pr[actual_pr$cuenca %in% coord_nona, ]
df_e5tm_val <- actual_tm[actual_tm$cuenca %in% coord_nona, ]
df_e5_val   <- cbind(df_e5pr_val, tm = df_e5tm_val$tm) %>% reshape2::melt(id = c("cuenca", "date"))

## Retiene solo algunas variables... 

rm(df_obspr_cal, df_obstm_cal, df_obspr_val, df_obstm_val, df_e5pr_cal, df_e5tm_cal, df_e5pr_val, df_e5tm_val,
   coord_nona, agnos_cal, df_pr_cr2, df_pr_e5, df_tm_cr2, df_tm_e5, cr2_lat, cr2_lon, cr2_t, cr2_tm, cr2u_pr,
   e5_lat, e5_lon, e5_pr, e5_tm, e5pr_t, e5tm_t)

####

## Estandariza valores para comparaciones inter-variable

sc_e5_cal  <- df_e5_cal
sc_e5_val  <- df_e5_val

Agnos   <- unique(sc_e5_val$date %>% format("%Y"))
Meses   <- unique(sc_e5_val$date %>% format("%m"))
Vars    <- unique(sc_e5_val$variable)
Cuencas <- unique(sc_e5_val$cuenca)

for (Mes in Meses){
  for (Cuenca in Cuencas){
    # Caso precip. Transforma a Yeo-Johnson y luego estandariza
    bool_cal <- df_obs_cal$cuenca %in% Cuenca & format(df_obs_cal$date, "%m") %in% Mes & df_obs_cal$variable %in% "pr"
    df_sca   <- sqrt(df_obs_cal$value[bool_cal])
    sca.yj   <- yeojohnson(df_sca[df_sca != 0], standardize = T)
    sc_e5_cal$value[bool_cal] <- predict(sca.yj, sqrt(sc_e5_cal$value[bool_cal]))
    bool_val <- df_e5_val$cuenca %in% Cuenca & format(df_e5_val$date, "%m") %in% Mes & df_e5_val$variable %in% "pr"
    sc_e5_val$value[bool_val] <- predict(sca.yj, sqrt(sc_e5_val$value[bool_val]))
    # Caso temperatura. Estandariza
    bool_cal <- df_obs_cal$cuenca %in% Cuenca & format(df_obs_cal$date, "%m") %in% Mes & df_obs_cal$variable %in% "tm"
    df_sca   <- df_obs_cal$value[bool_cal]
    sca.st   <- yeojohnson(df_sca, standardize = T)
    sc_e5_cal$value[bool_cal] <- predict(sca.st, sc_e5_cal$value[bool_cal])
    bool_val <- df_e5_val$cuenca %in% Cuenca & format(df_e5_val$date, "%m") %in% Mes & df_e5_val$variable %in% "tm"
    sc_e5_val$value[bool_val] <- predict(sca.st, sc_e5_val$value[bool_val])
  }
}
rm(Mes, Cuenca, bool_cal, bool_val, df_sca, sca.yj, sca.st)

####

## Funcion que busca dias similares/analogos
## "cal" es periodo de calibracion y "proy" es periodo de proyeccion
## cal, proy: son dataframes de cuatro columnas, despues de usar "melt"
## df variables: coord (factor), date (Date), variable (factor), value (num)
## N: es numero de dias similares retenidos
## Inputs ya estratificados por mes!

DiaSim <- function(sim.cal, sim.proy, N){
  cal.cuenca  <- as.character(sim.cal$cuenca)
  cal.date    <- as.character(sim.cal$date)
  cal.var     <- as.character(sim.cal$variable)
  cal.val     <- sim.cal$value
  proy.cuenca <- as.character(sim.proy$cuenca)
  proy.date   <- as.character(sim.proy$date)
  proy.var    <- as.character(sim.proy$variable)
  proy.val    <- sim.proy$value
  Dias        <- unique(proy.date)
  Vars        <- unique(proy.var)
  Cuencas     <- unique(proy.cuenca)
  Anas        <- unique(cal.date)
  Sim         <- lapply(Dias, function(Dia) sapply(Anas, function(Ana)
    lapply(Vars, function(Var) lapply(Cuencas, function(Cuenca)
      (proy.val[proy.cuenca %in% Cuenca & proy.date %in% Dia & proy.var %in% Var] -
         cal.val[cal.cuenca %in% Cuenca & cal.date %in% Ana & cal.var %in% Var]) ^ 2)) %>%
      unlist(recursive = T) %>% mean %>% sqrt, USE.NAMES = F) %>% data.table(date = Anas, simil = .) %>%
      arrange(simil) %>% .[1:N, ]) %>% setNames(Dias)
  return(Sim)
}

####
  
gc()
print(Sys.time())
Agnos <- sc_e5_val$date %>% format("%Y") %>% unique
Meses <- sc_e5_val$date %>% format("%m") %>% unique
Res   <- vector("list", length(Meses)) %>% setNames(Meses) %>% list %>%
  rep(length(Agnos)) %>% setNames(Agnos)
N     <- 40
for (Agno in Agnos){
  if (Agno %in% "2021"){
    for (Mes in c("04", "05", "06", "07", "08", "09", "10", "11", "12")){  # verificar
      df_cal <- sc_e5_cal[sc_e5_cal$date %>% format("%m") %in% Mes, ]
      df_val <- sc_e5_val[sc_e5_val$date %>% format("%Y") %in% Agno &
                            sc_e5_val$date %>% format("%m") %in% Mes, ]
      Res[[Agno]][[Mes]] <- DiaSim(df_cal, df_val, N = N)
      print(paste(Mes, Agno))
      print(Sys.time())
    }
  } else {
    for (Mes in Meses){
      df_cal <- sc_e5_cal[sc_e5_cal$date %>% format("%m") %in% Mes, ]
      df_val <- sc_e5_val[sc_e5_val$date %>% format("%Y") %in% Agno &
                            sc_e5_val$date %>% format("%m") %in% Mes, ]
      Res[[Agno]][[Mes]] <- DiaSim(df_cal, df_val, N = N)
      print(paste(Mes, Agno))
      print(Sys.time())
    }
  }
}
namef <- paste0("era5 actual/ResSimil_Cuencas_Val_Actualizacion.RData")
saveRDS(Res, namef)
rm(Agno, Mes, df_cal, df_val, namef, Res)
#}

####        
