####

setwd("C:/Users/diego/Desktop/BP")

library(lubridate)
library(data.table)
library(dplyr)
library(reshape2)
#library(ggplot2)
#library(ggpubr)
library(bestNormalize)
library(magrittr)

rm(list = ls())

####

load("C:/Users/diego/Desktop/BP/ws_EscalaCuenca3_SeriesCuenca.RData")

## Sim Schaake Shuffle usa dias "similares" basado en dias anteriores de reanalisis
## Para esto se debe definir una metrica de similitud
## En lo siguiente se usan dos variables y cinco locaciones
## Tambien se compatibilizan los periodos recurrentes para ambas variables...

## Verificación tipo leave-5-out
## Bloques de 5 años periodo 1979-2018 (n = 40)
## E.g.,
## Periodo calibracion: 1979-2013 (n = 35)
## Periodo validacion: 2014-2018 (n = 5)
## Analisis de estratifican por mes!

## Elimina cuenca del Melado del analisis
## (Melado anidada a Maule)
coord_nona <- c("Achibueno", "Ancoa", "Longavi", "Lontue", "Maule")

agnos_val <- seq(from = 1979, to = 1983)
agnos_cal <- seq(from = 1979, to = 2018)[! seq(from = 1979, to = 2018) %in% agnos_val]

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

df_e5pr_val <- df_pr_e5[df_pr_e5$date %>% format("%Y") %in% agnos_val &
                          df_pr_e5$cuenca %in% coord_nona, ]
df_e5tm_val <- df_tm_e5[df_tm_e5$date %>% format("%Y") %in% agnos_val &
                          df_tm_e5$cuenca %in% coord_nona, ]
df_e5_val   <- cbind(df_e5pr_val, tm = df_e5tm_val$tm) %>% reshape2::melt(id = c("cuenca", "date"))

## Retiene solo algunas variables... 
rm(list = ls()[! ls() %in% c("df_obs_cal", "df_obs_val", "df_e5_cal", "df_e5_val")])

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
  print(Mes)
  print(Sys.time())
}
rm(Mes, Cuenca, bool_cal, bool_val, df_sca, sca.yj, sca.st)

####

## Funcion que busca dias similares/analogos
## "cal" es periodo de calibracion y "proy" es periodo de proyeccion
## sim.cal, simp.proy: son dataframes de cuatro columnas, despues de usar "melt"
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
Sys.time()
Agnos <- sc_e5_val$date %>% format("%Y") %>% unique
Meses <- sc_e5_val$date %>% format("%m") %>% unique
Res   <- vector("list", length(Meses)) %>% setNames(Meses) %>% list %>%
  rep(length(Agnos)) %>% setNames(Agnos)
for (Agno in Agnos){
  for (Mes in Meses){
    df_cal <- sc_e5_cal[sc_e5_cal$date %>% format("%m") %in% Mes, ]
    df_val <- sc_e5_val[sc_e5_val$date %>% format("%Y") %in% Agno &
                          sc_e5_val$date %>% format("%m") %in% Mes, ]
    Res[[Agno]][[Mes]] <- DiaSim(df_cal, df_val, N = 41)
    print(paste(Mes, Agno))
    print(Sys.time())
  }
}
rm(Mes, df_cal, df_val)
saveRDS(Res, "ResSimil_Cuencas_Val1979-1983.RData")

####

## Agrupa resultados (dias similares) por mes

Res_Mes <- Meses %>% lapply(function(Mes) Agnos %>% lapply(function(Agno)
  Res[[Agno]][[Mes]]) %>% unlist(recursive = F)) %>% setNames(Meses)
saveRDS(Res_Mes, "ResSimil_Cuencas_Val1979-1983.RData_mes.RData")

####
