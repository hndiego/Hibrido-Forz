####

setwd("/home/diego/Desktop/BP v2")

library(data.table)
library(dplyr)
library(reshape2)
library(magrittr)

rm(list = ls())

####

load("ws_EscalaCuenca3_SeriesCuenca.RData")

## Verificaci?n tipo leave-5-out
## Bloques de 5 a?os periodo 1979-2018 (n = 40)
## E.g.,
## Periodo calibracion: 1979-2013 (n = 35)
## Periodo validacion: 2014-2018 (n = 5)
## Analisis de estratifican por mes!

## Elimina cuenca del Melado del analisis
## (Melado anidada a Maule)
coord_nona <- c("Achibueno", "Ancoa", "Longavi", "Lontue", "Maule", "Melado")

PeriodosVal  <- list(c(1979, 1981), c(1982, 1984), c(1985, 1987), c(1988, 1990), c(1991, 1993), c(1994, 1996), c(1997, 1999),
                     c(2000, 2002), c(2003, 2005), c(2006, 2008), c(2009, 2011), c(2012, 2014), c(2015, 2017), c(2018, 2020))
PeriodosName <- sapply(PeriodosVal, function(per) paste(per[1], per[2], sep = "-"))
PeriodosVal  <- setNames(PeriodosVal, PeriodosName)

## Guarda data de cada periodo de validacion

list_obs_cal <- vector("list", length(PeriodosName)) %>% setNames(PeriodosName)
list_obs_val <- vector("list", length(PeriodosName)) %>% setNames(PeriodosName)
list_e5_cal  <- vector("list", length(PeriodosName)) %>% setNames(PeriodosName)
list_e5_val  <- vector("list", length(PeriodosName)) %>% setNames(PeriodosName)

for (Per in PeriodosName){
  agnos_val    <- seq(from = PeriodosVal[[Per]][1], to =  PeriodosVal[[Per]][2])
  agnos_cal    <- seq(from = 1979, to = 2020)[! seq(from = 1979, to = 2020) %in% agnos_val]
  
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
  
  df_e5pr_cal  <- df_pr_e5[df_pr_e5$date %>% format("%Y") %in% agnos_cal &
                             df_pr_e5$cuenca %in% coord_nona, ]
  df_e5tm_cal  <- df_tm_e5[df_tm_e5$date %>% format("%Y") %in% agnos_cal &
                             df_tm_e5$cuenca %in% coord_nona, ]
  df_e5_cal    <- cbind(df_e5pr_cal, tm = df_e5tm_cal$tm) %>% reshape2::melt(id = c("cuenca", "date"))
  
  df_e5pr_val  <- df_pr_e5[df_pr_e5$date %>% format("%Y") %in% agnos_val &
                             df_pr_e5$cuenca %in% coord_nona, ]
  df_e5tm_val  <- df_tm_e5[df_tm_e5$date %>% format("%Y") %in% agnos_val &
                             df_tm_e5$cuenca %in% coord_nona, ]
  df_e5_val    <- cbind(df_e5pr_val, tm = df_e5tm_val$tm) %>% reshape2::melt(id = c("cuenca", "date"))
  
  list_obs_cal[[Per]] <- df_obs_cal
  list_obs_val[[Per]] <- df_obs_val
  list_e5_cal[[Per]]  <- df_e5_cal
  list_e5_val[[Per]]  <- df_e5_val
}

## Retiene solo algunas variables... 
rm(list = ls()[! ls() %in% c("list_obs_cal", "list_obs_val", "list_e5_cal", "list_e5_val")])

####

## Da formato a resultados de dias similares
  
setwd("/home/diego/Desktop/BP v2/L3O 1979-2020")
Res <- list(readRDS("ResSimil_Cuencas_Val1979-1981.RData"), readRDS("ResSimil_Cuencas_Val1982-1984.RData"),
            readRDS("ResSimil_Cuencas_Val1985-1987.RData"), readRDS("ResSimil_Cuencas_Val1988-1990.RData"),
            readRDS("ResSimil_Cuencas_Val1991-1993.RData"), readRDS("ResSimil_Cuencas_Val1994-1996.RData"),
            readRDS("ResSimil_Cuencas_Val1997-1999.RData"), readRDS("ResSimil_Cuencas_Val2000-2002.RData"),
            readRDS("ResSimil_Cuencas_Val2003-2005.RData"), readRDS("ResSimil_Cuencas_Val2006-2008.RData"),
            readRDS("ResSimil_Cuencas_Val2009-2011.RData"), readRDS("ResSimil_Cuencas_Val2012-2014.RData"),
            readRDS("ResSimil_Cuencas_Val2015-2017.RData"), readRDS("ResSimil_Cuencas_Val2018-2020.RData")) %>%
  unlist(recursive = F)
setwd("/home/diego/Desktop/BP v2")

## Agrupa resultados de dias similares por mes

Agnos <- seq(from = 1979, to = 2020) %>% as.character
Meses <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

Res_Mes <- Meses %>% lapply(function(Mes) Agnos %>% lapply(function(Agno)
  Res[[Agno]][[Mes]]) %>% unlist(recursive = F)) %>% setNames(Meses)
saveRDS(Res_Mes, "ResSimil_Cuencas_ValConsolidado_Mes.RData")

rm(Res, Agnos, Meses)

####

save.image("ws_PreprocCuenca2_Data.RData")

####