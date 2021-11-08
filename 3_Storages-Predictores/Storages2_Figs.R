####

setwd("~/Desktop/BP v2")

library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(magrittr)

rm(list = ls())

####

## Funcion agno hidrologico

wYear <- function(x){
  agno <- format(x, "%Y")
  mes  <- format(x, "%m")
  wy   <- ifelse(mes > "03", agno, as.character(as.numeric(agno) - 1))
  return(wy)
}

## Data

df_ensm <- readRDS("forzERA5BC_hist1979-ACTUAL_qdm-ss_n40.RData")  # forzante
df_sto  <- readRDS("SimIHC-Q_wys1981-2021_forzERA5BC.RData")  # storages

Meses   <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "01", "02", "03")
Cuencas <- c("Achibueno", "Ancoa", "Longavi", "Lontue", "Maule", "Melado")

## Truncar a fin de mes !

df_ensm <- filter(df_ensm, date <= as.Date("2021-10-31"))
df_sto  <- lapply(df_sto, function(df) filter(df, date <= as.Date("2021-10-31")))

####

## Forzante: Precip

df_pr <- df_ensm %>% filter(variable %in% "pr") %>% group_by(cuenca) %>% mutate(wy = wYear(date), mes = format(date, "%m")) %>%
  group_by(cuenca, wy, mes) %>% summarise(value = sum(value)) %>% ungroup %>% filter(wy %in% as.character(1988:2021))
df_pr$variable <- df_pr$wy
df_pr$cat      <- df_pr$wy
df_pr$wy       <- NULL
df_pr_m <- filter(df_pr, variable %in% as.character(1988:2020)) %>% group_by(cuenca, mes) %>% summarise(value = mean(value)) %>%
  ungroup %>% mutate(variable = "Promedio", cat = "Promedio 1988-2020")

dfgg_pr     <- rbind(df_pr, df_pr_m)
dfgg_pr$cat <- dfgg_pr$cat %in% c("2016", "2019", "2021", "Promedio 1988-2020") %>% ifelse(dfgg_pr$cat, "1988-2020")

dfgg_pr$mes <- factor(dfgg_pr$mes, levels = Meses)

dfgg_cumpr <- dfgg_pr %>% group_by(cuenca, variable) %>% arrange(mes) %>% mutate(cumval = cumsum(value)) %>%
  group_by(cuenca, variable, mes) %>% arrange(.by_group = T) %>% ungroup

for (Cuenca in Cuencas){
  ggplot()+
    geom_line(data = filter(dfgg_cumpr, cuenca %in% Cuenca),
              aes(x = mes, y = cumval, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    geom_line(data = filter(dfgg_cumpr, cuenca %in% Cuenca, ! cat %in% "1988-2020"),
              aes(x = mes, y = cumval, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    #facet_wrap(~cuenca)+
    scale_color_manual(name = "Año hidrológico",
                       values = c("grey","springgreen3","orange", "red", "black","blue"))+
    labs(title = paste0("Precipitación acumulada mensual", "\n",  "producto meteorológico 1988-2021"),
         x = "",
         y = "Precipitación (mm)")+
    #scale_x_date(date_breaks = "2 month", date_labels =  "%m")+ 
    scale_y_continuous(expand = c(0,0.01))+
    #theme_bw()+
    theme(legend.position="right")#+
    #guides(col=guide_legend(nrow=1,byrow=TRUE))
  
  save_filename= paste0("Predictores/precipcum-era5_mes_1988_2021_", Cuenca, ".png")
  ggsave(save_filename,width = 8,height = 3,units = "in",dpi=400)
}

## Forzante: Temp

df_tm <- df_ensm %>% filter(variable %in% "tm") %>% group_by(cuenca) %>% mutate(wy = wYear(date), mes = format(date, "%m")) %>%
  group_by(cuenca, wy, mes) %>% summarise(value = mean(value)) %>% ungroup %>% filter(wy %in% as.character(1988:2021))
df_tm$variable <- df_tm$wy
df_tm$cat      <- df_tm$wy
df_tm$wy       <- NULL
df_tm_m <- filter(df_tm, variable %in% as.character(1988:2020)) %>% group_by(cuenca, mes) %>% summarise(value = mean(value)) %>%
  ungroup %>% mutate(variable = "Promedio", cat = "Promedio 1988-2020")

dfgg_tm     <- rbind(df_tm, df_tm_m)
dfgg_tm$cat <- dfgg_tm$cat %in% c("2016", "2019", "2021", "Promedio 1988-2020") %>% ifelse(dfgg_tm$cat, "1988-2020")

dfgg_tm$mes <- factor(dfgg_tm$mes, levels = Meses)

for (Cuenca in Cuencas){
  ggplot()+
    geom_line(data = filter(dfgg_tm, cuenca %in% Cuenca),
              aes(x = mes, y = value, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    geom_line(data = filter(dfgg_tm, cuenca %in% Cuenca, ! cat %in% "1988-2020"),
              aes(x = mes, y = value, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    #facet_wrap(~cuenca)+
    scale_color_manual(name = "Año hidrológico",
                       values = c("grey","springgreen3","orange", "red", "black","blue"))+
    labs(title = paste0("Temperatura mensual", "\n",  "producto meteorológico 1988-2021"),
         x = "",
         y = "Temperatura promedio (°C)")+
    #scale_x_date(date_breaks = "2 month", date_labels =  "%m")+ 
    scale_y_continuous(expand = c(0,0.01))+
    #theme_bw()+
    theme(legend.position="right")#+
    #guides(col=guide_legend(nrow=1,byrow=TRUE))
  
  save_filename= paste0("Predictores/temperatura-era5_mes_1988_2021_", Cuenca, ".png")
  ggsave(save_filename,width = 8,height = 3,units = "in",dpi=400)
}

## Storages: SWE

# df_pred <- do.call(rbind, df_sto) %>%
#   mutate(value = SP + PROD + ROUT, date = date + 1, wy = wYear(date), mes = format(date, "%m"), dia = format(date, "%d")) %>%
#   filter(dia %in% "01" & wy %in% as.character(1988:2021)) %>% select(cuenca, mes, value, wy)
df_pred <- do.call(rbind, df_sto) %>% mutate(value = SP, wy = wYear(date), mes = format(date, "%m")) %>%
  group_by(cuenca, wy, mes) %>% summarise(value = mean(value)) %>% ungroup %>% filter(wy %in% as.character(1988:2021))

df_pred$variable <- df_pred$wy
df_pred$cat      <- df_pred$wy
df_pred$wy       <- NULL
df_pred_m <- filter(df_pred, variable %in% as.character(1988:2020)) %>% group_by(cuenca, mes) %>% summarise(value = mean(value)) %>%
  ungroup %>% mutate(variable = "Promedio", cat = "Promedio 1988-2020")

dfgg_pred     <- rbind(df_pred, df_pred_m)
dfgg_pred$cat <- dfgg_pred$cat %in% c("2016", "2019", "2021", "Promedio 1988-2020") %>% ifelse(dfgg_pred$cat, "1988-2020")

dfgg_pred$mes <- factor(dfgg_pred$mes, levels = Meses)

for (Cuenca in Cuencas){
  ggplot()+
    geom_line(data = filter(dfgg_pred, cuenca %in% Cuenca),
              aes(x = mes, y = value, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    geom_line(data = filter(dfgg_pred, cuenca %in% Cuenca, ! cat %in% "1988-2020"),
              aes(x = mes, y = value, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    #facet_wrap(~cuenca)+
    scale_color_manual(name = "Año hidrológico",
                       values = c("grey","springgreen3","orange", "red", "black","blue"))+
    labs(title = paste0("Nieve equivalente en agua", "\n",  "simulación hidrológica 1988-2021"),
         x = "",
         y = "Equivalente en agua (mm)")+
    #scale_x_date(date_breaks = "2 month", date_labels =  "%m")+ 
    scale_y_continuous(expand = c(0,0.01))+
    #theme_bw()+
    theme(legend.position="right")#+
  #guides(col=guide_legend(nrow=1,byrow=TRUE))
  
  save_filename= paste0("Predictores/swe_mes_1988_2021_", Cuenca, ".png")
  ggsave(save_filename,width = 8,height = 3,units = "in",dpi=400)
}

## Storages: PROD

# df_pred <- do.call(rbind, df_sto) %>%
#   mutate(value = SP + PROD + ROUT, date = date + 1, wy = wYear(date), mes = format(date, "%m"), dia = format(date, "%d")) %>%
#   filter(dia %in% "01" & wy %in% as.character(1988:2021)) %>% select(cuenca, mes, value, wy)
df_pred <- do.call(rbind, df_sto) %>% mutate(value = PROD, wy = wYear(date), mes = format(date, "%m")) %>%
  group_by(cuenca, wy, mes) %>% summarise(value = mean(value)) %>% ungroup %>% filter(wy %in% as.character(1988:2021))

df_pred$variable <- df_pred$wy
df_pred$cat      <- df_pred$wy
df_pred$wy       <- NULL
df_pred_m <- filter(df_pred, variable %in% as.character(1988:2020)) %>% group_by(cuenca, mes) %>% summarise(value = mean(value)) %>%
  ungroup %>% mutate(variable = "Promedio", cat = "Promedio 1988-2020")

dfgg_pred     <- rbind(df_pred, df_pred_m)
dfgg_pred$cat <- dfgg_pred$cat %in% c("2016", "2019", "2021", "Promedio 1988-2020") %>% ifelse(dfgg_pred$cat, "1988-2020")

dfgg_pred$mes <- factor(dfgg_pred$mes, levels = Meses)

for (Cuenca in Cuencas){
  ggplot()+
    geom_line(data = filter(dfgg_pred, cuenca %in% Cuenca),
              aes(x = mes, y = value, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    geom_line(data = filter(dfgg_pred, cuenca %in% Cuenca, ! cat %in% "1988-2020"),
              aes(x = mes, y = value, group = interaction(variable, cuenca), col = cat),
              size = 1)+
    #facet_wrap(~cuenca)+
    scale_color_manual(name = "Año hidrológico",
                       values = c("grey","springgreen3","orange", "red", "black","blue"))+
    labs(title = paste0("Humedad en el suelo", "\n",  "simulación hidrológica 1988-2021"),
         x = "",
         y = "Almacenamiento (mm)")+
    #scale_x_date(date_breaks = "2 month", date_labels =  "%m")+ 
    scale_y_continuous(expand = c(0,0.01))+
    #theme_bw()+
    theme(legend.position="right")#+
  #guides(col=guide_legend(nrow=1,byrow=TRUE))
  
  save_filename= paste0("Predictores/prod_mes_1988_2021_", Cuenca, ".png")
  ggsave(save_filename,width = 8,height = 3,units = "in",dpi=400)
}

####
