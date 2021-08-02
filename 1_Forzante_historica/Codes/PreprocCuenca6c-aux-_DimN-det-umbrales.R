####

setwd("~/Desktop/BP v2")

library(Hmisc)
library(data.table)
library(dplyr)
library(reshape2)
library(scales)
library(ggplot2)
library(ggpubr)
library(hydroGOF)
library(EnvStats)
library(magrittr)

rm(list = ls())

####

## Data cr2met y era5
## Consolidado verificacion cruzada! deja-5-fuera 

load("ws_PreprocCuenca2_Data.RData")
rm(Res_Mes)

PeriodosVal  <- list(c(1979, 1981), c(1982, 1984), c(1985, 1987), c(1988, 1990), c(1991, 1993), c(1994, 1996), c(1997, 1999),
                     c(2000, 2002), c(2003, 2005), c(2006, 2008), c(2009, 2011), c(2012, 2014), c(2015, 2017), c(2018, 2020))
PeriodosName <- sapply(PeriodosVal, function(per) paste(per[1], per[2], sep = "-"))
PeriodosVal  <- setNames(PeriodosVal, PeriodosName)

df_obs_val   <- do.call(rbind, list_obs_val) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
df_e5_val    <- do.call(rbind, list_e5_val) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup

## Coordenadas

Meses   <- df_obs_val$date %>% format("%m") %>% unique
Agnos   <- df_obs_val$date %>% format("%Y") %>% unique
Cuencas <- df_obs_val$cuenca %>% as.character %>% unique
Vars    <- df_obs_val$variable %>% as.character %>% unique

####

## Series:
## cr2met (referencia)
## era5 (raw)
## chcr + qdm (BC single)
## chcr + qdm + ss (BC ensemble)

## caso ensemble
BC_ens     <- readRDS("ResBC_VerifCons_Ens.RData")
BC_ens     <- lapply(Meses, function(Mes)
  lapply(PeriodosName, function(Per) BC_ens[[Per]][[Mes]]) %>% do.call(rbind, .)) %>% setNames(Meses)
## solo algunos miembros ensemble
n          <- seq(from = 10, to = 100, by = 10)
## promedio ens
df_bc_ensm <- lapply(n, function(nens) lapply(Meses, function(Mes)
  BC_ens[[Mes]] %>% filter(i_ens <= nens) %>% group_by(cuenca, date, variable) %>% summarise(value = mean(value), n_ens = nens)
  %>% arrange(.by_group = T) %>% ungroup) %>% do.call(rbind, .)) %>% do.call(rbind, .) %>% group_by(variable, cuenca, date, n_ens) %>%
  arrange(.by_group = T) %>% ungroup
# df_bc_ENS  <- Meses %>% lapply(function(Mes) BC_ens[[Mes]]) %>% do.call(rbind, .) %>%
#   group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
rm(BC_ens, n)

####

## Distintos umbrales precip: 0, 2, 10, 20, 30

Umbrales <- c(NA, 0, 2, 10, 20, 30)

for (Umbral in Umbrales){
  ## Data consolidada precip
  df          <- dcast(df_bc_ensm, variable + cuenca + date ~ n_ens) %>% cbind(CR2MET = df_obs_val$value, ERA5 = df_e5_val$value) %>%
    melt(id = c("variable", "cuenca", "date", "CR2MET", "ERA5"), variable.name = "n_ens")
  df          <- filter(df, variable %in% "pr")
  df$variable <- NULL
  df$mes      <- format(df$date, "%m")
  ## Filtra por umbral
  #titl <- "Precipitación: datos completos"
  fnam <- "gg-Nensemble_rmse-pr_todo.png"
  if (! Umbral %in% NA){
    df <- filter(df, CR2MET > Umbral)
    #titl <- paste0("Precipitación: caso > ", Umbral, " (mm)")
    fnam <- paste0("gg-Nensemble_rmse-pr_umbral-", Umbral, ".png")
  }
  ## Crea df de average-rmse y average-spread
  ## RMSE por cuenca!
  df_rmse <- dcast(df, cuenca + mes + date + CR2MET + ERA5 ~ n_ens) %>%
    melt(id = c("cuenca", "mes", "date", "CR2MET"))
  df_rmse <- df_rmse %>% group_by(cuenca, mes, variable) %>% summarise(rmse = rmse(value, CR2MET)) %>% ungroup
  ## Plotea por variable y por cuenca
  df_rmse$mes <- factor(df_rmse$mes, levels = c(Meses[4:12], Meses[1:3]))
  ## Plotea!
  gg_pr <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
  maxv  <- max(df_rmse$rmse)
  #maxv  <- max(filter(df_rmse, ! variable %in% "ERA5")$rmse)
  for (Cuenca in Cuencas){
    df_error  <- filter(df_rmse, cuenca %in% Cuenca, ! variable %in% "ERA5")
    df_e5     <- filter(df_rmse, cuenca %in% Cuenca, variable %in% "ERA5")
    gg_pr[[Cuenca]] <- ggplot() + theme_bw() + coord_cartesian(ylim = c(0, maxv)) +
      geom_line(data = df_e5, aes(x = mes, y = rmse, group = variable), colour = "black") +
      geom_line(data = df_error, aes(x = mes, y = rmse, colour = variable, group = variable)) +
      scale_colour_brewer(palette = "Spectral") +
      labs(title = Cuenca, x = NULL, y = "RMSE (mm/día)", colour = "N ensemble") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme(legend.text = element_text(size = rel(1)))
  }
  rm(Cuenca, df_error, df_e5, maxv)
  gg_pr_grid <- ggarrange(plotlist = gg_pr, nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
  ggsave(fnam, gg_pr_grid, width = 4 *2, height = 2.5 *3, units = "in")
  rm(gg_pr, gg_pr_grid)
}

####