####

setwd("~/Desktop/BP v2")

library(Hmisc)
library(data.table)
library(dplyr)
library(reshape2)
library(scales)
library(ggplot2)
library(ggpubr)
library(ECBC)
library(MBC)
library(crch)
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

# ## caso single value
# BC_uni     <- readRDS("ResBC_VerifCons_Bench.RData") %>% unlist(recursive = F)
# df_bc_uni  <- do.call(rbind, BC_uni) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
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

## caso solo qdm  

# BC_uni         <- readRDS("/home/diego/Desktop/BP/Preproceso 3.0/Test v2.0 QDM/ResBC_VerifCons_Bench_soloQDM.RData") %>% unlist(recursive = F)
# df_bc_uni_qdm  <- do.call(rbind, BC_uni) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
# 
# BC_ens         <- readRDS("/home/diego/Desktop/BP/Preproceso 3.0/Test v2.0 QDM/ResBC_VerifCons_Ens_soloQDM.RData")
# BC_ens         <- lapply(Meses, function(Mes)
#   lapply(PeriodosName, function(Per) BC_ens[[Per]][[Mes]]) %>% do.call(rbind, .)) %>% setNames(Meses)
# df_bc_ensm_qdm <- lapply(Meses, function(Mes)
#   BC_ens[[Mes]] %>% group_by(cuenca, date, variable) %>% summarise(value = mean(value)) %>% arrange(.by_group = T) %>%
#     ungroup) %>% do.call(rbind, .) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
# 
# rm(BC_uni, BC_ens)

####

## data consolidada

df          <- dcast(df_bc_ensm, variable + cuenca + date ~ n_ens) %>% cbind(CR2MET = df_obs_val$value) %>%
  melt(id = c("variable", "cuenca", "date", "CR2MET"), variable.name = "n_ens")
df          <- filter(df, variable %in% "pr")
df$variable <- NULL
df$season   <- ifelse(format(df$date, "%m") %in% c("04", "05"), "AM",
                      ifelse(format(df$date, "%m") %in% c("06", "07", "08"), "JJA",
                             ifelse(format(df$date, "%m") %in% c("09", "10", "11"), "SON", "DJFM")))

## calcular a b c d de la tabla de contingencia...

umbrales <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
df_umbr  <- vector("list", length(umbrales)) %>% setNames(umbrales)
for (umbr in umbrales){
  dfu <- df %>% group_by(season, cuenca, n_ens) %>%
    summarise(a = sum(CR2MET >= umbr & value >= umbr), b = sum(CR2MET < umbr & value >= umbr),
              c = sum(CR2MET >= umbr & value < umbr), d = sum(CR2MET < umbr & value < umbr))
  df_umbr[[as.character(umbr)]] <- cbind(dfu, umbral = rep(umbr, dim(dfu)[1]))
}
rm(dfu, umbr)
df_umbr <- do.call(rbind, df_umbr)

## calcular BR, F, PSS, CSS

Scores    <- c("BR", "F", "PSS", "CSS")
df_scores <- df_umbr %>% group_by(season, cuenca, umbral, n_ens) %>%
  summarise(BR = (a + b)  / (a + c),
            F = 1 - (b / (b + d)),
            PSS = (a * d - b * c) / ((a + c) * (b + d)),
            CSS = (a * d - b * c) / ((a + b) * (c + d))) %>% ungroup

## plotea!

Seasons            <- c("AM", "JJA", "SON", "DJFM")
df_scores$season   <- factor(df_scores$season, levels = Seasons)

df_scores <- melt(df_scores, id = c("season", "cuenca", "umbral", "n_ens"))

gg_sco <- vector("list", length(Scores)) %>% setNames(Scores) %>%
  list %>% rep(each = length(Seasons)) %>% setNames(Seasons)
for (Seas in Seasons){
  for (Sco in Scores){
    dfgg <- filter(df_scores, season %in% Seas & variable %in% Sco, n_ens %in% c(10, 20, 30, 40))
    gg_sco[[Seas]][[Sco]] <- ggplot(data = dfgg, aes(x = umbral, y = value, colour = n_ens, group = n_ens)) +
      theme_bw() + geom_hline(yintercept = 1, linetype = "dashed") + geom_line(alpha = .75, size = .75) +
      labs(x = "Umbral (mm)", y = NULL, colour = "N ensemble", title = paste(Sco, Seas, sep = "-")) +
      #scale_colour_manual(values = c("blue", "green", "darkgreen", "orange", "red")) +
      scale_colour_brewer(palette = "Spectral") +
      facet_wrap(~ cuenca, nrow = 2, ncol = 3, scales = "fixed") +
      theme(legend.position = "bottom", legend.text = element_text(size = rel(1)))
  }
  ggfname <- paste0("ggscores_1-40_", Seas, ".png")
  gg      <- ggarrange(plotlist = gg_sco[[Seas]], ncol = 2, nrow = 2, common.legend = T, legend = "bottom")
  ggsave(ggfname, gg, width = 16, height = 9, units = "in")
}
rm(Seas, Sco, dfgg, ggfname, gg)

####

## Data consolidada precip

df          <- dcast(df_bc_ensm, variable + cuenca + date ~ n_ens) %>% cbind(CR2MET = df_obs_val$value, ERA5 = df_e5_val$value) %>%
  melt(id = c("variable", "cuenca", "date", "CR2MET", "ERA5"), variable.name = "n_ens")
df          <- filter(df, variable %in% "pr")
df$variable <- NULL
df$mes      <- format(df$date, "%m")

## Crea df de average-rmse y average-spread
## RMSE por cuenca!

df_rmse <- dcast(df, cuenca + mes + date + CR2MET + ERA5 ~ n_ens) %>%
  melt(id = c("cuenca", "mes", "date", "CR2MET"))

df_rmse <- df_rmse %>% group_by(cuenca, mes, variable) %>% summarise(rmse = rmse(value, CR2MET)) %>% ungroup

## Plotea por variable y por cuenca

df_rmse$mes <- factor(df_rmse$mes, levels = c(Meses[4:12], Meses[1:3]))

## Plotea!
gg_pr <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
#maxv  <- max(df_rmse$rmse)
maxv  <- max(filter(df_rmse, ! variable %in% "ERA5")$rmse)
for (Cuenca in Cuencas){
  df_error  <- filter(df_rmse, cuenca %in% Cuenca, ! variable %in% "ERA5")
  #df_e5     <- filter(df_rmse, cuenca %in% Cuenca, variable %in% "ERA5")
  gg_pr[[Cuenca]] <- ggplot() + theme_bw() + coord_cartesian(ylim = c(0, maxv)) +
    #geom_line(data = df_e5, aes(x = mes, y = rmse, group = variable), colour = "black") +
    geom_line(data = df_error, aes(x = mes, y = rmse, colour = variable, group = variable)) +
    scale_colour_brewer(palette = "Spectral") +
    labs(title = Cuenca, x = NULL, y = "RMSE (mm/día)", colour = "N ensemble") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.text = element_text(size = rel(1)))
}
rm(Cuenca, df_error, df_e5, maxv)
gg_pr_grid <- ggarrange(plotlist = gg_pr, nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
ggsave("grid_rmse-pr_N-emsemble.png", gg_pr_grid, width = 4 *2, height = 2.5 *3, units = "in")
rm(gg_pr, gg_pr_grid)

## Data consolidada temp

df          <- dcast(df_bc_ensm, variable + cuenca + date ~ n_ens) %>% cbind(CR2MET = df_obs_val$value, ERA5 = df_e5_val$value) %>%
  melt(id = c("variable", "cuenca", "date", "CR2MET", "ERA5"), variable.name = "n_ens")
df          <- filter(df, variable %in% "tm")
df$variable <- NULL
df$mes      <- format(df$date, "%m")

## Crea df de average-rmse y average-spread
## RMSE por cuenca!

df_rmse <- dcast(df, cuenca + mes + date + CR2MET + ERA5 ~ n_ens) %>%
  melt(id = c("cuenca", "mes", "date", "CR2MET"))

df_rmse <- df_rmse %>% group_by(cuenca, mes, variable) %>% summarise(rmse = rmse(value, CR2MET)) %>% ungroup

## Plotea por variable y por cuenca

df_rmse$mes <- factor(df_rmse$mes, levels = c(Meses[4:12], Meses[1:3]))

## Plotea!
gg_pr <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
#maxv  <- max(df_rmse$rmse)
maxv  <- max(filter(df_rmse, ! variable %in% "ERA5")$rmse)
for (Cuenca in Cuencas){
  df_error  <- filter(df_rmse, cuenca %in% Cuenca, ! variable %in% "ERA5")
  #df_e5     <- filter(df_rmse, cuenca %in% Cuenca, variable %in% "ERA5")
  gg_pr[[Cuenca]] <- ggplot() + theme_bw() + coord_cartesian(ylim = c(0, maxv)) +
    #geom_line(data = df_e5, aes(x = mes, y = rmse, group = variable), colour = "black") +
    geom_line(data = df_error, aes(x = mes, y = rmse, colour = variable, group = variable)) +
    scale_colour_brewer(palette = "Spectral") +
    labs(title = Cuenca, x = NULL, y = "RMSE (°C)", colour = "N ensemble") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.text = element_text(size = rel(1)))
}
rm(Cuenca, df_error, df_e5, maxv)
gg_pr_grid <- ggarrange(plotlist = gg_pr, nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
ggsave("grid_rmse-tm_N-emsemble.png", gg_pr_grid, width = 4 *2, height = 2.5 *3, units = "in")
rm(gg_pr, gg_pr_grid)
