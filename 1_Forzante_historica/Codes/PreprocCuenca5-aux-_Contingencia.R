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
## Consolidado verificacion cruzada! deja-3-fuera 

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

## caso single value
BC_uni     <- readRDS("ResBC_VerifCons_Bench.RData") %>% unlist(recursive = F)
df_bc_uni  <- do.call(rbind, BC_uni) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
## caso ensemble
BC_ens     <- readRDS("ResBC_VerifCons_Ens.RData")
BC_ens     <- lapply(Meses, function(Mes)
  lapply(PeriodosName, function(Per) BC_ens[[Per]][[Mes]]) %>% do.call(rbind, .)) %>% setNames(Meses)
## solo algunos miembros ensemble (primer orden...)
n          <- seq_len(40)
BC_ens     <- lapply(BC_ens, function(elem) filter(elem, i_ens %in% n))
rm(n)
## promedio ens
df_bc_ensm <- lapply(Meses, function(Mes)
  BC_ens[[Mes]] %>% group_by(cuenca, date, variable) %>% summarise(value = mean(value)) %>% arrange(.by_group = T) %>%
    ungroup) %>% do.call(rbind, .) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
# df_bc_ENS  <- Meses %>% lapply(function(Mes) BC_ens[[Mes]]) %>% do.call(rbind, .) %>%
#   group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
rm(BC_uni, BC_ens)

####

## data consolidada

df          <- cbind(df_bc_ensm[,-4], obs = df_obs_val$value, "ERA5" = df_e5_val$value,
                     "QDM" = df_bc_uni$value, "QDM ens" = df_bc_ensm$value)
df          <- filter(df, variable %in% "pr")
df$variable <- NULL
df$season   <- ifelse(format(df$date, "%m") %in% c("04", "05"), "AM",
                      ifelse(format(df$date, "%m") %in% c("06", "07", "08"), "JJA",
                             ifelse(format(df$date, "%m") %in% c("09", "10", "11"), "SON", "DJFM")))
df          <- melt(df, id = c("cuenca", "season", "date", "obs"))

## calcular a b c d de la tabla de contingencia...

umbrales <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
df_umbr  <- vector("list", length(umbrales)) %>% setNames(umbrales)
for (umbr in umbrales){
  dfu <- df %>% group_by(season, cuenca, variable) %>%
    summarise(a = sum(obs >= umbr & value >= umbr), b = sum(obs < umbr & value >= umbr),
              c = sum(obs >= umbr & value < umbr), d = sum(obs < umbr & value < umbr))
  df_umbr[[as.character(umbr)]] <- cbind(dfu, umbral = rep(umbr, dim(dfu)[1]))
}
rm(dfu, umbr)
df_umbr <- do.call(rbind, df_umbr)

## calcular BR, F, PSS, CSS

Scores    <- c("BR", "F", "PSS", "CSS")
df_scores <- df_umbr %>% group_by(season, cuenca, umbral, variable) %>%
  summarise(BR = (a + b)  / (a + c),
            F = 1 - (b / (b + d)),
            PSS = (a * d - b * c) / ((a + c) * (b + d)),
            CSS = (a * d - b * c) / ((a + b) * (c + d))) %>% ungroup

## plotea!

Seasons            <- c("AM", "JJA", "SON", "DJFM")
df_scores$season   <- factor(df_scores$season, levels = Seasons)
df_scores$prepro   <- df_scores$variable
df_scores$variable <- NULL

df_scores <- melt(df_scores, id = c("season", "cuenca", "umbral", "prepro"))

gg_sco <- vector("list", length(Scores)) %>% setNames(Scores) %>%
  list %>% rep(each = length(Seasons)) %>% setNames(Seasons)
for (Seas in Seasons){
  for (Sco in Scores){
    dfgg <- filter(df_scores, season %in% Seas & variable %in% Sco)
    gg_sco[[Seas]][[Sco]] <- ggplot(data = dfgg, aes(x = umbral, y = value, colour = prepro, group = prepro)) +
      theme_bw() + geom_hline(yintercept = 1, linetype = "dashed") + geom_line(alpha = .5, size = .75) +
      labs(x = "Umbral (mm)", y = NULL, colour = "Preproceso", title = paste(Sco, Seas, sep = "-")) +
      scale_colour_manual(values = c("blue", "green", "darkgreen", "orange", "red")) +
      facet_wrap(~ cuenca, nrow = 2, ncol = 3, scales = "fixed") +
      theme(legend.position = "bottom", legend.text = element_text(size = rel(1)))
  }
  ggfname <- paste0("ggscores_", Seas, ".png")
  gg      <- ggarrange(plotlist = gg_sco[[Seas]], ncol = 2, nrow = 2, common.legend = T, legend = "bottom")
  ggsave(ggfname, gg, width = 16, height = 9, units = "in")
}
rm(Seas, Sco, dfgg, ggfname, gg)

####

## histogramas!

dfgg          <- filter(df_obs_val, variable %in% "pr")
dfgg$variable <- NULL
dfgg$season   <- ifelse(format(dfgg$date, "%m") %in% c("04", "05"), "AM",
                      ifelse(format(dfgg$date, "%m") %in% c("06", "07", "08"), "JJA",
                             ifelse(format(dfgg$date, "%m") %in% c("09", "10", "11"), "SON", "DJFM")))

dfgg          <- filter(dfgg, value >= 2 & value <= 100)

lgg <- vector("list", length(Cuencas)) %>% setNames(Cuencas) %>% list %>% rep(each = length(Seasons)) %>% setNames(Seasons)
for (Seas in Seasons){
  maxv <- filter(dfgg, season %in% Seas)$value %>% max
  for (Cuenca in Cuencas){
    x             <- filter(dfgg, season %in% Seas, cuenca %in% Cuenca)
    lgg[[Seas]][[Cuenca]] <- ggplot() + theme_bw() +
      geom_histogram(data = x, aes(x = value, y = 100 * stat(count) / sum(count)), bins = 20, alpha = 0.9) +
      labs(x = "Precipitación (mm/día)", y = "Frecuencia relativa (%)", title = paste(Cuenca, Seas, sep = "-")) +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 50))
  }
  gg <- ggarrange(plotlist = lgg[[Seas]], ncol = 3, nrow = 2)
  ggsave(paste0("gghist_", Seas, ".png"), gg, width = 9, height = 6)
}
rm(Seas, Cuenca, maxv, x, gg)
