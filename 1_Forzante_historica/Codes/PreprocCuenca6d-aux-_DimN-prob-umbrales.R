####

setwd("~/Desktop/BP v2")

library(data.table)
library(dplyr)
library(reshape2)
library(scales)
library(ggplot2)
library(ggpubr)
library(hydroGOF)
library(verification)
library(SpecsVerification)
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
# ## solo algunos miembros ensemble
# n          <- seq(from = 10, to = 100, by = 10)
# ## promedio ens
# df_bc_ensm <- lapply(n, function(nens) lapply(Meses, function(Mes)
#   BC_ens[[Mes]] %>% filter(i_ens <= nens) %>% group_by(cuenca, date, variable) %>% summarise(value = mean(value), n_ens = nens)
#   %>% arrange(.by_group = T) %>% ungroup) %>% do.call(rbind, .)) %>% do.call(rbind, .) %>% group_by(variable, cuenca, date, n_ens) %>%
#   arrange(.by_group = T) %>% ungroup
# # df_bc_ENS  <- Meses %>% lapply(function(Mes) BC_ens[[Mes]]) %>% do.call(rbind, .) %>%
# #   group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
# rm(BC_ens, n)

BC_ens <- do.call(rbind, BC_ens)
BC_ens <- filter(BC_ens, variable %in% "pr")
BC_ens$variable <- NULL
BC_ens$mes      <- format(BC_ens$date, "%m")

df_obs_val <- filter(df_obs_val, variable %in% "pr")
df_obs_val$variable <- NULL
df_obs_val$mes      <- format(df_obs_val$date, "%m")

rm(df_e5_val, list_e5_cal, list_e5_val, list_obs_cal, list_obs_val, PeriodosVal)

####

## Estaciones del agno

Seasons <- c("AM", "JJA", "SON", "DJFM")

BC_ens$season <- ifelse(BC_ens$mes %in% c("04", "05"), "AM",
                        ifelse(BC_ens$mes %in% c("06", "07", "08"), "JJA",
                               ifelse(BC_ens$mes %in% c("09", "10", "11"), "SON", "DJFM")))

df_obs_val$season <- ifelse(df_obs_val$mes %in% c("04", "05"), "AM",
                            ifelse(df_obs_val$mes %in% c("06", "07", "08"), "JJA",
                                   ifelse(df_obs_val$mes %in% c("09", "10", "11"), "SON", "DJFM")))

## Orden comun

BC_ens <- BC_ens %>% group_by(cuenca, date, i_ens) %>% arrange(.by_group = T) %>% ungroup
df_obs_val <- df_obs_val %>% group_by(cuenca, date) %>% arrange(.by_group = T) %>% ungroup

####

Umbrales <- c(NA, 0, 2, 10, 20, 30)

####

for (Umbral in Umbrales){
  ## CRPSS
  ## ref: n = 35... adivinanza inicial
  Sizes   <- seq(5, 60, by = 5)
  l_crpss <- vector("list", length(Sizes)) %>% setNames(Sizes) %>% list %>% rep(each = length(Cuencas)) %>%
    setNames(Cuencas) %>% list %>% rep(each = length(Seasons)) %>% setNames(Seasons)
  for (Season in Seasons){
    for (Cuenca in Cuencas){
      ens_ref  <- filter(BC_ens, cuenca %in% Cuenca & season %in% Season & i_ens %in% seq_len(35)) %>%
        acast(date ~ i_ens, value.var = "value")
      obs_ref  <- filter(df_obs_val, cuenca %in% Cuenca & season %in% Season)$value
      bool     <- rep(T, length(obs_ref))
      fnam     <- "gg-Nensemble_crpss_refN35_todo.png"
      if (! Umbral %in% NA){
        bool   <- obs_ref > Umbral
        fnam   <- paste0("gg-Nensemble_crpss_refN35_umbral-", Umbral, ".png")
      }
      crps_ref <- crpsDecomposition(obs_ref[bool], ens_ref[bool, ])$CRPS
      for (Size in Sizes){
        ens  <- filter(BC_ens, cuenca %in% Cuenca & season %in% Season & i_ens %in% seq_len(Size)) %>%
          acast(date ~ i_ens, value.var = "value")
        crps <- crpsDecomposition(obs_ref[bool], ens[bool, ])$CRPS
        l_crpss[[Season]][[Cuenca]][[as.character(Size)]] <- 1 - crps / crps_ref
      }
    }
  }
  rm(Season, Cuenca, Size, ens_ref, obs_ref, crps_ref, ens, crps)
  ## Plotea!
  df_crpss <- lapply(Seasons, function(Season) lapply(Cuencas, function(Cuenca) lapply(as.character(Sizes), function(Size)
    data.frame(season = Season, cuenca = Cuenca, n_ens = Size, crpss = l_crpss[[Season]][[Cuenca]][[Size]])) %>%
      do.call(rbind, .)) %>% do.call(rbind, .)) %>% do.call(rbind, .)
  gg   <- ggplot(data = df_crpss, aes(x = n_ens, y = crpss, group = interaction(cuenca, season))) + theme_bw() +
    geom_hline(yintercept = 0, linetype = "dotted") + geom_line() + facet_grid(season ~ cuenca) +
    labs(x = "N ensemble", y = "CRPSS, ref: N=35") + theme(legend.text = element_text(size = rel(1)))
  ggsave(fnam, gg, width = 12, height = 8, units = "in")
  rm(gg)
}

## Rank Histogram
## EDITA FUNCION, K = 10 FIJO!

##

body(Rankhist)[[12]] <- substitute(
  rank.hist <- hist(((ranks - 1) / K) * 9 + 1, breaks = seq(0.5, 10 + 0.5, reduce.bins), 
                    plot = FALSE)[["counts"]]
)

##

for (Umbral in Umbrales[2:6]){
  
  Sizes   <- seq(20, 60, by = 5)
  l_rh <- vector("list", length(Sizes)) %>% setNames(Sizes) %>% list %>% rep(each = length(Cuencas)) %>%
    setNames(Cuencas) %>% list %>% rep(each = length(Seasons)) %>% setNames(Seasons)
  for (Season in Seasons){
    for (Cuenca in Cuencas){
      obs   <- filter(df_obs_val, cuenca %in% Cuenca & season %in% Season)$value
      bool  <- rep(T, length(obs))
      fnam1 <- "gg-Nensemble_rankhist_todo_"
      if (! Umbral %in% NA){
        bool   <- obs > Umbral
        fnam1  <- paste0("gg-Nensemble_rankhist_umbral-", Umbral, "_")
      }
      for (Size in Sizes){
        ens  <- filter(BC_ens, cuenca %in% Cuenca & season %in% Season & i_ens %in% seq_len(Size)) %>%
          acast(date ~ i_ens, value.var = "value")
        l_rh[[Season]][[Cuenca]][[as.character(Size)]]$rh  <- Rankhist(ens[bool, ], obs[bool])
        l_rh[[Season]][[Cuenca]][[as.character(Size)]]$rel <- crpsDecomposition(obs[bool], ens[bool, ])$Reli
      }
    }
  }
  rm(Season, Cuenca, Size, ens, obs)
  ## formato data
  df_rh <- lapply(Seasons, function(Season) lapply(Cuencas, function(Cuenca) lapply(as.character(Sizes), function(Size)
    data.frame(season = Season, cuenca = Cuenca, n_ens = Size, counts = l_rh[[Season]][[Cuenca]][[Size]]$rh, bins = 1:10,
               rel = paste0("Reli: ", format(round(l_rh[[Season]][[Cuenca]][[Size]]$rel, 3), nsmall = 3)))) %>%
      do.call(rbind, .)) %>% do.call(rbind, .)) %>% do.call(rbind, .)
  df_rh       <- df_rh %>% group_by(season, cuenca, n_ens) %>% mutate(rfrec = 100 * counts / sum(counts)) %>% ungroup
  df_rh$bins  <- factor(as.character(df_rh$bins), levels = as.character(1:10))
  df_rh$n_ens <- paste("N =", df_rh$n_ens)
  ## plotea!
  for (Season in Seasons){
    df <- filter(df_rh, season %in% Season)
    gg <- ggplot(data = df, aes(x = bins, y = rfrec)) + theme_bw() + geom_hline(yintercept = 100/10, linetype = "dashed") +
      geom_bar(stat = "identity", alpha = 0.8) + facet_grid(cuenca ~ n_ens) +
      geom_text(data = df, aes(x = -Inf, y = Inf, label = rel), hjust = -0.1, vjust = 1.5) +
      labs(x = "Ranked-bins de observaciones", y = "Frecuencia relativa (%)") + theme(legend.text = element_text(size = rel(1)))
    fnam2 <- paste0(fnam1, Season, ".png")
    ggsave(fnam2, gg, width = 12, height = 8, units = "in")
  }
  rm(Season, df, gg, fnam2)
  
}

####