####

#setwd("~/Desktop/BP/Preproceso 3.0/Test v2.0")
setwd("~/Desktop/BP/Preproceso 3.0")

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

load("Test v2.0/ws_PreprocCuenca2_Data.RData")
rm(Res_Mes)

PeriodosVal  <- list(c(1979, 1983), c(1984, 1988), c(1989, 1993), c(1994, 1998),
                     c(1999, 2003), c(2004, 2008), c(2009, 2013), c(2014, 2018))
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

## caso "control"

BC_uni     <- readRDS("Test v2.0/ResBC_VerifCons_Bench.RData") %>% unlist(recursive = F)
df_bc_uni  <- do.call(rbind, BC_uni) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup

BC_ens     <- readRDS("Test v2.0/ResBC_VerifCons_Ens.RData")
BC_ens     <- lapply(Meses, function(Mes)
  lapply(PeriodosName, function(Per) BC_ens[[Per]][[Mes]]) %>% do.call(rbind, .)) %>% setNames(Meses)
df_bc_ensm <- lapply(Meses, function(Mes)
  BC_ens[[Mes]] %>% group_by(cuenca, date, variable) %>% summarise(value = mean(value)) %>% arrange(.by_group = T) %>%
    ungroup) %>% do.call(rbind, .) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup
# df_bc_ENS  <- Meses %>% lapply(function(Mes) BC_ens[[Mes]]) %>% do.call(rbind, .) %>%
#   group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup

rm(BC_uni, BC_ens)

## caso solo qdm

BC_uni         <- readRDS("Test v2.0 QDM/ResBC_VerifCons_Bench_soloQDM.RData") %>% unlist(recursive = F)
df_bc_uni_qdm  <- do.call(rbind, BC_uni) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup

BC_ens         <- readRDS("Test v2.0 QDM/ResBC_VerifCons_Ens_soloQDM.RData")
BC_ens         <- lapply(Meses, function(Mes)
  lapply(PeriodosName, function(Per) BC_ens[[Per]][[Mes]]) %>% do.call(rbind, .)) %>% setNames(Meses)
df_bc_ensm_qdm <- lapply(Meses, function(Mes)
  BC_ens[[Mes]] %>% group_by(cuenca, date, variable) %>% summarise(value = mean(value)) %>% arrange(.by_group = T) %>%
    ungroup) %>% do.call(rbind, .) %>% group_by(variable, cuenca, date) %>% arrange(.by_group = T) %>% ungroup

rm(BC_uni, BC_ens)

####

## Crea df de average-rmse y average-spread
## RMSE por cuenca!

df_rmse <- data.frame(`ERA5 raw` = (df_e5_val$value - df_obs_val$value) ^ 2,
                      `QDM sin` = (df_bc_uni_qdm$value - df_obs_val$value) ^ 2,
                      `QDM ens` = (df_bc_ensm_qdm$value - df_obs_val$value) ^ 2,
                      `LR+QDM sin` = (df_bc_uni$value - df_obs_val$value) ^ 2,
                      `LR+QDM ens` = (df_bc_ensm$value - df_obs_val$value) ^ 2, check.names = F) %>% cbind(df_obs_val[, c("cuenca", "date", "variable")], .)
df_rmse <- melt(df_rmse, id = c("cuenca", "date", "variable"), variable.name = "serie", value.name = "se")
df_rmse <- cbind(df_rmse, mes = format(df_rmse$date, "%m"))
df_rmse <- df_rmse %>% group_by(variable, cuenca, mes, serie) %>% summarise(rmse = sqrt(mean(se))) %>% ungroup

####
  
## Plotea por variable y por cuenca

df_rmse$mes <- factor(df_rmse$mes, levels = c(Meses[4:12], Meses[1:3]))

## Precipitacion
gg_pr <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
maxv  <- max(df_rmse$rmse[df_rmse$variable %in% "pr"])
pal   <- c("blue", "green", "darkgreen", "orange", "red")
for (Cuenca in Cuencas){
  df_error  <- df_rmse[df_rmse$variable %in% "pr" & df_rmse$cuenca %in% Cuenca, ]
  gg_pr[[Cuenca]] <- ggplot() + theme_bw() + coord_cartesian(ylim = c(0, maxv)) +
    scale_colour_manual(values = pal) +
    geom_line(data = df_error, aes(x = mes, y = rmse, colour = serie, group = serie)) +
    labs(title = Cuenca, x = NULL, y = "RMSE (mm/día)", colour = "Series") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme(legend.text = element_text(size = rel(1)))
}
rm(Cuenca, df_error, maxv, pal)
gg_pr_grid <- ggarrange(gg_pr[[1]], gg_pr[[2]], gg_pr[[3]], gg_pr[[4]], gg_pr[[5]], gg_pr[[6]],
                        nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
ggsave("grid_rmse-pr_cr2met-v2.0_lr-qdm.png", gg_pr_grid, width = 4 *2, height = 2.5 *3, units = "in")
rm(gg_pr, gg_pr_grid)

# ## Temperatura
# gg_tm <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
# maxv  <- max(df_rmse$rmse[df_rmse$variable %in% "tm"], df_sd$sd[df_sd$variable %in% "tm"])
# pal   <- rev(hue_pal()(3))
# for (Cuenca in Cuencas){
#   df_error  <- df_rmse[df_rmse$variable %in% "tm" & df_rmse$cuenca %in% Cuenca, ]
#   df_spread <- df_sd[df_sd$variable %in% "tm" & df_sd$cuenca %in% Cuenca, ]
#   gg_tm[[Cuenca]] <- ggplot() + theme_bw() + coord_cartesian(ylim = c(0, maxv)) + scale_colour_manual(values = pal) +
#     geom_line(data = df_error, aes(x = mes, y = rmse, colour = serie, group = serie)) +
#     #geom_line(data = df_spread, aes(x = mes, y = sd, group = 1), colour = pal[3], linetype = "dotdash") +
#     labs(title = Cuenca, x = NULL, y = "RMSE (°C)", colour = "Series") + geom_hline(yintercept = 0, color = "darkgrey")
# }
# rm(Cuenca, df_error, df_spread, maxv, pal)
# gg_tm_grid <- ggarrange(gg_tm[[1]], gg_tm[[2]], gg_tm[[3]], gg_tm[[4]], gg_tm[[5]], gg_tm[[6]],
#                         nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
# ggsave("grid_rmse-tm_cr2met-v2.0.png", gg_tm_grid, width = 4 *2, height = 2.5 *3, units = "in")
# rm(gg_tm, gg_tm_grid)

####

# ## Agregacion diaria
# ## Plotea para cada cuenca (scatters)
# 
# df_scat <- data.frame(`E5 raw` = df_e5_val$value, `E5 BC uni` = df_bc_uni$value, `E5 BC ens` = df_bc_ensm$value,
#                       check.names = F) %>% cbind(df_obs_val, .)
# df_scat <- melt(df_scat, id = c("cuenca", "date", "variable", "value"), variable.name = "serie", value.name = "svalue")
# 
# ## Precipitación
# gg_pr <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
# maxv  <- max(df_scat$value[df_scat$variable %in% "pr"])
# for (Cuenca in Cuencas){
#   df_error  <- df_scat[df_scat$variable %in% "pr" & df_scat$cuenca %in% Cuenca, ]
#   df_lines  <- df_error %>% group_by(serie) %>% summarise(svalue = mean(svalue)) %>% ungroup
#   lineobs   <- mean(df_error$value)
#   gg_pr[[Cuenca]] <- ggplot(data = df_error, aes(x = value, y = svalue, colour = serie)) +
#     #geom_hline(yintercept = lineobs, colour = "black", size = 0.7, alpha = 0.8) +
#     #geom_hline(data = df_lines, aes(yintercept = svalue, colour = serie), size = 0.7, alpha = 0.8) +
#     geom_point(size = 0.4, alpha = 0.8) + scale_x_sqrt(breaks = c(0, 1, 5, 20, 50, 100, 150, 250)) +
#     scale_y_sqrt(breaks = c(0, 1, 5, 20, 50, 100, 150, 250)) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     coord_equal(xlim = c(0, maxv), ylim = c(0, maxv)) + theme_bw() + scale_colour_manual(values = rev(hue_pal()(3))) +
#     labs(title = Cuenca, x = "CR2MET (mm/día)", y = "Serie (mm/día)", colour = "Series")
# }
# rm(Cuenca, df_error, df_lines, lineobs, maxv)
# gg_pr_dia <- ggarrange(gg_pr[[1]], gg_pr[[2]], gg_pr[[3]], gg_pr[[4]], gg_pr[[5]], gg_pr[[6]],
#                        nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
# #ggsave("grid_serie-obs_prdia_scatter_cuenca.png", gg_pr_dia, width = 3.4 *2, height = 3.6 *3, units = "in")
# rm(gg_pr)
# 
# ## Temperatura
# gg_tm <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
# limv  <- c(min(df_scat$value[df_scat$variable %in% "tm"], df_scat$svalue[df_scat$variable %in% "tm"]),
#            max(df_scat$value[df_scat$variable %in% "tm"], df_scat$svalue[df_scat$variable %in% "tm"]))
# for (Cuenca in Cuencas){
#   df_error  <- df_scat[df_scat$variable %in% "tm" & df_scat$cuenca %in% Cuenca, ]
#   df_lines  <- df_error %>% group_by(serie) %>% summarise(svalue = mean(svalue)) %>% ungroup
#   lineobs   <- mean(df_error$value)
#   gg_tm[[Cuenca]] <- ggplot(data = df_error, aes(x = value, y = svalue, colour = serie)) +
#     #geom_hline(yintercept = lineobs, colour = "black", size = 0.7, alpha = 0.8) +
#     #geom_hline(data = df_lines, aes(yintercept = svalue, colour = serie), size = 0.7, alpha = 0.8) +
#     geom_point(size = 0.4, alpha = 0.8) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     coord_equal(xlim = limv, ylim = limv) + theme_bw() + scale_colour_manual(values = rev(hue_pal()(3))) +
#     labs(title = Cuenca, x = "CR2MET (°C)", y = "Serie (°C)", colour = "Series")
# }
# rm(Cuenca, df_error, df_lines, lineobs, limv)
# gg_tm_dia <- ggarrange(gg_tm[[1]], gg_tm[[2]], gg_tm[[3]], gg_tm[[4]], gg_tm[[5]], gg_tm[[6]],
#                         nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
# #ggsave("grid_serie-obs_tmdia_scatter_cuenca.png", gg_tm_dia, width = 3.4 *2, height = 3.6 *3, units = "in")
# rm(gg_tm)
# 
# ####
# 
# ## Agregacion mensual ¿?
# ## Plotea para cada cuenca (scatters)
# 
# ## Precipitación
# df_scat_mes <- df_scat %>% cbind(agno = format(df_scat$date, "%Y"), mes = format(df_scat$date, "%m")) %>%
#   group_by(variable, cuenca, serie, agno, mes) %>% summarise(value = sum(value), svalue = sum(svalue)) %>% ungroup
# gg_pr       <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
# maxv        <- max(df_scat_mes$value[df_scat_mes$variable %in% "pr"], df_scat_mes$svalue[df_scat_mes$variable %in% "pr"])
# for (Cuenca in Cuencas){
#   df_error  <- df_scat_mes[df_scat_mes$variable %in% "pr" & df_scat_mes$cuenca %in% Cuenca, ]
#   df_lines  <- df_error %>% group_by(serie) %>% summarise(svalue = mean(svalue)) %>% ungroup
#   lineobs   <- mean(df_error$value)
#   gg_pr[[Cuenca]] <- ggplot(data = df_error, aes(x = value, y = svalue, colour = serie)) +
#     geom_hline(data = df_lines, aes(yintercept = svalue, colour = serie), alpha = 0.8, size = 1) +
#     geom_hline(yintercept = lineobs, colour = "black", alpha = 0.8, size = 0.6, linetype = "dashed") +
#     geom_point(size = 0.7, alpha = 0.8) + scale_x_sqrt(breaks = c(0, 25, 100, 250, 500, 1000, 1500)) +
#     scale_y_sqrt(breaks = c(0, 25, 100, 250, 500, 1000, 1500)) +
#     geom_smooth(method = "lm", se = F, fullrange = T, size = 1.2, alpha = 0.8) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     geom_smooth(method = "lm", se = F, fullrange = T, size = 0.2, aes(colour = NULL, group = serie), colour = "black", alpha = 0.8) +
#     coord_equal(xlim = c(0, maxv), ylim = c(0, maxv)) + theme_bw() + scale_colour_manual(values = rev(hue_pal()(3))) +
#     labs(title = Cuenca, x = "CR2MET (mm/mes)", y = "Serie (mm/mes)", colour = "Series")
# }
# rm(Cuenca, df_error, df_lines, lineobs, maxv)
# gg_pr_mes <- ggarrange(gg_pr[[1]], gg_pr[[2]], gg_pr[[3]], gg_pr[[4]], gg_pr[[5]], gg_pr[[6]],
#                        nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
# #ggsave("grid_serie-obs_prmes_scatter_cuenca.png", gg_pr_mes, width = 3.4 *2, height = 3.6 *3, units = "in")
# rm(gg_pr)
# rm(df_scat_mes)
# 
# ## Temperatura
# df_scat_mes <- df_scat %>% cbind(agno = format(df_scat$date, "%Y"), mes = format(df_scat$date, "%m")) %>%
#   group_by(variable, cuenca, serie, agno, mes) %>% summarise(value = mean(value), svalue = mean(svalue)) %>% ungroup
# gg_tm       <- vector("list", length(Cuencas)) %>% setNames(Cuencas)
# limv        <- c(min(df_scat_mes$value[df_scat_mes$variable %in% "tm"], df_scat_mes$svalue[df_scat_mes$variable %in% "tm"]),
#                  max(df_scat_mes$value[df_scat_mes$variable %in% "tm"], df_scat_mes$svalue[df_scat_mes$variable %in% "tm"]))
# for (Cuenca in Cuencas){
#   df_error  <- df_scat_mes[df_scat_mes$variable %in% "tm" & df_scat_mes$cuenca %in% Cuenca, ]
#   df_lines  <- df_error %>% group_by(serie) %>% summarise(svalue = mean(svalue)) %>% ungroup
#   lineobs   <- mean(df_error$value)
#   gg_tm[[Cuenca]] <- ggplot(data = df_error, aes(x = value, y = svalue, colour = serie)) +
#     geom_hline(data = df_lines, aes(yintercept = svalue, colour = serie), alpha = 0.8, size = 1) +
#     geom_hline(yintercept = lineobs, colour = "black", alpha = 0.8, size = 0.6, linetype = "dashed") +
#     geom_point(size = 0.7, alpha = 0.8) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     coord_equal(xlim = limv, ylim = limv) + theme_bw() + scale_colour_manual(values = rev(hue_pal()(3))) +
#     labs(title = Cuenca, x = "CR2MET (°C)", y = "Serie (°C)", colour = "Series")
# }
# rm(Cuenca, df_error, df_lines, lineobs, limv)
# gg_tm_mes <- ggarrange(gg_tm[[1]], gg_tm[[2]], gg_tm[[3]], gg_tm[[4]], gg_tm[[5]], gg_tm[[6]],
#                        nrow = 3, ncol = 2, common.legend = T, legend = "bottom")
# #ggsave("grid_serie-obs_tmmes_scatter_cuenca.png", gg_tm_mes, width = 3.4 *2, height = 3.6 *3, units = "in")
# rm(gg_tm)
# rm(df_scat_mes)
# 
# ## Agrupa ambos: diarios y mensual
# 
# ## Precipitacion
# gg_pr_panel <- ggarrange(gg_pr_dia, gg_pr_mes, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")
# ggsave("grid-scatt-pr_cr2met-v2.0.png", gg_pr_panel, width = 3.4 *4, height = 3.6 *3, units = "in")
# 
# ## Temperatura
# gg_tm_panel <- ggarrange(gg_tm_dia, gg_tm_mes, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")
# ggsave("grid-scatt-tm_cr2met-v2.0.png", gg_tm_panel, width = 3.4 *4, height = 3.6 *3, units = "in")
# 
# rm(gg_pr_dia, gg_pr_mes, gg_pr_panel, gg_tm_dia, gg_tm_mes, gg_tm_panel)

####

#save.image("~/Desktop/BP/ws_PreprocCuenca5_OutputComparacion.RData")

####

