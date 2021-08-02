####

setwd("~/Desktop/BP v2")

library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(magrittr)

rm(list = ls())

load("~/Desktop/BP/ws_EscalaCuenca3_SeriesCuenca.RData")
rm(list = ls()[! ls() %in% c("df_pr_cr2", "df_tm_cr2")])

load("~/Desktop/BP/ws_PreprocCuenca5_OutputComparacion.RData")
rm(list = ls()[! ls() %in% c("df_bc_ensm", "df_pr_cr2", "df_tm_cr2")])

# load("~/Desktop/BP/lims_scatter_sesgosporcuenca.RData")

####

## Acota 1979-2018

Agnos <- seq(from = 1979, to = 2019) %>% as.character

df_pr_cr2 <- df_pr_cr2 %>% filter(format(date, "%Y") %in% Agnos)
df_tm_cr2 <- df_tm_cr2 %>% filter(format(date, "%Y") %in% Agnos)

####

## Elevación: desde DEM (QGIS)

Cuencas  <- c("Achibueno", "Ancoa", "Longavi", "Lontue", "Maule", "Melado")
ElevMean <- c(1333.2, 1386.4, 1568.7, 2181.9, 2054.3, 1974.0)
ElevSd   <- c(621.4, 502.4, 416.9, 624.1, 555.5, 437.5)

####

## Consolida df

## CR2MET

df_pr_cr2_anual      <- df_pr_cr2
df_pr_cr2_anual$agno <- format(df_pr_cr2_anual$date, "%Y")
df_pr_cr2_anual      <- df_pr_cr2_anual %>% group_by(cuenca, agno) %>% summarise(pr = sum(pr))

df_tm_cr2_anual      <- df_tm_cr2
df_tm_cr2_anual$agno <- format(df_tm_cr2_anual$date, "%Y")
df_tm_cr2_anual      <- df_tm_cr2_anual %>% group_by(cuenca, agno) %>% summarise(tm = mean(tm)) %>% ungroup

df_cr2_anual <- cbind(df_pr_cr2_anual, tm = df_tm_cr2_anual$tm) %>% melt(id = c("cuenca", "agno"))
df_cr2_clim  <- df_cr2_anual %>% group_by(variable, cuenca) %>% summarise(value = mean(value)) %>% ungroup %>%
  dcast(cuenca ~ variable, value.var = "value") %>% cbind(elevmean = ElevMean)
rm(df_pr_cr2_anual, df_tm_cr2_anual)

## ERA5-BiasCorrected

df_pr_bc_ensm       <- df_bc_ensm %>% dcast(cuenca + date ~ variable, value.var = "value") %>%
  .[ , c("cuenca", "date", "pr")] %>% filter(format(date, "%Y") %in% Agnos)
df_tm_bc_ensm       <- df_bc_ensm %>% dcast(cuenca + date ~ variable, value.var = "value") %>%
  .[ , c("cuenca", "date", "tm")] %>% filter(format(date, "%Y") %in% Agnos)

df_pr_e5_anual      <- df_pr_bc_ensm
df_pr_e5_anual$agno <- format(df_pr_e5_anual$date, "%Y")
df_pr_e5_anual      <- df_pr_e5_anual %>% group_by(cuenca, agno) %>% summarise(pr = sum(pr))

df_tm_e5_anual      <- df_tm_bc_ensm
df_tm_e5_anual$agno <- format(df_tm_e5_anual$date, "%Y")
df_tm_e5_anual      <- df_tm_e5_anual %>% group_by(cuenca, agno) %>% summarise(tm = mean(tm)) %>% ungroup

df_e5_anual <- cbind(df_pr_e5_anual, tm = df_tm_e5_anual$tm) %>% melt(id = c("cuenca", "agno"))
df_e5_clim  <- df_e5_anual %>% group_by(variable, cuenca) %>% summarise(value = mean(value)) %>% ungroup %>%
  dcast(cuenca ~ variable, value.var = "value") %>% cbind(elevmean = ElevMean)
rm(df_pr_e5_anual, df_tm_e5_anual)

## Diferencia entre sets

df_dif_clim    <- df_cr2_clim
df_dif_clim$pr <- 100 * (df_e5_clim$pr - df_cr2_clim$pr) / df_cr2_clim$pr
df_dif_clim$tm <- df_e5_clim$tm - df_cr2_clim$tm

## Diferencia entre sets pero por año

df_cr2_anual <- df_cr2_anual %>% dcast(cuenca + agno ~ variable, value.var = "value") %>%
  cbind(elevmean = rep(ElevMean, each = length(Agnos)))
df_e5_anual  <- df_e5_anual %>% dcast(cuenca + agno ~ variable, value.var = "value") %>%
  cbind(elevmean = rep(ElevMean, each = length(Agnos)))

df_dif_anual    <- df_cr2_anual
df_dif_anual$pr <- 100 * (df_e5_anual$pr - df_cr2_anual$pr) / df_cr2_anual$pr
df_dif_anual$tm <- df_e5_anual$tm - df_cr2_anual$tm

####

## Plotea!

## Climatología CR2MET
gg_cr2 <- ggplot(data = df_cr2_clim, aes(x = tm, y = pr, shape = cuenca, colour = elevmean)) +
  geom_point(size = 4) + theme_bw() + scale_colour_fermenter(palette = "Spectral", direction = +1) +
  labs(x = "Temperatura (°C)", y = "Precipitación (mm/año)", shape = "Cuenca",
       colour = paste0("Elevación", "\n", "(m.s.n.m.)"), title = "CR2MET") +
  guides(shape = guide_legend(order = 1)) + theme(legend.text = element_text(size = rel(1)))

## Climatología diferencia ERA5 - CR2MET
gg_dif <- ggplot(data = df_dif_clim, aes(x = tm, y = pr, shape = cuenca, colour = elevmean)) +
  geom_point(size = 4) + theme_bw() + scale_colour_fermenter(palette = "Spectral", direction = +1) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Temperatura (°C)", y = "Precipitación (%)", shape = "Cuenca",
       colour = paste0("Elevación", "\n", "(m.s.n.m.)"), title = "Diferencia ERA5(BC) - CR2MET") +
  guides(shape = guide_legend(order = 1)) +
  #coord_cartesian(xlim = c(lims_clim$min[2], lims_clim$max[2]), ylim = c(lims_clim$min[1], 0)) +
  theme(legend.text = element_text(size = rel(1)))

## Guarda
gg <- ggarrange(gg_cr2, gg_dif, common.legend = T, legend = "right")
ggsave("gg_ClimatBC_EscalaCuenca.png", gg, height = 3.6, width = 8.2)
rm(gg_cr2, gg_dif, gg)

####

## Plotea pero por año!

## Climatología CR2MET
gg_cr2 <- ggplot(data = df_cr2_anual, aes(x = tm, y = pr, shape = cuenca, colour = elevmean)) +
  geom_point(size = 2) + theme_bw() + scale_colour_fermenter(palette = "Spectral", direction = +1) +
  labs(x = "Temperatura (°C)", y = "Precipitación (mm/año)", shape = "Cuenca",
       colour = paste0("Elevación", "\n", "(m.s.n.m.)"), title = "CR2MET") +
  guides(shape = guide_legend(order = 1)) + theme(legend.text = element_text(size = rel(1)))
  
## Climatología diferencia ERA5 - CR2MET
gg_dif <- ggplot(data = df_dif_anual, aes(x = tm, y = pr, shape = cuenca, colour = elevmean)) +
  geom_point(size = 2) + theme_bw() + scale_colour_fermenter(palette = "Spectral", direction = +1) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Temperatura (°C)", y = "Precipitación (%)", shape = "Cuenca",
       colour = paste0("Elevación", "\n", "(m.s.n.m.)"), title = "Diferencia ERA5(BC) - CR2MET") +
  guides(shape = guide_legend(order = 1)) +
  #coord_cartesian(xlim = c(lims_anual$min[2], lims_anual$max[2]), ylim = c(lims_anual$min[1], lims_anual$max[1])) +
  theme(legend.text = element_text(size = rel(1)))

## Guarda
gg <- ggarrange(gg_cr2, gg_dif, common.legend = T, legend = "right")
ggsave("gg_ClimatBC_EscalaCuenca_Agnos.png", gg, height = 3.6, width = 8.2)
rm(gg_cr2, gg_dif, gg)

####

# df_dif_clim_bc  <- df_dif_clim
# df_dif_anual_bc <- df_dif_anual
# save(df_dif_clim_bc, df_dif_anual_bc, file = "difs_bc.RData")