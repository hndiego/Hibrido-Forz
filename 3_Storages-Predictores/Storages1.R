####

setwd("~/Desktop/BP v2")

library(scales)
library(airGR)
library(hydroGOF)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(magrittr)

rm(list = ls())

load("ws_NuevaMod1.RData")
df_bc_ensm <- readRDS("forzERA5BC_hist1979-ACTUAL_qdm-ss_n40.RData")  # actualiza forzante

####

## Solo "mejores" configuraciones segun resultados exploratorios
## Melado: GR6J, params de Achibueno

CuencasMod   <- c("Achibueno", "Ancoa", "Longavi", "Lontue", "Maule", "Melado")  # actualiza, agrega melado

ParAchibueno <- readRDS("Res Calib v6/ResCal_GR6J_FO-Q_Grads-9m.RData")$Achibueno[[1]]$par
ParAncoa     <- readRDS("Res Calib v6/ResCal_GR6J_FO-Q_Grads-9m.RData")$Ancoa[[1]]$par
ParLongavi   <- readRDS("Res Calib v6/ResCal_GR6J_FO-Q_Grads-9m.RData")$Longavi[[1]]$par
ParLontue    <- readRDS("Res Calib v6/ResCal_GR6J_FO-Mixta_Grads-9m.RData")$Lontue[[1]]$par
ParMaule     <- readRDS("Res Calib v6/ResCal_GR6J_FO-Mixta_Grads-9m.RData")$Maule[[1]]$par

Params       <- list(ParAchibueno, ParAncoa, ParLongavi, ParLontue, ParMaule, ParAchibueno) %>% setNames(CuencasMod)
rm(ParAchibueno, ParAncoa, ParLongavi, ParLontue, ParMaule)

####

## GRADIENTES MODIFICADOS

## Carga funciones GradP_Valery2010() y GradT_Valery2010() de airGR
## Modifica codigo de DataAltiExtrapolation_Valery() de airGR
## Linkea codigo modificado en CreateInputsModel() de airGR

source("GradT_Valery2010.R")
source("GradP_Valery2010.R")
source("DataAltiExtrapolation_Valery_Mod.R")

body(CreateInputsModel)[[20]][[3]] <- substitute(
  {
    RESULT <- DataAltiExtrapolation_Valery_Mod(DatesR = DatesR, Precip = Precip,
                                               PrecipScale = PrecipScale, TempMean = TempMean, TempMin = TempMin,
                                               TempMax = TempMax, ZInputs = ZInputs, HypsoData = HypsoData,
                                               NLayers = NLayers, verbose = verbose)
    if (verbose) {
      if (NLayers == 1) {
        message("input series were successfully created on 1 elevation layer for use by CemaNeige")
      }
      else {
        message("input series were successfully created on ",
                NLayers, " elevation layers for use by CemaNeige")
      }
    }
  }
)

####

dfforz    <- df_bc_ensm
dfforz$wy <- wYear(dfforz$date)
Nwup   <- 2  # cambia nwup 

AgnosMod   <- seq(from = 1981, to = 2021) %>% as.character
AgnosMeteo <- seq(from = 1981 - Nwup, to = 2021) %>% as.character

####

## Run modelo

RESMOD <- vector("list", length(CuencasMod)) %>% setNames(CuencasMod)
for (Cuenca in CuencasMod){
  dias <- filter(dfforz, wy %in% AgnosMeteo)$date %>% unique
  wy   <- wYear(dias)
  pr   <- filter(dfforz, cuenca %in% Cuenca & variable %in% "pr" & wy %in% AgnosMeteo)$value
  tm   <- filter(dfforz, cuenca %in% Cuenca & variable %in% "tm" & wy %in% AgnosMeteo)$value
  etp  <- PE_Oudin(as.numeric(format(dias, "%j")), tm, Lats[Cuenca], "deg")
  hyp  <- filter(df_hypsos, cuenca %in% Cuenca)$z
  
  print(Cuenca)
  print(Sys.time())
  
  ## Para grads modificados:
  source("GradT_Cuenca_v0.R")
  source("GradP_Cuenca_v0.R")
  GradP_Exp <- function(){GradP_Cuenca_v0()}
  assign("GradP_Exp", GradP_Exp, envir = .GlobalEnv)
  GradT_Exp <- function(){GradT_Cuenca_v0()}
  assign("GradT_Exp", GradT_Exp, envir = .GlobalEnv)
  
  ## MODELOS GR
  InputsModel6J <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR6J, DatesR = as.POSIXlt(dias), Precip = pr, PotEvap = etp,
                                     TempMean = tm, ZInputs = ElevMean[Cuenca], HypsoData = hyp, NLayers = Nbandas)
  
  ind_val <- which(wy %in% AgnosMod)  # validacion: por cada año fuera de calib
  ind_wup <- which(wy %in% AgnosMeteo[! AgnosMeteo %in% AgnosMod])

  RunOptions6J <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR6J, InputsModel = InputsModel6J,
                                   IndPeriod_Run = ind_val, IndPeriod_WarmUp = ind_wup)
  OutputsModel <- RunModel(InputsModel = InputsModel6J, RunOptions = RunOptions6J,
                           Param = Params[[Cuenca]], FUN = RunModel_CemaNeigeGR6J)
  
  ## FIN
  PRODsim <- OutputsModel$Prod
  ROUTsim <- OutputsModel$Rout
  EXPsim  <- OutputsModel$Exp
  SPsim   <- lapply(OutputsModel$CemaNeigeLayers, function(layer) layer$SnowPack) %>% Reduce("+", .) %>% `/`(Nbandas)
  Qsim    <- OutputsModel$Qsim
  RESMOD[[Cuenca]] <- data.table(date = dias[ind_val], wy = wy[ind_val], cuenca = Cuenca,
                                         SP = SPsim, PROD = PRODsim, ROUT = ROUTsim, EXP = EXPsim, Q = Qsim)
}
"SimIHC-Q_wys1981-2021_forzERA5BC.RData" %>% saveRDS(RESMOD, .)

rm(InputsModel4J, InputsModel5J, InputsModel6J, RunOptions4J, RunOptions5J, RunOptions6J,
   OutputsModel, Cuenca, Agno, dias, wy, pr, tm, etp, hyp, ind_val, ind_wup, SPsim,
   Params, EXPsim, PRODsim, ROUTsim)
  
####
####
####

# ## Plotea comparacion 2020
# 
# df_q0 <- readRDS("data Q6/Data_Q0_m3s_wys1978-2020.RData") %>% filter(wy %in% "2020") %>%
#   melt(id = c("date", "wy"), variable.name = "cuenca") %>% mutate(variable = "Q0")
# 
# df_qf <- readRDS("data Q6/Data_Qf_m3s_wys1978-2020.RData") %>% filter(wy %in% "2020") %>%
#   melt(id = c("date", "wy"), variable.name = "cuenca") %>% mutate(variable = "Qf")
# 
# Areas <- c(894223342.98, 273376074.55, 465501086.73, 1368173710.52, 5474329605.44, 2128367023.88) %>% setNames(Cuencas)
# 
# df_qs <- lapply(CuencasMod, function(Cuenca) RESMOD[[Cuenca]] %>% select(date, wy, cuenca, Q) %>% filter(wy %in% "2020")) %>%
#   do.call(rbind, .) %>% dcast(date + wy ~ cuenca, value.var = "Q")
# 
# df_qm3           <- df_qs
# #df_qm3$Ancoa     <- df_qs$Ancoa * Areas["Ancoa"] / (1000 * 86400)
# df_qm3$Achibueno <- df_qs$Achibueno * Areas["Achibueno"] / (1000 * 86400)
# df_qm3$Longavi   <- df_qs$Longavi * Areas["Longavi"] / (1000 * 86400)
# df_qm3$Lontue    <- df_qs$Lontue * Areas["Lontue"] / (1000 * 86400)
# df_qm3$Maule     <- df_qs$Maule * Areas["Maule"] / (1000 * 86400)
# df_qm3$Melado    <- df_qs$Melado * Areas["Melado"] / (1000 * 86400)
# rm(df_qs)
# df_qm3 <- df_qm3 %>% melt(id = c("date", "wy"), variable.name = "cuenca") %>% mutate(variable = "Qs")
# 
# ## Consolida data
# 
# df_gg     <- rbind(df_q0, df_qf, df_qm3)
# df_gg$mes <- df_gg$date %>% format("%m")
# 
# df_ggmes  <- df_gg %>% filter(! mes %in% "11" ) %>% group_by(cuenca, mes, variable) %>%
#   summarise(value = mean(value, na.rm = T)) %>% ungroup
# df_ggmes$variable <- factor(df_ggmes$variable, levels = c("Qf", "Q0", "Qs"))
# 
# gg <- ggplot(data = df_ggmes, aes(x = mes, y = value, colour = variable, group = interaction(variable, cuenca))) +
#   geom_line() + facet_wrap(~ cuenca, nrow = 3, ncol = 2, scales = "free_y") + theme_bw() +
#   theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
#   labs(x = NULL, y = expression("Caudal medio mensual (m" ^{3} * "/s)"), color = "Serie") +
#   scale_colour_manual(values = c("darkgrey", "black", "blue")) + coord_cartesian(ylim = c(0, NA))
# ggsave("gg_compara2020.png", gg, width = 7, height = 5, units = "in")
  