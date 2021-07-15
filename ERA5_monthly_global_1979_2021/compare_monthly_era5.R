rm(list = ls())
gc()
library(feather)
library(dplyr)
#################################################################
##                    COMPARE PRECIPITATION                    ##
#################################################################

## read era5 from daily to monthly (Diego's)
file_lsp = read_feather("/home/fco/git_workspace/Jupyter_projects/files_nc/DIEGOS//era5_lsp_1979-2020_mes-prom.feather")
file_cp  = read_feather("/home/fco/git_workspace/Jupyter_projects/files_nc/DIEGOS//era5_cp_1979-2020_mes-prom.feather")

file_pt  = (select(file_cp,-date)+ select(file_lsp,-date) )%>% multiply_by(24) %>% cbind(date=file_cp$date)
col_names= names(file_pt)
rm(file_cp,file_lsp)

## read era5 monthly directly from platform
file_pp=read_feather("/home/fco/git_workspace/Jupyter_projects/files_feather//pp_global_monthly_era5_1979_2021.feather", columns= col_names)

dif_pp= head(file_pp,-2)-file_pt
plot(head(file_pp$X288_.37,-2),file_pt$X288_.37)

#################################################################
##                     COMPARE TEMPERATURE                     ##
###################################### read era5 from daily to monthly (Diego's)
file_t2m_fromdaily = read_feather("/home/fco/git_workspace/Jupyter_projects/files_nc/DIEGOS//era5_t2m_1979-2020_mes.feather")
col_names= names(file_t2m_fromdaily)

## read era5 monthly directly from platform
file_t2m =read_feather("/home/fco/git_workspace/Jupyter_projects/files_feather//t2m_global_monthly_era5_1979_2021.feather", columns= col_names)

plot(head(file_t2m$X288_.37,-2),file_t2m_fromdaily$X288_.37)

dif_t2m= head(file_t2m,-2)-file_t2m_fromdaily
#############################
