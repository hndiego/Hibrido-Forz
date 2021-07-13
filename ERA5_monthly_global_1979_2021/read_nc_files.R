rm(list = ls())
gc()

args = commandArgs(trailingOnly=TRUE)
# run in terminal:
# for i in `seq 2 5`; do Rscript read_nc_files.R $i ; done

library(parallel)
library(magrittr)
library(ncdf4)
library(dplyr)
library(pbapply)
library(feather)
library(reshape2)

##################################################################
##                                                              ##
##                          SECCION 0:                          ##
##                IMPORTACION DE ARCHIVOS                       ##
##                                                              ##
##################################################################

read_only_one_lat <- function(lat_index,var_name, nc_file, file_lon, file_lat, file_time) {

  #message(paste0("reading lat index: ", lat_index))
  
  # read netcdf for variable var_name
  # only read for lat=lat_index
  # transform into a tabular format
  file_pr   <- nc_file %>%
    ncvar_get(var_name,
              start = c(1, lat_index, 1, 1),
              count = c(-1, 1, 1, -1)) %>%
    structure(dimnames  = list(file_lon, file_time / 24)) %>%
    reshape2::melt(varnames = c("x", "date"), value.name = var_name) %>%
    transform(y = file_lat[lat_index]) %>%
    reshape2::dcast(date ~ x + y, value.var = var_name) %>%
    select(-date)
  
  return(file_pr)
}

data_netcdf_filelist <- function(filename) {
  message(filename)
  
  nc_file   <- filename  %>% nc_open
  file_lon  <- nc_file %>% ncvar_get("longitude")
  file_lat  <- nc_file %>% ncvar_get("latitude")
  file_time <- nc_file %>% ncvar_get("time")
  var_name  <- names(nc_file$var)
  
  dataframe = pblapply(seq(file_lat), 
                     function(lat_index) read_only_one_lat(lat_index,var_name, nc_file, file_lon, file_lat, file_time)
                     ) %>%
    Reduce(cbind, .)  %>%
    transform(date = as.Date(file_time / 24, origin = "1900-01-01")) %>%
    na.omit()
  
  nc_close(nc_file)
  filename_export = gsub(".nc$", ".feather", filename)
  
  write_feather(dataframe, filename_export)
  
  return(filename_export)
}

#################################################################
##            READ NETCDF FROM FILELIST IN A FOLDER            ##
#################################################################
path_dir             <- "/home/fco/git_workspace/Jupyter_projects/"
list_filenames       <- list.files(path = path_dir ,pattern = ".nc$",full.names = T)
filename             <- list_filenames[as.integer(args[1])]
#message(filename)

data_netcdf_filelist(filename)
