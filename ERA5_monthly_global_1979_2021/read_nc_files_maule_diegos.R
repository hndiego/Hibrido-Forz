rm(list = ls())
gc()

library(magrittr)
library(ncdf4)
library(dplyr)
library(pbapply)
library(feather)
library(reshape2)


data_netcdf_filelist <- function(filename) {
  message(filename)
  
  nc_file     <- filename  %>% nc_open
  file_lon    <- nc_file %>% ncvar_get("longitude")
  file_lat    <- nc_file %>% ncvar_get("latitude")
  file_time   <- nc_file %>% ncvar_get("time")
  
 
  var_name    <- names(nc_file$var)[2]
  print(var_name)
  
  dataframe   <- nc_file %>% ncvar_get(var_name) %>%
      structure(dimnames  = list(file_lon,file_lat, file_time / 24)) %>%
    reshape2::melt(varnames = c("x","y", "date"), value.name = var_name) %>%
    reshape2::dcast(date ~ x + y, value.var = var_name) %>%
    transform(date = as.Date(file_time / 24, origin = "1900-01-01"))
     
  lubridate::day(dataframe$date) <- 1 # format YYYY-MM-01
  
  nc_close(nc_file)
  filename_export = gsub(".nc$", ".feather", filename)
  
  write_feather(dataframe, filename_export)
  return(filename_export)
}


#################################################################
##            READ NETCDF FROM FILELIST IN A FOLDER            ##
#################################################################
path_dir                <- "/home/fco/git_workspace/Jupyter_projects/files_nc/DIEGOS/"
list_filenames          <- list.files(path = path_dir ,pattern = ".nc$",full.names = T)
for (filename in list_filenames) {
  try(data_netcdf_filelist(filename))
}
