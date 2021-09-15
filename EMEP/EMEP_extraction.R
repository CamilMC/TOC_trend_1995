library(ncdf4)
EMEP_file <- "EMEP/EMEP01_L20EC_rv4_33_year.2000met_2000emis_rep2019.nc"

site_name <- waterchem_1995$lake_name2
site_lat <- waterchem_1995$Latitude
site_long <- waterchem_1995$Longitude

site_n <- length(site_name)

nearest_grid_in_lat <- rep(0,site_n)
nearest_grid_in_long <- rep(0,site_n)

grid_data <- nc_open(EMEP_file)

grid_lat <- grid_data$dim$lat$vals
grid_long <- grid_data$dim$lon$vals

n_lat_points <- length(grid_lat)
n_long_points <- length(grid_long)

total_SOX <- grid_data$var$WDEP_SOX$vals + grid_data$var$DDEP_SOX_m2Grid$vals

for (i in 1:site_n){
  for (j in 1:(n_lat_points-1)){
    if (((site_lat[i]-grid_lat[j])*(site_lat[i]-grid_lat[j+1]) > 0) == TRUE) {
      next
    } else if (abs(site_lat[i] - grid_lat[j]) < abs(site_lat[i] - grid_lat[j+1])){
      nearest_grid_in_lat[i] <- j-1
    } else {
      nearest_grid_in_lat[i] <- j
    }
  }
}

for (i in 1:site_n){
  for (j in 1:(n_long_points-1)){
    if (((site_long[i]-grid_long[j])*(site_long[i]-grid_long[j+1]) > 0 ) == TRUE){
      next
    } else if ((site_long[i] - grid_long[j]) < (site_long[i] - grid_long[j+1])){
      nearest_grid_in_long[i] <- j
    } else {
      nearest_grid_in_long[i] <- j+1
    }
  }
}


wsox <- ncvar_get(grid_data,varid = "WDEP_SOX")
dsox <- ncvar_get(grid_data,varid = "DDEP_SOX_m2Grid")

tsox <- NA
for (x in 1:site_n){
  tsox[x] <- wsox[nearest_grid_in_long[x],nearest_grid_in_lat[x]]+
    dsox[nearest_grid_in_long[x],nearest_grid_in_lat[x]]
}

df_sox <- data.frame(site_name = site_name, site_lat = site_lat, site_long = site_long, tsox = tsox)
saveRDS(df_sox, file = "df_sox.rds")

no_test <- readRDS("Fennoscandia_NTNU/GADM_2.8_NOR_adm0.rds")
se_test <- readRDS("Fennoscandia_NTNU/GADM_2.8_SWE_adm0.rds")
