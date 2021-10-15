library(DBI)
library(sf)
library(dplyr)
library(gimms)
library(ncdf4)

library(devtools)
#install_github("mitmat/eurocordexr")
library(eurocordexr)
library(rgdal)
library(stringr)


library(stars)
library(lwgeom)

con <- dbConnect(RPostgreSQL::PostgreSQL(),user = "camille.crapart", password = "camille",host = "vm-srv-wallace.vm.ntnu.no", dbname = "nofa")
#catchment.geom <- tbl(con,sql("SELECT ebint, geom FROM catchments.lake_catchments"))


ebint.tbl <- tbl(con,sql("SELECT ebint FROM catchments.lake_catchments")) 
ebint.catch <- pull(ebint.tbl,ebint)

ebint.waterchem.tbl <- tbl(con,sql("SELECT ebint FROM environmental.north_euro_lake_surv_1995"))
ebint.waterchem <- pull(ebint.waterchem.tbl,ebint)

ebint <- intersect(ebint.waterchem, ebint.catch)

catchment.poly.1000 <- st_read(con,query = "SELECT ebint, geom FROM catchments.lake_catchments WHERE ebint IN (SELECT ebint FROM environmental.north_euro_lake_surv_1995)")
plot(catchment.poly.1000)

#TOC
toc.tbl <- tbl(con,sql("SELECT ebint,toc_mg_l,dist_closest_ebint,dist_2nd_closest_ebint FROM environmental.north_euro_lake_surv_1995"))
toc.df <- as.data.frame(toc.tbl)

#NDVI
# http://poles.tpdc.ac.cn/en/data/9775f2b4-7370-4e5e-a537-3482c9a83d88/

ndvi.1994 <- downloadGimms(x= 1994,y=1994,dsn = "NDVI")
ndvi.max <- monthlyComposite(ndvi.1994,monthlyIndices(ndvi.1994))

mai.ndvi <- extract(ndvi.max[[5]],catchment.poly.1000)
names(mai.ndvi) <- ebint
saveRDS(mai.ndvi,"NDVI/mai.ndvi.Rdata")

june.ndvi <- extract(ndvi.max[[6]],catchment.poly.1000)
names(june.ndvi) <- ebint
saveRDS(june.ndvi,"NDVI/june.ndvi.Rdata")
june.info <- 

july.ndvi <- extract(ndvi.max[[7]],catchment.poly.1000)
names(july.ndvi) <- ebint
saveRDS(july.ndvi,"NDVI/july.ndvi.Rdata")

august.ndvi <- extract(ndvi.max[[8]],catchment.poly.1000)
names(august.ndvi) <- ebint
saveRDS(august.ndvi,"NDVI/august.ndvi.Rdata")

mean.june.ndvi <- lapply(june.ndvi,mean) %>% unlist() %>% as.data.frame %>% setNames("june")
mean.july.ndvi <- lapply(july.ndvi,mean) %>% unlist() %>% as.data.frame %>% setNames("july")
mean.august.ndvi <- lapply(august.ndvi,mean) %>% unlist() %>% as.data.frame %>% setNames("august")

mean.ndvi <- cbind(mean.june.ndvi,mean.july.ndvi,mean.august.ndvi)
mean.ndvi$summer <- rowMeans(data.matrix(mean.ndvi), na.rm = T) * 0.0001

summer.list <- mapply(c,june.ndvi,july.ndvi,august.ndvi)
summer.list.ndvi <- lapply(summer.list,function(x) {(floor(x)/10)/1000})
summer.ndvi.df <- lapply(summer.list.ndvi,mean) %>% unlist() %>% as.data.frame() %>% setNames("ndvi")
summer.ndvi.df$ebint <- rownames(summer.ndvi.df)
saveRDS(summer.ndvi.df, "NDVI/summer.ndvi.df.Rdata")

summer.list.flag <- lapply(summer.list,function(x) {x-(floor(x)/10)*10+1})
flags <- summer.list.flag %>% unlist() %>% as.numeric()

# Merge NDVI and toc
dataset <- merge(toc.df, summer.ndvi.df, by.x = "ebint", by.y = "ebint")

# temperature data
temp.file <- "CORDEX/tas_EUR-11i_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1_mon_199101-200012.nc"
temp.test <- raster(temp.file,varname = "tas", band = 54)
plot(temp.test)

temp.info <- nc_open(temp.file)
temp.stack <- stack(temp.file, varname = "tas")
  
dates <- getZ(temp.stack)
tas.1995 <- stack(temp.file,varname = "tas", bands = grep("1995",dates))
mean.tas.1995 <- mean(tas.1995) 

mean.tas.1995.nordic <- extract(mean.tas.1995,catchment.poly.1000)
names(mean.tas.1995.nordic) <- ebint

tas.1995.nordic <- lapply(mean.tas.1995.nordic,mean) %>% unlist() %>% -273.15 %>% as.data.frame() %>% setNames("tas")
View(tas.1995.nordic)

### runoff data
runoff.file <- "CORDEX/mrros_EUR-11_IPSL-IPSL-CM5A-LR_historical_r1i1p1_GERICS-REMO2015_v1_mon_199101-200012.nc"
runoff.nc <- nc_open(runoff.file)
runoff.st <- st_layers(runoff.nc)

## Change projection
# https://github.com/dquesadacr/CORDEX/blob/master/CORDEX_plot.R

runoff.info <- GDALinfo(runoff.nc)
runoff.stack <- stack(runoff.file, varname = "mrros")

attr.runoff <- attr(runoff.info,"mdata")
index.var <- attr.runoff[grep("#units",attr.runoff)] %>% str_which("mrros")
long.name <- unlist(
  str_split(attr.runoff[grep("long_name", attr.runoff, ignore.case = T)]
            [str_which(
              attr.runoff[grep("long_name", attr.runoff, ignore.case = T)],"mrros")
              ],
            "="))[2]
# gives that the long name corresponsing to mrros is "Surface Runoff". Thanks

rot.info <- attr.runoff[grep("^rotated",attr(runoff.info,"mdata"))]
coord<- sapply(rot.info, FUN = function(x) {
  unlist(str_split(x, pattern = "="))[2]})
coord.num<- round(as.numeric(coord[2:3]),2)
coord.num[2]<- coord.num[2]+360
target.crs <- paste0("+proj=ob_tran +o_proj=longlat +o_lon_p=" , 
                     coord.num[2], " +o_lat_p=", coord.num[1], 
                     " +lon_0=180 +to_meter=0.0174532925199433")
crs(runoff.stack) <- st_crs(4326)$proj4string
reproj.1 <- projectRaster(runoff.stack,crs = target.crs)

plot(reproj.1)

# with another file
runoff.file <- "CORDEX/mrros_EUR-11_NCC-NorESM1-M_historical_r1i1p1_DMI-HIRHAM5_v3_mon_196101-197012.nc"
runoff.nc <- nc_open(runoff.file)
runoff.gdal <- GDAL.open(runoff.file)
plot(getRasterTable(runoff.gdal)$band1)



runoff.info <- GDALinfo(runoff.nc)

attr.runoff <- attr(runoff.info,"mdata")

runoff.stack <- stack(runoff.file, varname = "mrros")
runoff.mean  <- mean(runoff.stack)
plot(runoff.mean)

rotate(runoff.stack)

runoff.lon <- raster(runosff.file, varname = "lon")
runoff.lat <- raster(runoff.file, varname = "lat")
lat <- ncvar_get(runoff.nc,"lat")
lon <- ncvar_get(runoff.nc,"lon")
plot(runoff.lat)

ts <- as.POSIXct(nc.get.time.series(runoff.nc))
runoff <- ncvar_get(runoff.nc,"mrros",start = c(1,1,1), count = c(-1,-1,1))

nc_close(runoff.nc)





rot.info <- attr.runoff[grep("^rotated",attr(runoff.info,"mdata"))]
coord <- sapply(rot.info, FUN = function(x) {
  unlist(str_split(x, pattern = "="))[2]})
coord.num<- round(as.numeric(coord[2:3]),2)

y <- coord.num[1]
x <- coord.num[2]

target.crs <- paste0("+proj=ob_tran +o_proj=longlat +o_lon_p=" , 
                     x, " +o_lat_p=", y, 
                     " +lon_0=180 +to_meter=0.0174532925199433")

crs(runoff.mean) <- st_crs(4326)$proj4string #WGS84
crs(runoff.mean) <- CRS(SRS_string = "EPSG:4326")
crs(runoff.mean) <- st_crs(25833)$proj4string #UTM 33

catchment.poly.wgs84 <- st_transform(catchment.poly.1000,st_crs("EPSG:4326")$proj4string)
catchment.poly <- st_transform_proj(catchment.poly.wgs84, crs = target.crs)
ggplot()+geom_sf(data = head(catchment.poly.1000),aes(geometry = geom))
ggplot()+geom_sf(data = head(catchment.poly.wgs84),aes(geometry = geom))
ggplot()+geom_sf(data = head(catchment.poly),aes(geometry = geom))
extent(catchment.poly)

runoff.mean  <- mean(runoff.stack)
extent(runoff.mean)

projection(runoff.mean) <- CRS("EPSG:4326")
reproj.1 <- projectRaster(runoff.mean,crs = target.crs)
plot(reproj.1)
reproj.2 <- reproj.1
projection(reproj.2) <- CRS("EPSG:4326")
extent(reproj.2)
plot(reproj.2)
#europe <- crop(reproj.1,catchment.poly)
#projection(europe) <- CRS("EPSG:4326")
#plot(europe)
#extent(europe)
runoff.60s <- raster::extract(reproj.2,catchment.poly.wgs84, fun = mean, na.rm = T, df = T, exact = F, sp = T)
View(runoff.60s@data)





runoff.spdf <- as(reproj.1, "SpatialPixelsDataFrame")

# get data
dates <- getZ(runoff.stack)
mrros.1995 <- stack(runoff.file,varname = "mrros", bands = grep("1995",dates))
crs(mrros.1995) <- "+init=EPSG:4326"

mean.mrros.1995 <- mean(mrros.1995) 

mean.mrros.1995.nordic <- extract(mean.mrros.1995,catchment.poly.1000)
names(mean.mrros.1995.nordic) <- ebint

mrros.1995.nordic <- lapply(mean.mrros.1995.nordic,mean) %>% unlist() %>% -273.15 %>% as.data.frame() %>% setNames("mrros")
View(mrros.1995.nordic)


# test nor2k
nor2k_sf <- st_as_sf(nor2k,)
ggplot(nor2k_sf)+geom_sf()+borders(regions = "Norway",xlim=c(0,35),ylim=c(55,73))+theme_void()

# using esd
install_github("metno/esd")
library(esd)
runoff.esd <- retrieve(runoff.file)

install_github(c("SantanderMetGroup/loadeR.java",
                 "SantanderMetGroup/climate4R.UDG",
                 "SantanderMetGroup/loadeR",
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/visualizeR",
                 "SantanderMetGroup/downscaleR"))
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_301")
devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/climate4R.UDG", "SantanderMetGroup/loadeR"))


climate4R <- list("loadeR", "transformeR", "downscaleR", "visualizeR", "climate4R.climdex")
lapply(climate4R, require, character.only = TRUE)


# using neumann

remotes::install_github("neumannd/eurocordex2swatTools")
library(eurocordex2swatTools)

toimport <- data.frame(filename = "mrros_EUR-11_NCC-NorESM1-M_historical_r1i1p1_DMI-HIRHAM5_v3_mon_196101-197012.nc",read_idx_start = 1, read_idx_count = 10)

test <- read_eurocordex_files(dir = "CORDEX",fImport = toimport,idxStations = data.frame(), varname = "mrros")
