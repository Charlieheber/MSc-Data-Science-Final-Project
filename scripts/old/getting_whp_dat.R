# 
# hazard_data_file_loc <- paste0(here::here(), "/data/hazard data/")
# whp_2020_gdb <- paste0(hazard_data_file_loc, "whp_2020/Data/whp2020.gdb")
# whp_2020_tif <- paste0(hazard_data_file_loc, "whp_2020/Data/whp2020_GeoTIF/")
# 
# # ogrListLayers(whp_2020_gdb)
# # st_layers(whp_2020_gdb)
# 
# # whp_2020_cls_shp <- readOGR(whp_2020_gdb,"whp2020_cls_conus")
# # whp_2020_cls_shp <- st_read(whp_2020_gdb,"whp2020_cnt_conus")
# 
# # whp_2020_cls_shp
# # 
# # whp_2020_cls_shp@proj4string
# # plot(whp_2020_cls_shp)
# 
# whp_2020_cls_rast <- raster::raster(paste0(whp_2020_tif, "whp2020_cls_conus.tif"))
# # whp_2020_cls_rast@crs
# # coordinates(raster(whp_2020_cls_rast))
# 
# ## Define the function
# whp_2020_cls_shp <- rasterToPoints(whp_2020_cls_rast, spatial=TRUE)
# whp_2020_cls_shp@data
# 
# whp_2020_cls_df <- raster::as.data.frame(whp_2020_cls_rast)
# colnames(whp_2020_cls_df)
# max(whp_2020_cls_df, na.rm=TRUE)
# 

hazard_data_file_loc <- paste0(here::here(), "/data/hazard data/")
whp_2020_gdb <- paste0(hazard_data_file_loc, "whp_2020/Data/whp2020.gdb")
whp_2020_tif <- paste0(hazard_data_file_loc, "whp_2020/Data/whp2020_GeoTIF/")

ogrListLayers(whp_2020_gdb)
st_layers(whp_2020_gdb)

whp_2020_cls_shp <- readOGR(whp_2020_gdb,"whp2020_cls_conus")
whp_2020_cls_shp <- st_read(whp_2020_gdb,"whp2020_cnt_conus")

# read in whp 2020 raster
whp_2020_cls_rast <- raster::raster(paste0(whp_2020_tif, "whp2020_cls_conus.tif"))

# transform raster to spdf
whp_2020_cls_shp <- rasterToPoints(whp_2020_cls_rast, spatial=TRUE)
whp_2020_cls_shp@data

# reproject spdf to lat/lons
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
whp_2020_cls_shp <- spTransform(whp_2020_cls_shp, CRS(geo.prj)) 
proj4string(whp_2020_cls_shp)

# extract lat lons and whp classes as a data.frame
whp_2020_cls_df <- data.frame(whp_2020_cls_shp@data, long=coordinates(whp_2020_cls_shp)[,1],
                              lat=coordinates(whp_2020_cls_shp)[,2])                         
head(whp_2020_cls_df)

# save result
write.csv(whp_2020_cls_df, paste0(hazard_data_file_loc, "whp_cls_conus.csv"))



