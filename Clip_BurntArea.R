# read raster file 

burnt_SepFeb = raster("./BurntArea_BS/spring-summer 2019-2020.tif")

# clip to southeast Australia polygon: crop + mask 

statebord<-shapefile("StateBoundaries.shp")
SEAbound<-statebord[c(4,5,8),]  # shapefile format


BA_r1 = crop(burnt_SepFeb, extent(SEAbound))
BA_r2 = mask(BA_r1, SEAbound)


## aggregate/resampled to 0.05 * 0.05 AGCD grids
SEA_template = raster(xmn = 140.975, xmx = 153.625, 
                      ymn = -39.125, ymx =  -28.175, resolution = 0.05,
                      crs = "+proj=longlat +datum=WGS84 +no_defs") 
                      
BA_r2_agg = raster::resample(BA_r2, SEA_template, method = "ngb")

# convert to data frame for plotting

BA_SEA_agg_df = as.data.frame(BA_r2_agg,xy = TRUE )

 #### filter NA value in the data frame ###
 BA_SEA_sub = BA_SEA_df %>% rename(burnt = spring.summer_2019.2020, lon = x, lat = y) %>%
                filter(!is.na(burnt))
 
 BA_SEA_agg_sub = BA_SEA_agg_df %>% rename(burnt = spring.summer_2019.2020, lon = x, lat = y) %>%
   filter(!is.na(burnt))
