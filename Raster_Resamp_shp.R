 ## create the raster template for the rectangular grids #####
 
grid_401230 <- as.data.frame(expand.grid(seq(147.70,148.00,0.05),seq(-36.65,-36.10,0.05)))
grid_401230_rs = raster(   xmn = min(grid_401230$Var1) - 0.025, xmx = max(grid_401230$Var1) + 0.025,
                        ymn = min(grid_401230$Var2) - 0.025, ymx = max(grid_401230$Var2) + 0.025,
                        crs = PROJ_LATLON, res= 0.05)   # TEMPLATE
values(grid_401230_rs) = 1       # assign a value 
crop_grid_401230 = crop(grid_401230_rs, extent(shp_401230))   # crop to shapefile extent 
crop_grid_401230 = mask(crop_grid_401230, shp_401230)

length(which(!is.na(values(crop_grid_401230) )))

  #### convert the masked raster into data frame, some grids would be NA values  #####
  
crop_shp_df = as.data.frame(crop_grid_401230, xy = T)  %>% filter(!is.na(layer)) # only select grids with values assigned
 
  ## subset of coordinates that enclose the catchment ### 
  
crop_shp_coord  = SpatialPoints(crop_shp_df[,1:2], proj4string = CRS(PROJ_LATLON ))

coordinates(crop_shp_coord) %>% head()  # check coordinates, can see that they're not continuous 
          x      y
[1,] 147.90 -36.15
[2,] 147.95 -36.15
[3,] 147.80 -36.20
[4,] 147.85 -36.20
[5,] 147.90 -36.20
[6,] 147.95 -36.20
