library(raster); library(rgdal); library(abind)

### Table array (ordered) to raster
PET<-read.table("etapann 2.txt",skip = 6)  # example of a table downloaded from BoM
PET_rst<-raster(as.matrix(PET),crs="+proj=longlat +datum=WGS84",xmn=112.15,xmx=112.15+417*0.1,ymn=-43.8,ymx=-43.8+337*0.1)

### Shapefile 
statebord<-shapefile("StateBoundaries.shp")
crs(statebord)      ## check the coordinate systems of shapefile (vector data) /raster (field data) 

# in shapefile, each row represents a polygon, so extract by rows 
SEAbound<-statebord[c(4,5,8),]

#### Raster data structure manipulation
# always remember, raster is one point - extrapolate to the whole GRID 

## 1) Coordinates and data to raster 

dat<-list(x=lon2019,y=lat2019,z=temp_all_diff_v2[,,123])   # create a list, and specify xyz
rst1<-raster(dat,crs="+proj=longlat +datum=WGS84 +no_defs ")   # convert list to raster

### Note: to make the central point in the 0.05, the xmn/xmx and ymn/ymx need to be 0.025 wider
> range(lon2019);range(lat2019)  # range of the points
[1] 112.00 156.25
[1] -44.5 -10.0
> rst1    ## extent of raster
class      : RasterLayer 
dimensions : 691, 886, 612226  (nrow, ncol, ncell)
resolution : 0.05, 0.05  (x, y)
extent     : 111.975, 156.275, -44.525, -9.975  (xmin, xmax, ymin, ymax)

##### 2) Extract points inside a polygon from the raster brick
# -- If single raster (e.g. a GIS map project)

rst_sub<-mask(rst1,SEAbound,inverse=F,updatevalue=NA)  ## points outside the polygon is in NA

# -- If a raster brick:

# step 0: get NETCDF data by looping through years (if needed)
path2Tmax<-"agcd_v1_tmax_mean_r005_daily_"
Years<-c(2018,2019)
for (k in 1:length(Years)) {
  Tmax.open<-nc_open(paste0(path2Tmax,Years[k],".nc"))
  temps<-ncvar_get(Tmax.open,varid="tmax")
  ### append data for year k 
  tmax_allyears<-abind(tmax_allyears,temps,along=3)
}

# step 1: convert netcdf array to raster brick
# need to take transpose since columns of rasters are longitudes, which are rows of the array

PROJ_LATLON<-"+proj=longlat +datum=WGS84"
Tmax_brick<-brick(tmax_allyears,xmn=111.975,xmx=156.275,
                  ymn=-44.525,ymx=-9.975,crs=PROJ_LATLON,transpose=T)

# Step 2: as NETCDF file records data from bottom left (PS: Check the order of lon & lat in NETCDF file, normally from small to large values)
# we need to flip it, so that the "uppermost" point in data matrix (Lat is smallest) is actually in the southmost 

Tmax_brick<-flip(Tmax_brick,direction = "y")

# Step 3: Use the polygon to mask the area - RETURN A MASKED $raster object$

Mask_Tmax_brick_yrs<-mask(Tmax_brick,SEAbound,inverse=F,updatevalue=NA)   

# Step 4: Convert raster brick to data frame and extract points inside polygon 

Mask_Tmax_brick_yrs.df = as.data.frame(Mask_Tmax_brick_yrs, xy=TRUE)
Mask_Tmax_brick_yrs.df<-Mask_Tmax_brick_yrs.df[complete.cases(Mask_Tmax_brick_yrs.df),]  # complete.cases(): remove NA points (outside polygon)


## This returns a subset of points inside the polygon. We can do the calculation on the raster time series

# e.g. differencing of the data - need the transpose so that ** EACH ROW MATCHES the series of ONE GRID POINT **

Mask_diff_yrs<-t(apply(Mask_Tmax_yrs_values,1,function(x) {diff(x,lag = 1,differences = 1)}))
Mask_diff_yrs_df<-data.frame(Mask_diff_yrs)

### PS:  Extract comma separated colnames for calculation invovling multiple columns in tidyverse framework 
colname.layers<-noquote(paste(names(Mask_diff_yrs_df),collapse = ","))  ## convert to things like (layer1,layer2,layer3), can be passed to %>% calculation

Mask_diff_yrs_df<-Mask_diff_yrs_df%>%mutate(Ex_prob=mean(BS_most>c(colname.layers)))  # can exactly do the calculation combining hundreds of columns

## Step 5: Convert the desired statistic back to raster
# add coordinate 
Mask_diff_yrs_df$x<-Mask_Tmax_brick_yrs.df$x
Mask_diff_yrs_df$y<-Mask_Tmax_brick_yrs.df$y

# set up raster template 
r_template_SEA<-raster(xmn=141.0-0.025,xmx=153.6+0.025,
                       ymn=-39.5-0.025,ymx=-27.8+0.025,crs=PROJ_LATLON,resolution=c(0.05,0.05))

# convert back to raster by rasterize() command

Map_exceed_SE<- rasterize(Mask_diff_yrs_df[,c("x","y")],r_template_SEA,Mask_diff_yrs_df$layer.4,fun=mean)

# write raster
writeRaster(Map_exceed_SE,"BS_ExceedSEA_5km.tif")
