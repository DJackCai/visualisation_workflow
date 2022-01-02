### Task: use a wide type data frame to create a mixture of trend maps ,
## and use a break-point legend for the colour scheme 

## Load essential packages
## We will rely on the layered feature of ggplot2 packages to conduct raster mapping
library(raster)
library(RColorBrewer)  # for customised colour definition
library(ggplot2)  

mycolor<-rev(brewer.pal(10,"Spectral"))  ## red to blue colour scheme suits temperature data 

Afreq_below12<-as.data.frame(read.table("AFreq_all_Below12.txt"))

## See the data shape
> head(Afreq_below12[,1:15],3)

Point    lon   lat 1910 1911 1912 1913 1914 1915 1916 1917 1918 1919
1 152.55,-28.5 152.55 -28.5    0    0    0    0    0    0    0    0    0    0
2  152.6,-28.5 152.60 -28.5    0    0    0    0    0    0    0    0    0    0
3 152.65,-28.5 152.65 -28.5    0    0    0    0    0    0    0    0    0    0
1920 1921
1    0    0
2    0    0
3    0    0

### See that each row is a point (cross-section) while each column represents a value 
## at one year, so the columns form a time series.

# This is a "Wide" type dataframe. 
# For more straightforward DATA PROFILING, we need to create a column variable for "Year" 
# This means we will have the values from 1910-2020 in *one* column, so each point will replicate 111 times 

# This becomes a "Long" type data which is less organised for distinguishing points, but 
# easier for subsetting and profiling

# We can use reshape() function, however to make it general to panel data analysis,
# I will use the reshape functionality in the panelr package

library(panelr)
# the column name needs to have a common "identifier" 
names(Afreq_below12)<-c("lon","lat",c(paste0("X",(1910:2020))))
# reshape to combine lat and lon as a point, distinguish each observation 
Afreq_below12_reshp<-Afreq_below12%>%unite("Point",lon:lat,remove=T,sep=",")

# Convert to long type data
long_Afreq_below12 <-long_panel(Afreq_below12_reshp,periods = c(1910:2020),label_location = "end",id="Point")

#### Construct decadal mean data frame

### Step 1: Extract the re-arranged point coordinates 
# (Note: will be different from the original data)
long_points<-long_Afreq_below10%>%group_by(Point)%>%dplyr::filter(wave %in% 1911:1920)%>%
  summarise(dec_mean=mean(X))%>%select(Point)%>%as.data.frame()
Dec_mean_b10.DF<-Dec_mean_b12.DF<-long_points

### Step 2: Loop through the decades by "filter" and compute the decadal mean,
# then append to the columns of points 
for (i in 1:11) {
  values_b12 <- long_Afreq_below12%>%group_by(Point)%>%dplyr::filter(wave %in% c( (1910+(i-1)*10+1) :(1910+(i-1)*10+10) ))%>%
    summarise(dec_mean=mean(X))%>%dplyr::select(dec_mean)%>%as.data.frame()
  Dec_mean_b12.DF<-data.frame(cbind(Dec_mean_b12.DF,values_b12))
 
}

## Define column names 
names(Dec_mean_b12.DF)[1]<-"Point"
names(Dec_mean_b12.DF)[-1] <- paste0("decmean",1:11)

### Step 3: Conversion to raster

## Prepare data frame with x and y

## Separate coordinate to lon and lat again
Dec_mean_b12.DF.v2 <- Dec_mean_b12.DF%>%separate(col = Point,into=c("lon","lat"),sep=",")%>%mutate_at(vars(lon,lat),as.numeric)

# Compute decadal trend

Dec_mean_b12.trend.DF <- Dec_mean_b12.DF.v2 %>% mutate(Trend_from1910 = (decmean11-decmean1)/10,
                                                       Trend_from1970 = (decmean11-decmean7)/4) %>%
  select(c(lon,lat,Trend_from1910,Trend_from1970) ) 

# Give column names x and y
names(Dec_mean_b12.trend.DF)[1:2]<-names(Dec_mean_b10.trend.DF)[1:2]<-c("x","y")

## Convert data frame to raster by the template 
r_template_SEA<-raster(xmn=141.0-0.025,xmx=153.6+0.025,
                       ymn=-39.5-0.025,ymx=-27.8+0.025,crs=PROJ_LATLON,resolution=c(0.05,0.05))

Map_b12_SEA_from1910 <- rasterize(Dec_mean_b12.trend.DF[,c("x","y")],r_template_SEA,Dec_mean_b12.trend.DF$Trend_from1910,fun=mean,na.rm=T)

## Step 4: Mapping

## ggplot2() still takes in the data frame as the building block

## Use quantile summary to determine suitable cutting values 
quantile(Map_b12_SEA_from1970[],prob=seq(0,1,0.1),na.rm=T)
Breaks_1970 <- c(-Inf, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5,0.6, Inf)

## Here, cut() function is used within ggplot2 to group the continous values into multiple classes.
###  n Breakpoints, n+1 classes, n+2 in defining breaks (need -inf and inf)


## Convert background shapefile to sf object for quicker graphing 
SEAbound.sf<-st_as_sf(SEAbound)

## Graphical elements 
graph.theme.beta<-theme(axis.title=element_text(size=12,vjust=-1),axis.text = element_text(size=10),plot.title = element_text(hjust=0.5,size=13,vjust=0.6),legend.text = element_text(size=10.5),
                        plot.margin = unit(c(0.3,0.1,0.1,0.1), "cm"),
                        legend.title = element_text(size=12))

gplot_b12_trend1910 <-ggplot(data = Dec_mean_b12.trend.DF)  +  
  ## Cut the continous values to class
  geom_raster(aes(x=x,y=y,fill=cut(Trend_from1910, Breaks_1970, right = T))) + 
  ## Each class matches a colour - the last is removed 
   scale_fill_manual(values=mycolor, name="Days/Decade",labels=c("-0.2","-0.1","0","0.1","0.2","0.3","0.4","0.5","0.6","")) + 
  ##  Allow the "Break point" legend by vjust
  theme_bw() +   guides(fill = guide_legend(label.vjust = -0.25)) + 
  labs(title="Trend of strong front frequency from 1910s ",x="Longitude",y="Latitude") + scale_x_continuous(limits=c(140,155))+
  ## OVERLAY THE SHAPEFILE POLYGON
  geom_sf(data=SEAbound.sf,fill=NA,col="black",size=0.25)+ graph.theme.beta

gplot_b12_trend1970 <- ggplot()  +  geom_raster(data = Dec_mean_b12.trend.DF, 
                                                aes(x=x,y=y,fill=cut(Trend_from1970, Breaks_1970, right = T))) + 
  scale_fill_manual(values=mycolor, name="Days/Decade",labels=c("-0.2","-0.1","0","0.1","0.2","0.3","0.4","0.5","0.6","")) + 
  theme_bw() + guides(fill = guide_legend(label.vjust = -0.25)) + 
  labs(title="Trend of strong front frequency from 1970s ",x="Longitude",y="Latitude") + scale_x_continuous(limits=c(140,155))+
  geom_sf(data=SEAbound.sf,fill=NA,col="black",size=0.25)+ graph.theme.beta 


### Combine into one plot with a common legend by ggarrange()
library(ggpubr)
ggarrange(gplot_b12_trend1910,gplot_b12_trend1970,ncol=2,nrow=1,common.legend = T,legend="right")


