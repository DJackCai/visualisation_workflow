 ### plot global raster and address the blank background issue in R

library(raster)
beta_r = raster("Beta_Opt_Global_10km.tif")
mywindow = extent(-160,170,-65, 65)

# first set a blank plot as a canvas to limit the range of raster plotting
plot(mywindow, col = NA,
     # xlab = "Longitude",  ylab = "Latitude", 
     xlab = "", ylab = "", 
     cex.lab = 1.3, cex.axis = 1.2,
     xaxt = "n", yaxt = 'n',axes=F)
par(xpd = TRUE)

# add the raster layer, and specify the colour ramp
plot(beta_r, add = T,  
     #legend.args = list(text = expression(beta),line = 0.5, 
      #                                     adj = 0.2, cex = 1.4), 
     legend.args = list(text = expression(beta), line = 0.5, cex = 1.4), 
     col = rev(terrain.colors(100)),
     horizontal = TRUE, 
     # set limits of colour ramp 
     zlim = c(0,1)) 
     
