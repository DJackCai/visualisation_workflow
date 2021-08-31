#### 1. Set fixed colour scale and use values of one variable to show the colour gradient 
## Suits: target of 3 continous variables, one variable acting as additional information of one distinguishing property of each observation
library(RColorBrewer)
library(ggplot2)
## Set-up graphics for axis ticks and labels for clearer appreance
graph.theme.beta<-
  theme_classic()+theme(axis.title=element_text(size=14),axis.text = element_text(size=11),plot.title = element_text(hjust=0.5,size=14),legend.text = element_text(size=12),legend.title = element_text(size=14))
### Set-up the colour gradients
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
### Set limits for the colour - anything above 8 will be red. 
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))
# Make the plot
ggplot(subset(mtcars, am==0), aes(x=wt, y=mpg, colour=carb)) + 
  geom_point(size=6) + sc + graph.theme.beta 
