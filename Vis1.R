### Set fixed colour scale
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))
ggplot(subset(mtcars, am==0), aes(x=wt, y=mpg, colour=carb)) + 
  geom_point(size=6) + sc + graph.theme.beta 