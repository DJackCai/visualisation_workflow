## useful reference: https://rpubs.com/huanfaChen/ggplotShapefile

## read shapefile 
statebord<-shapefile("StateBoundaries.shp")

    ## add a factor indicating SEA to the shapefile's attribute table ####
SEA_names = c("New South Wales", "Victoria", "Australian Capital Territory")
statebord@data = statebord@data %>% 
                  mutate(id = rownames(.)) %>%
                  mutate(SEA = as.factor(ifelse(NAME %in% SEA_names, "Yes","No")))

 ## convert spatial feature to a data frame for spatial polygon, using id as region
 
statebord_df = broom::tidy(statebord, region = "id")  # spatial polygon data frame

# head(statebord_df) ## each polygon defined by lots of positions, distinguished by id 
# A tibble: 6 × 7
   long   lat order hole  piece group id   
  <dbl> <dbl> <int> <lgl> <fct> <fct> <chr>
1  141. -26.0     1 FALSE 1     0.1   0    
2  141. -26.0     2 FALSE 1     0.1   0    
3  141. -26.0     3 FALSE 1     0.1   0    
4  141. -27.7     4 FALSE 1     0.1   0    
5  141. -28.5     5 FALSE 1     0.1   0    

 ## join the shp data to the attribute data to add the extra factor variable, through id ####

statebord_df = statebord_df %>% left_join(statebord@data, by = c('id' = 'id'))

# bord_f = fortify(statebord, region = "OBJECTID")
 
 ### Create map, with southeast Australia being the coloured site  ##### 
 # -> group = group, fill = factor
ggplot() + geom_polygon(data = statebord_df, aes(x = long, y = lat, 
                        group = group, fill =  SEA), col = "black") +
        scale_fill_manual(values = c("white", "red")) +
        theme_void()  + theme (legend.position = "none") # no need for lat/lon


