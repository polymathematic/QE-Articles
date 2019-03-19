#Clear Environment
rm(list = ls())
gc()

#Load Packages
require(rgdal)
require(ggplot2)
require(ggmap)
require(PlotCC)
require(svglite)

#Load Data
dcbt <- read.csv('Data/dcbt_output.csv',
                 stringsAsFactors = F)
dcbt <- dcbt[order(dcbt$final_rank),]

#Load map
map_area <- estimate_map_area(longitude = dcbt$lng, latitude = dcbt$lat)
dc <- get_mapbox_map(center = map_area$center,
                   zoom = map_area$zoom + 1,
                   size = c(1024,1024),
                   user = "hsysinc",
                   style = "cjle20z107ikb2rpddyozo8qf",
                   access_token = "pk.eyJ1IjoiaHN5c2luYyIsImEiOiJjaXM5OHM0ZzEwMDVtMnpzNGNzZmRoMTE1In0.-9evnwPDQf6eK5DL1iRsqg")

map <- add_map_registration(map = ggmap(dc),
                                   center = map_area$center,
                                   zoom = map_area$zoom + 1, 
                            size = c(1024, 1024))+
  labs(title = toupper("The Washington, DC Beer Trail"),
       subtitle = toupper("The distance-optimized rank order for visiting all of DC's breweries"),
       caption = 'Source: Brewery data compiled by Kate Rabinowitz') +
  generate_theme(map = TRUE) +
  geom_point(data = dcbt, aes(x = lng, y = lat),
             alpha = 1, size = 10, color = create_palette()[2]) +
  geom_path(data = dcbt, aes(x = lng, y = lat),
              alpha = 1, size = 2, color = create_palette()[2]) +
  scale_shape_discrete(solid = FALSE) +
  geom_text(data = dcbt, aes(x = lng, y = lat, label = final_rank),
            color = '#f0f0f0', fontface = 'bold',
            size = 6,
            vjust = .35, hjust = .5)

export_plot(map, "Graphics", "DCBT", width = 1024, height = 1024, export_type = c("PNG","SVG"))
