#Clear Environment
rm(list = ls())
gc()

#Load Packages
require(rgdal)
require(ggplot2)
require(ggmap)
require(dhplot)
require(svglite)

#Load Data
dcbq <- read.csv('Data/DCBQ_output.csv',
                 stringsAsFactors = F)
dcbq <- dcbq[order(dcbq$final_rank),]

#Load map
loc <- apply(dcbq[,c('lng','lat')], 2, mean)
dc <- get_googlemap(loc, zoom = 12,
                    maptype = 'roadmap',
                    scale = 2,
                    size = c(500, 500))

map <- ggmap(dc) + theme(axis.title = element_blank(), axis.text = element_blank()) +
  geom_point(data = dcbq, aes(x = lng, y = lat), alpha = 1, size = 7, color = '#2196f3') +
  geom_path(data = dcbq, aes(x = lng, y = lat), alpha = 1, size = 1, color = '#2196f3') +
  scale_shape_discrete(solid = FALSE) +
  geom_text(data = dcbq, aes(x = lng, y = lat, label = final_rank),
            color = '#f0f0f0', fontface = 'bold',
            vjust = .35, hjust = .5)


svglite(file = 'dcbq.svg', width = 500/72, height = 500/72)
map
dev.off()