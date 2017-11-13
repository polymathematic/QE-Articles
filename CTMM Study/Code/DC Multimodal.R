#Clear Environment
rm(list = ls())
gc()

#Load Packages
require(rgdal)
require(ggplot2)
require(MASS)
require(reshape2)
require(ggmap)
require(hexbin)
require(dhplot)
require(geosphere)
require(svglite)

#Read Map
dcmm <- readOGR(dsn = paste0(getwd(),"/Data"),
                layer = 'Crosstown Multimodal Study.20160609_points',
                stringsAsFactors = FALSE)

#Create Data Frame
dcmm_df <- data.frame(dcmm@data,
                      dcmm@coords)

#Colors and Categories
colors <- c(blue = '#2196f3',
  orange = '#ff9800',
  green = '#4caf50',
  red = '#f44336',
  purple = '#673ab7')

categories <- c('Biking',
                'Intersection',
                'Congestion',
                'Parking',
                'Pedestrians',
                'PublicSpace',
                'Transit')


#Create binaries from comment categories
dcmm_df <- cbind(dcmm_df,model.matrix(~ 0 + kml_name,dcmm_df))
names(dcmm_df)[-(1:13)] <- categories

dcmm_df$Cars <- dcmm_df$Congestion + dcmm_df$Parking

#Load base map
loc <- apply(dcmm@coords, 2, median)

dc <- get_googlemap(loc, zoom = 14,
                     maptype = 'roadmap',
                     scale = 2,
                     size = c(640, 320),
                     style = dh_mapStyle())

#Map heat layer
heatLayer <- function(var = NULL,color = 'blue'){
  if(is.null(var)){
    indx <- rep(TRUE, nrow(dcmm_df))
  }else{
    indx <- dcmm_df[,var] == 1
  }
  temp <- stat_density2d(data = dcmm_df[,],
                        aes(x = coords.x1,
                            y = coords.x2,
                            alpha = ..level..),
                        fill = colors[color],
                        geom = "polygon",
                        bins = 10,
                        h = c(.002,.002))
  return(temp)
}

# Base Map
base <- ggmap(dc) + dh_map() + coord_cartesian() 
bikes <- base + heatLayer(var = 'Biking', color = 'blue')
peds <- base + heatLayer(var = 'Pedestrians', color = 'orange')
cars <- base + heatLayer(var = 'Cars', color = 'green')
transit <- base + heatLayer(var = 'Transit', color = 'red')
bikes 
peds
cars
transit

#Determine Bandwidth and KDE output matrix side
#1 mile = 1609.34 meters
#1 degree lat = 54.6 miles
#1 degree long = 69 miles)
meters <- 300
areadim <- c()

areadim['EW'] <- (max(dcmm_df$coords.x1) - min(dcmm_df$coords.x1)) * 54.6 * 1609.34
areadim['NS'] <- (max(dcmm_df$coords.x2) - min(dcmm_df$coords.x2)) * 69 * 1609.34
areadim <- floor(areadim/40) #Output matrix has a resolution of 40m
bandwidth <- (((meters)/1609.34)/((54.6 + 69)/2))

#Create density estimate
dcmm_kde <- kde2d(dcmm_df$coords.x1, dcmm_df$coords.x2, n = areadim, h = bandwidth)

#Find local maxima
maxima <- function(x){
  temp <- rep(FALSE, length(x))
  temp[which(diff(sign(diff(x)))==-2)+1] <- TRUE
  return(temp)
}

maxima_long <- apply(dcmm_kde$z,1,maxima)
maxima_lat <- apply(dcmm_kde$z,2,maxima)
maxima_comb <- t(maxima_long) & maxima_lat

maxima_values <- melt(dcmm_kde$z)
names(maxima_values) <- c('long','lat','density')

maxima_values$long <- dcmm_kde$x[maxima_values$long]
maxima_values$lat <- dcmm_kde$y[maxima_values$lat]

maxima_comb <- melt(maxima_comb)
maxima_comb <- maxima_values[maxima_comb$value,]

#Apply threshold
maxima_comb <- maxima_comb[maxima_comb$density >= 5000,]

#Identify relevant comments
d = 50
dmat <- distm(dcmm_df[,12:13],maxima_comb[,1:2])
dmat <- dmat <= d
colnames(dmat) <- LETTERS[1:ncol(dmat)]
dcmm_df <- cbind(dcmm_df, dmat, inAny = apply(dmat, 1, any))
head(dcmm_df)

#Plot maxima
maxkde <- base + stat_density2d(data = dcmm_df,
                                aes(x = coords.x1,
                                    y = coords.x2,
                                    alpha = ..level..),
                                fill = colors['blue'],
                                geom = "polygon",
                                bins = 10,
                                h = c(.002,.002)) +
  geom_point(aes(long, lat),
             color = '#212121',
             size = 20,
             shape = 1,
             data = maxima_comb) +
  ggtitle("Crosstown Transportation Pain Points",
          "Areas in the DC Crosstown Tranportation Study with the most public commentary") +
  labs(caption = 'SOURCE: DDOT')
maxkde

svglite(file = 'maxkde_2.svg',
        width = 840/72,
        height = 525/72)
maxkde
dev.off()

