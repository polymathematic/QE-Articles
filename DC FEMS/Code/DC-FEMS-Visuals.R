#Clear Environment
rm(list = ls())
gc()

#Load Packages
require(lubridate)
require(ggmap)
require(rgdal)
require(maptools)
require(sp)
require(ggplot2)
require(dhplot)
require(svglite)
require(reshape2)
require(scales)

#load fonts
extrafont::font_import(pattern = "OpenSans", prompt = FALSE)
extrafont::loadfonts(device = "win")

#Load Data and maps
fems <- read.csv('../Data/FEMS DATA (APR14-16).csv', stringsAsFactors = FALSE)
stations <- read.csv('../Data/FirehouseData.csv', stringsAsFactors = FALSE)
soa_ct <- scan('../Data/Census Tracts South of the Anacostia.txt', what = 'character')
censusTracts <- readOGR('../Maps/CensusTracts.geojson',
                        'OGRGeoJSON')
FA_Districts <- readOGR('../Maps/FireAlarmDistricts.geojson',
                        'OGRGeoJSON')

#Project maps
proj <- CRS('+init=epsg:4326 +proj=longlat +ellps=WGS84')
censusTracts <- spTransform(censusTracts, proj)
FA_Districts <- spTransform(FA_Districts, proj)

#Subset stations
stations <- stations[stations$OBJECTID %in% 1:33,]

#Convert variables
fems$TYPE <- as.factor(fems$TYPE)
fems$Critical_NON <- as.factor(fems$Critical_NON)
fems$UNID <-as.factor(fems$UNID)

fems$DP_date_time <- as.POSIXct(fems$DP_date_time, format = "%m/%d/%Y %H:%M")
fems$ER_date_time <- as.POSIXct(fems$ER_date_time, format = "%m/%d/%Y %H:%M")
fems$AR_date_time <- as.POSIXct(fems$AR_date_time, format = "%m/%d/%Y %H:%M")

#Create time dimensions
fems$y <- year(fems$DP_date_time)
fems$hr <- hour(fems$DP_date_time)
fems$wd <- wday(fems$DP_date_time)

#Subset to critical calls
fems <- fems[fems$Critical_NON == 'CRITICAL',]

#Create response time variables
#A similar project indicates FEMS uses 1 to 30 minutes as thresholds to eliminate outliers and errors
#https://hackpad.com/ep/pad/static/DmrRNw6nxqj
#The lower bound of 1 minute seems reasonable, but the 30m upper bound seems short given the response distribution is smooth through that point
timecalc <- function(timeA,
                     timeB,
                     upperLim = 40,
                     lowerLim = 1){
  temp <- as.numeric(difftime(timeA, timeB, units = 'mins'))
  temp <- ifelse(temp > upperLim | temp < lowerLim,
                 NA,
                 temp)
  return(temp)
}

fems$pt <- timecalc(fems$ER_date_time, fems$DP_date_time)
fems$tt <- timecalc(fems$AR_date_time, fems$ER_date_time)
fems$rt <- timecalc(fems$AR_date_time, fems$DP_date_time)

#Identify key breaks (10 and 15 minutes)
#10 minutes or better fgor 90% of calls is DC's contractual standard with AMR 
fems$time_10plus <- ifelse(fems$rt > 10, 1, 0) 

#15 minutes seems to be another threshold in this document:
#http://media.nbcwashington.com/documents/AMR+Response+Time+Charts+(Week+02+2016-04-04).pdf
fems$time_15plus <- ifelse(fems$rt > 15, 1, 0) 

#Remove incorrect and incomplete records
fems <- fems[fems$Longitude < 0,]
fems <- fems[fems$Latitude > 0,]
fems <- fems[year(fems$DP_date_time) != 1969,]
fems <- fems[!is.na(fems$AR_date_time),]
fems <- fems[!is.na(fems$ER_date_time),]
fems <- fems[fems$AR_date_time >= fems$ER_date_time,]
fems <- fems[!is.na(fems$time_10plus),]

#Group calls by census tract, FA_District, and side of the Anacostia River
fems_sp <- fems
coordinates(fems_sp) <- c("Longitude","Latitude")
proj4string(fems_sp) <- proj
fems <- cbind(fems,
              ct = over(fems_sp, censusTracts)[,'TRACT'],
              fad = over(fems_sp, FA_Districts)[,'OBJECTID'],
              stringsAsFactors = FALSE)

fems$anacostia <- as.factor(ifelse(fems$ct %in% soa_ct, 'South', 'North'))

#Remove NA's
fems <- fems[!is.na(fems$ct),]

#Group CTs by side of the Anacostia
censusTracts@data$Anacostia <- ifelse(censusTracts@data$TRACT %in% soa_ct, 'South', 'North')

#Compute aggregate metrics by CT
metrics_ct <- list()

#Mean response time
metrics_ct[['meanRespTime']] <- tapply(fems$rt,
                                       fems$ct,
                                       function(x) mean(x, na.rm = TRUE))

#Proportion of calls over 10 minutes
metrics_ct[['pcntOver10']] <- tapply(fems$time_10plus, 
                                     fems$ct,
                                     function(x) sum(x, na.rm = TRUE)/length(x))

#Proportion of calls over 10 minutes (binned)
metrics_ct[['pcntOver10Bins']] <- cut(metrics_ct[['pcntOver10']], 
                                      breaks = c(0, .1, .2, .3, 1),
                                      labels = c('<10%', '10 to 20%', '20 to 30%', '>30%'),
                                      include.lowest = TRUE)
names(metrics_ct[['pcntOver10Bins']]) <- names(metrics_ct[['pcntOver10']])

#Proportion of calls over 15 minutes
metrics_ct[['pcntOver15']] <- tapply(fems$time_15plus, 
                                     fems$ct,
                                     function(x) sum(x, na.rm = TRUE)/length(x))

#Proportion of calls over 15 minutes (binned)
metrics_ct[['pcntOver15Bins']] <- cut(metrics_ct[['pcntOver15']], 
                                      breaks = c(0, .1, .2, 1),
                                      labels = c('<10%', '10 to 20%', '>20%'),
                                      include.lowest = TRUE)
names(metrics_ct[['pcntOver15Bins']]) <- names(metrics_ct[['pcntOver15']])

#Total number of calls
metrics_ct[['nCalls']] <- tapply(fems$Resp_number,
                                 fems$ct,
                                 function(x) length(x))

#Convert to data frame
metrics_ct <- do.call(data.frame, metrics_ct)

#Areas with few calls should be dismissed
metrics_ct$pcntOver10Bins_adj <- metrics_ct$pcntOver10Bins
levels(metrics_ct$pcntOver10Bins_adj) <- c(levels(metrics_ct$pcntOver10Bins_adj),"Insufficient Data")
metrics_ct$pcntOver10Bins_adj[metrics_ct$nCalls < 15] <- "Insufficient Data"

#Check outputs
summary(metrics_ct)

# Create Census Tract geometry
fems_ct <- fortify(censusTracts, region = "TRACT") 
fems_ct <- cbind(fems_ct, metrics_ct[fems_ct$id,])
fems_ct <- fems_ct[order(fems_ct$order),]

#Create Fire Alarm District Overlay
fems_fad <- fortify(FA_Districts, region = "OBJECTID")
fems_fad <- fems_fad[order(fems_fad$order),]

#Create Anacostia Overlay
fems_ana <- fortify(censusTracts, region = "Anacostia") 
fems_ana <- fems_ana[order(fems_ana$order),]

#Query Basemap (optional)
loc <- c(-77.0289881, 38.8986599)
dc <- get_googlemap(loc, zoom = 11,
                    maptype = 'roadmap',
                    size = c(640, 640),
                    scale = 2,
                    style = dh_mapStyle(),
                    extent="normal")
dc_basemap <- ggmap(dc)

#Map of CTs by >10min bins (Try using FAD instead of CT and adding the Engine number instead of the station location)
map_10minPrcntBins <- ggplot() +
  dh_map(legend = TRUE) +
  ggtitle('Ambulance Response Times in DC (2014-2016)',
          "Proportion of all critical call responses completed in over 10 minutes") +
  labs(caption = 'SOURCE: DC FEMS') +
  coord_cartesian() +
  coord_fixed() +
  coord_map("lambert", lat0=31,lat1=40) +
  geom_polygon(data = fems_ct,
               aes(x = long,
                   y = lat, 
                   group = group,
                   fill = factor(pcntOver10Bins_adj))) +
  discrete_scale('fill', 'GMD', palette = function(n) {c('#2196f3',
                                                         '#82C4F8',
                                                         '#FFC166',
                                                         '#ff9800',
                                                         '#b6b6b6')[1:n]},
                 na.value = '#b6b6b6') +
  geom_polygon(data = fems_ana,
               aes(x = long,
                   y = lat, 
                   group = group), fill = NA, size = 1, color = '#f1f1f1')

map_10minPrcntBins

#North vs South of the Anacostia
fems_soa <- aggregate(fems$time_10plus,
                      by = list(Year = fems$y, soa = fems$anacostia),
                      function(x) sum(x, na.rm = TRUE)/length(x))

#Split bins for a stacked bar chart
fems_soa$bin1 <- sapply(fems_soa$x, function(x) max(min(x,.1),0))
fems_soa$bin2 <-  sapply(fems_soa$x, function(x) max(min(x-.1,.1),0))
fems_soa$bin3 <-  sapply(fems_soa$x, function(x) max(min(x-.2,.1),0))
fems_soa$bin4 <- sapply(fems_soa$x, function(x) max(min(x-.3,.7),0))

fems_soa <- melt(fems_soa, id.vars = c('Year', 'soa', 'x'))
levels(fems_soa$variable) <-  c('<10%', '10 to 20%', '20 to 30%', '>30%')

#Plot bar charts
fems_bar <- function(df){
  
  temp <- ggplot(data = df,
                         aes(x = Year,
                             y = value,
                             fill = variable)) +
  geom_bar(stat="identity", size = 10) +
  dh_bar(gridlines = TRUE, legend = TRUE, ylab = FALSE, xlab = FALSE) + 
  scale_y_continuous(breaks = (0:6) * .05,
                     limits = c(0,.305),
                     labels = percent,
                     expand = c(0,0)) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  discrete_scale('fill', 'GMD',
                 palette = function(n) {c('#2196f3',
                                          '#82C4F8',
                                          '#FFC166',
                                          '#ff9800',
                                          '#b6b6b6')[1:n]},
                 na.value = '#b6b6b6') 
  return(temp)}

bar_soa <- fems_bar(fems_soa[fems_soa$soa == 'South',])
bar_noa <- fems_bar(fems_soa[fems_soa$soa == 'North',])
bar_soa
bar_noa

# Plot histograms
fems_hist <- function(df){
  temp_plot <- ggplot() +
    geom_histogram(data = df[df$rt <= 10,], aes(x = rt),
                   fill = '#2196f3',
                   binwidth = 1,
                   size = 100) + 
    geom_histogram(data = df[df$rt > 10,], aes(x = rt),
                   fill= '#ff9800',
                   binwidth = 1,
                   size = 100) +
    scale_x_continuous( breaks = (0:6)*5,limits = c(0,40)) +
    dh_bar(gridlines = TRUE)
  return(temp_plot)
}

fems_hist_soa <- fems_hist(fems[fems$ct %in% soa_ct,])
fems_hist_noa <- fems_hist(fems[!(fems$ct %in% soa_ct),])
fems_hist_soa
fems_hist_noa

#Time of Day plot
fems_time <- aggregate(fems$time_10plus[fems$y == 2016],
                        by = list(hr = fems$hr[fems$y == 2016],
                                  soa = fems$anacostia[fems$y == 2016]),
                        function(x) sum(x, na.rm = TRUE)/length(x))

fems_time$bin1 <- sapply(fems_time$x, function(x) max(min(x,.1),0))
fems_time$bin2 <-  sapply(fems_time$x, function(x) max(min(x-.1,.1),0))
fems_time$bin3 <-  sapply(fems_time$x, function(x) max(min(x-.2,.1),0))
fems_time$bin4 <- sapply(fems_time$x, function(x) max(min(x-.3,.7),0))

fems_time <- melt(fems_time, id.vars = c('hr', 'soa', 'x'))
levels(fems_time$variable) <-  c('<10%', '10 to 20%', '20 to 30%', '>30%')

fems_radial <-  function(df){
  temp_plot <- ggplot(data = df, aes(x = hr, y = value, fill = variable)) +
    geom_bar(stat = 'identity') +
    coord_polar() + 
    
    dh_radial(gridlines = TRUE, nbreaks = 24) +
    scale_y_continuous(breaks = (0:8) * .05,
                       limits = c(0,.354),
                       labels = percent,
                       expand = c(.1,0)) +
    scale_x_continuous(breaks = 0:23,
                       expand = c(.0025,0),
                       labels = function(t) paste0(t,':00')) +
    discrete_scale('fill', 'GMD',
                   palette = function(n) {c('#2196f3',
                                            '#82C4F8',
                                            '#FFC166',
                                            '#ff9800',
                                            '#b6b6b6')[1:n]},
                   na.value = '#b6b6b6') +
    theme(axis.text = element_text(size = 20)) 
  return(temp_plot)
}

radial_soa <- fems_radial(fems_time[fems_time$soa == 'South',])
radial_noa <- fems_radial(fems_time[fems_time$soa == 'North',])
radial_soa
radial_noa

#Time of Day/Day of Week tile plot
fems_dt <- aggregate(fems$time_10plus[fems$y <= 2016],
                       by = list(hr = fems$hr[fems$y <= 2016],
                                 wd = fems$wd[fems$y <= 2016],
                                 soa = fems$anacostia[fems$y <= 2016]),
                       function(x) sum(x, na.rm = TRUE)/length(x))

fems_dt$bin <- ifelse(fems_dt$x < .1, '<10%', NA)
fems_dt$bin <- ifelse(fems_dt$x < .2 & is.na(fems_dt$bin), '10 to 20%', fems_dt$bin)
fems_dt$bin <- ifelse(fems_dt$x < .3 & is.na(fems_dt$bin), '20 to 30%', fems_dt$bin)
fems_dt$bin <- ifelse(is.na(fems_dt$bin), '>30%', fems_dt$bin)

fems_tile <-  function(df){
  temp_plot <- ggplot(data = df, aes(x = wd, y = hr, fill = bin)) +
    geom_tile(stat = 'identity') +
    discrete_scale('fill', 'GMD',
                   palette = function(n) {c('#2196f3',
                                            '#82C4F8',
                                            '#FFC166',
                                            '#ff9800',
                                            '#b6b6b6')[1:n]},
                   na.value = '#b6b6b6') 
  return(temp_plot)
}

tile_soa <- fems_tile(fems_dt[fems_dt$soa == 'South',])
tile_noa <- fems_tile(fems_dt[fems_dt$soa == 'North',])
tile_soa
tile_noa

#Export Visuals
svglite(file = '../Visuals/FEMS_Map.svg',
        width = 420/72)
map_10minPrcntBins
dev.off()

svglite(file = '../Visuals/FEMS_Bar_South.svg',
        width = 420/72)
bar_soa
dev.off()

svglite(file = '../Visuals/FEMS_Bar_North.svg',
        width = 420/72)
bar_noa
dev.off()

svglite(file = '../Visuals/FEMS_radial_North.svg',
        width = 500/72)
radial_noa
dev.off()

svglite(file = '../Visuals/FEMS_radial_South.svg',
        width = 500/72)
radial_soa
dev.off()

#Export data
write.csv(fems, '../Data/FEMS.csv', row.names = FALSE)
