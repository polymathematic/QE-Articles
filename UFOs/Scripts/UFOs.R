#Clear workspace
rm(list = ls())
gc()

#Load packages
require(ggplot2)
require(ggthemes)
require(extrafont)
require(plyr)
require(foreign)
require(grid)
require(scales)
require(lubridate)
require(reshape)
require(zoo)

# Define Colors
QE_colors <- c("#03436F", # Dark Blue
               "#006AB2", # Mid Blue
               "#0097FF", # Light Blue
               "#B26A00", # Dark Orange
               "#FF9700") # Light Orange

QE_blue_gradient <-c("#0097FF", # lightest blue
                     "#03436F") # darkest blue

QE_orange_gradient <-c("#FF9700", # lightest orange
                       "#4e2e00") # darkest orange

#Load OpenSans
font_import(pattern = "PT")
y
loadfonts(device = "win")

#Set Parameters
axis.title.size <- 14 # Default 14
axis.text.size <- 11 # Default 11
legend.title.size <- 14 # Default 14
legend.text.size <- 11 # Default 11
plot.title.size <- 20 # Default 20
width = 720 #720 is width of post column
height = 500

theme_QE <- theme(text = element_text(family = "PT Sans", color = "#333333"),
                  
                  axis.line = element_line(color = "#333333"),
                  axis.title = element_text(size = axis.title.size),
                  axis.title.x = element_text(color = "#333333", vjust = -.1),
                  axis.title.y = element_text(vjust = .8),
                  axis.text = element_text(size = axis.text.size, color = "#333333"),
                  axis.ticks = element_line(color = "#333333", size = .3),
                  axis.ticks.length = unit(4,"points"),
                  axis.ticks.margin = unit(4,"points"),
                  
                  legend.title = element_blank(),
                  legend.background = element_rect(fill = NA),
                  legend.text = element_text(size = legend.text.size),
                  legend.justification = c(0,0),
                  legend.margin = unit(0,"points"),
                  legend.position = "bottom",
                  legend.direction = "horizontal",
                  legend.box.just = "right",
                  legend.key = element_rect(fill = NA),
                  legend.key.size = unit(12,"pt"),
                  
                  panel.border = element_blank(),
                  panel.background = element_rect(fill = NA), #change to NA
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.margin = unit(20,"points"),
                  
                  plot.margin = unit(c(20,20,20,20), "points"), #Change the 3rd number to 5 if there's no label for x
                  plot.title = element_text(size = plot.title.size, face = "bold", hjust = 0, vjust = 1.2))

#Pull in Data
ufo <- read.csv("../Data/ufo_data.csv", stringsAsFactors = FALSE)
xfiles <- read.csv("../Data/ufo_year.csv", stringsAsFactors = FALSE)
weekend <- read.csv("../Data/ufo_night.csv", stringsAsFactors = FALSE)
iday <- read.csv("../Data/ufo_fourth_july_adj.csv", stringsAsFactors = FALSE)

#Format Variables
ufo$state <- as.factor(ufo$state)
ufo$shape <- as.factor(ufo$shape)
ufo$posted <- as.Date(ufo$posted, format = "%m/%d/%Y")
ufo$date_time <- as.POSIXct(ufo$date_time, format = "%m/%d/%Y %H:%M")
ufo$date <- as.Date(strptime(ufo$date_time, format = "%Y-%m-%d"))
ufo$dayofweek <- weekdays(strptime(ufo$date_time, "%Y-%m-%d %H:%M:%S"))
ufo$dayofweek <- as.factor(ufo$dayofweek)
ufo$hourofday <- hour(ufo$date_time) + minute(ufo$date_time)/60
ufo$year <- year(ufo$date_time)
ufo$mo <- month(ufo$date_time)
ufo$week <- week(ufo$date)

ufo$x <- ifelse(ufo$hourofday <= 5, ufo$hourofday + 19, ufo$hourofday-5)

#Clean data
ufo <- ufo[ufo$year <= 2014 & ufo$year >= 1974 & !is.na(ufo$year),]


#Density per year
break.vec <- seq(1965, 2015, by = 5)

width = 720 #720 is width of post column
height = 350

svg(filename = "../Visuals/xfiles2.svg",
    width = (width*3)/(72*4),
    height = (height*3)/(72*4),
    family = "PT Sans",
    antialias = "none")

plot <- ggplot(xfiles, aes(x = Year, y = Sightings)) +
  geom_line(color = QE_colors[2], size = 1) +
  geom_point(color = "#ffffff", shape = 16, size = 3) +
  geom_point(color = QE_colors[2], shape = 16, size = 2) +
  theme_QE + theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank()) +
  scale_x_continuous(breaks = break.vec,
                     expand = c(.03,0)) +
  scale_y_continuous(limits = c(0,8500),
                     breaks = c(2,4,6,8) * 1000,
                     expand = c(.01,0)) 
plot
dev.off()


#Time of Day

weekendmelted <- melt(weekend, id = c("Time", "Hour"))
weekendmelted

weekendmelted$variable <- factor(weekendmelted$variable,levels(weekendmelted$variable)[c(3,2,1,7,6,5,4)])


width = 720 #720 is width of post column
height = 300

div <- colorRampPalette(colors = c(QE_blue_gradient[2], "black", QE_orange_gradient[1]),
                        bias = 1)

v <- mean(weekendmelted$value)

svg(filename = "../Visuals/timeofday2.svg",
    width = (width*3)/(72*4),
    height = (height*3)/(72*4),
    family = "PT Sans",
    antialias = "none")

plot <- ggplot(weekendmelted, aes(x = Time,  y = variable, fill = value, height = 1.03, width = 1.03)) +
  geom_tile() + theme_QE +
  scale_y_discrete(expand = c(.01,0)) +
  scale_x_continuous(breaks = ((1:12)*2)-1,
                     expand = c(.003,0),
                     labels = unique(weekendmelted$Hour)[(1:12)*2 -1]) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) +
  scale_fill_gradientn(colours = div(3), values = rescale(c(0,v/4, v, v*2), to = c(0,1))) +
  coord_fixed(ratio=1) +
  guides(guide_legend(show = FALSE))
  
plot

dev.off()

#4th of July

ufo.agg = ddply(ufo[ufo$year >= 2010,], .(week), summarize, sightings = length(x)/length(unique(year)), date = min(date))
ufo.agg <- ufo.agg[-53,]

break.vec <- seq(52/24, 52-52/24, by = 52/12)

width = 720 #720 is width of post column
height = 350

svg(filename = "iday2.svg",   
    width = (width*3)/(72*4),
    height = (height*3)/(72*4),
    family = "PT Sans",
    antialias = "none")

plot <- ggplot(ufo.agg, aes(x = week,  y = sightings)) +
  geom_line(color = QE_colors[2], size = 1) +
  geom_point(color = "#ffffff", shape = 16, size = 3) +
  geom_point(color = QE_colors[2], shape = 16, size = 2) +
  theme_QE +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = break.vec,
                     expand = c(.03,0),
                     labels = c("Jan", "Feb", "Mar",
                                "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits = c(0,450),
                     breaks = c(1:4) * 100,
                     expand = c(.01,0)) # + theme(axis.text.x = element_text(angle = 45, vjust = 1.15, hjust = 1.15))

plot

dev.off()