#Clear workspace
rm(list = ls())
gc()

#Load packages
require(ggplot2)
require(extrafont)
require(grid)
require(scales)
require(plyr)
require(reshape2)
require(lubridate)
require(PlotCC)

#Pull in Data
ufo <- read.csv("../Data/ufo_data.csv", stringsAsFactors = FALSE)
xfiles <- read.csv("../Data/ufo_year.csv", stringsAsFactors = FALSE)
weekend <- read.csv("../Data/ufo_night.csv", stringsAsFactors = FALSE)
iday <- read.csv("../Data/ufo_fourth_july_adj.csv", stringsAsFactors = FALSE)

# Load fonts
LoadFontsCC()

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

#Year breaks
break.vec <- seq(1965, 2015, by = 5)

#Plot Sightings per year
plot <- ggplot(xfiles, aes(x = Year, y = Sightings)) +
  annotate("segment", x = 1992.8, xend = 1984, y = 400, yend = 4000,
           colour = palette_base_cc$black, size=.5) +
  annotate("text", x = 1984, y = 4000,
           label = "X-Files Premiers in 1993",
           size=4, family = "Roboto Mono",
           color = palette_base_cc$black, hjust = 1.03) +
  geom_line(color = palette_base_cc$reds['mid'], size = 1) +
  #geom_point(color = "#ffffff", shape = 16, size = 3) +
  #geom_point(color = palette_base_cc$reds['mid'], shape = 16, size = 2) +
  scale_x_continuous(breaks = break.vec,
                     expand = c(.03,0)) +
  scale_y_continuous(limits = c(0,8500),
                     breaks = c(2,4,6,8) * 1000,
                     expand = c(.01,0))  +
  labs(title = toupper("Aliens watch the X-files"),
       subtitle = toupper("Total reported UFO Sightings since 1963"),
       caption = 'Source: NUFORC') +
  ylab("UFO Sightings") +
  xlab("") +
  GenerateThemeCC() 
  
exportPlot(plot, path = "../Graphics", name = "UFOSightings_PerYear", height = 400)

#Time of Day
weekendmelted <- melt(weekend, id = c("Time", "Hour"))
weekendmelted$variable <- factor(weekendmelted$variable,
                                 levels(weekendmelted$variable)[c(3,2,1,7,6,5,4)])

#Determine color intervals
cols <- 9
div <- colorRampPalette(colors = c("#ffffff", palette_base_cc$reds['mid']), bias = 1)
obs_quantiles <- quantile(weekendmelted$value, probs = seq(0, 1, 1/cols))
obs_colors <- div(length(obs_quantiles) - 1)
weekendmelted$interval <- findInterval(weekendmelted$value, obs_quantiles, all.inside = TRUE)

plot <- ggplot(weekendmelted, aes(x = Time,  y = variable, fill = factor(interval), height = 1.03, width = 1.03)) +
  geom_tile() + 
  scale_y_discrete(expand = c(.01,0)) +
  scale_x_continuous(breaks = ((1:12)*2)-1,
                     expand = c(.003,0),
                     labels = unique(weekendmelted$Hour)[(1:12)*2 -1]) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) +
  scale_fill_manual(values = obs_colors) +
  coord_fixed(ratio=1) +
  guides(fill = "none") +
  labs(title = toupper("Aliens Working for the Weekend"),
       subtitle = toupper("Density of reported sightings by time of day and day of week"),
       caption = 'Source: NUFORC') +
  ylab("") + xlab("") + GenerateThemeCC() 
  
exportPlot(plot, path = "../Graphics", name = "UFOSightings_DOWandHOD", height = 340)

#4th of July
ufo.agg = ddply(ufo[ufo$year >= 2010,], .(week), summarize, sightings = length(x)/length(unique(year)), date = min(date))
ufo.agg <- ufo.agg[-53,]

break.vec <- seq(52/24, 52-52/24, by = 52/12)

plot <- ggplot(ufo.agg, aes(x = week,  y = sightings)) +
  annotate("segment", x = 26.8, xend = 25, y = 378, yend = 410,
           colour = palette_base_cc$black, size=.5) +
  annotate("text", x = 25, y = 410,
           label = "4th of July",
           size=4, family = "Roboto Mono",
           color = palette_base_cc$black, hjust = 1.03) +
  geom_line(color = palette_base_cc$reds['mid'], size = 1) +
  ylab("") + xlab("") + GenerateThemeCC() +
  labs(title = toupper("Aliens Love America"),
       subtitle = toupper("Average reported UFO sightings per week since 2010"),
       caption = 'Source: NUFORC') +
  scale_x_continuous(breaks = break.vec,
                     expand = c(.03,0),
                     labels = c("Jan", "Feb", "Mar",
                                "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits = c(0,450),
                     breaks = c(1:4) * 100,
                     expand = c(.01,0)) 

exportPlot(plot, path = "../Graphics", name = "UFOSightings_PerWeek", height = 400)
