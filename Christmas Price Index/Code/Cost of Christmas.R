#Clear workspace
rm(list = ls())
gc()

#Load packages
require(ggplot2)
require(ggthemes)
require(extrafont)
require(grid)
require(quantmod)
require(scales)
require(plyr)
require(reshape)

#Pull in Data
xmas <- read.csv("../Data/The Cost of Christmas.csv")
swans <- read.csv("../Data/SwanPopulation.csv")

#Reshape and Format
xmas$YEAR <- as.Date(paste(xmas$YEAR,"-01-01", sep = ""))
xmas <- melt(xmas, id = "YEAR")
xmas$value = xmas$value - 1 
xmas$variable = revalue(xmas$variable, c("CPI" = "Consumer Price Index",
                                         "SWAN_SCALED" = "Swan Price",
                                         "CPIXMAS_SCALED" = "Christmas Price Index",
                                         "CPIXMAS_CORE_SCALED" = "Christmas Price Index (No Swans)"))

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
font_import(pattern = "Open")
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

theme_QE <- theme(text = element_text(family = "Open Sans", color = "#333333"),
                  
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

# Plot Indices over time
xmas$variable <- ordered(xmas$variable, levels = c("Swan Price",
                                                   "Consumer Price Index",
                                                   "Christmas Price Index",
                                                   "Christmas Price Index (No Swans)"))

svg(filename = "../Visuals/PriceIndices.svg",
    width = (width*3)/(72*4),
    height = (height*3)/(72*4),
    family = "Open Sans",
    antialias = "none")

break.vec<-c(as.Date("1980-1-1"),
             seq(from=as.Date("1985-1-1"), to=as.Date("2010-1-1"), by="5 year"),
             as.Date("2015-1-1"))

plot <- ggplot(xmas, aes(y = value, x = YEAR, color = variable))
plot <- plot + ggtitle("Comparison of Price Indices")
plot <- plot + ylab("Percent Change Since 1984")
plot <- plot + geom_line(size = 1)
plot <- plot + theme_QE
plot <- plot + scale_color_manual(values = c(QE_colors[5], QE_colors[3], QE_colors[2], QE_colors[1]))
plot <- plot + scale_y_continuous(limits = c(-1,3),
                                  expand = c(0,0),
                                  labels = percent)
plot <- plot + theme(axis.title.x = element_blank(),
                     axis.text.x = element_text(hjust = 1.1, vjust = 1.1, angle = 45))
plot <- plot + scale_x_date(breaks = break.vec,
                            labels = date_format("%Y"),
                            limits = c(as.Date("1984-1-1"), as.Date("2015-1-1")),
                            expand = c(0,0))
plot <- plot + guides(col = guide_legend(nrow = 2))
plot

dev.off()

#Plot swan quantity/price
# swans.lm <- lm(P ~ SWANPOP, data = swans)
# swans.ci <- predict(swans.lm, interval = 'conf')
# swans <- cbind(swans, swans.ci)

svg(filename = "../Visuals/swanmarket.svg",
    width = (width*3)/(72*4),
    height = (height*3)/(72*4),
    family = "Open Sans",
    antialias = "none")

plot <- ggplot(swans, aes(y = P, x = SWANPOP))
plot <- plot + geom_smooth(method = "lm",
                           size = 1,
                           se = TRUE,
                           color = QE_colors[5])
plot <- plot + geom_point(size = 3, color = QE_colors[2])
plot <- plot + geom_text(aes(label = YEAR),
                         family = "Open Sans",
                         color = QE_colors[2],
                         fontface = "bold",
                         size = 3,
                         hjust = 1.25,
                         vjust = .5,
                         angle = 45)
plot <- plot + ggtitle("Mute Swan Price/Population Relationship")
plot <- plot + ylab("Price")
plot <- plot + xlab("Swans Observed in Survey")
plot <- plot + theme_QE
plot <- plot + theme(plot.margin = unit(c(20, 20, 20, 20),"points"))
#plot <- plot + theme(panel.grid.major = element_line(size = .3, color = "#dedede"))
plot <- plot + scale_x_continuous(limits = c(95,405),
                                  expand = c(0,0))
plot <- plot + scale_y_continuous(limits = c(1500,7500),
                                   expand = c(0,0),
                                  labels = dollar)
plot

dev.off()