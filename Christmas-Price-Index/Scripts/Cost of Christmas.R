#Clear workspace
rm(list = ls())
gc()

#Load packages
require(ggplot2)
require(extrafont)
require(grid)
require(quantmod)
require(scales)
require(plyr)
require(reshape2)
require(PlotCC)

# Pull in Data
xmas <- read.csv("../Data/The Cost of Christmas.csv")
swans <- read.csv("../Data/SwanPopulation.csv")

# Load fonts
LoadFontsCC()

# Reshape and Format
xmas$YEAR <- as.Date(paste(xmas$YEAR,"-01-01", sep = ""))
xmas <- melt(xmas, id = "YEAR")
xmas$value = xmas$value - 1 
xmas$variable = revalue(xmas$variable, c("CPI" = "Consumer Price Index",
                                         "SWAN_SCALED" = "Swan Price",
                                         "CPIXMAS_SCALED" = "Christmas Price Index",
                                         "CPIXMAS_CORE_SCALED" = "Christmas Price Index (No Swans)"))

xmas$variable <- ordered(xmas$variable, levels = c("Swan Price",
                                                   "Consumer Price Index",
                                                   "Christmas Price Index",
                                                   "Christmas Price Index (No Swans)"))

# Set date breaks
break.vec<-c(as.Date("1980-1-1"),
             seq(from=as.Date("1985-1-1"), to=as.Date("2010-1-1"), by="5 year"),
             as.Date("2015-1-1"))

# Plot Indices over time
plot <- ggplot(xmas, aes(y = value, x = YEAR, color = variable)) +
  labs(title = toupper("Price Index Comparison"),
         subtitle = toupper("Percent change in price levels since 1984"),
         caption = 'Source: PNC Bank, Federal Reserve Economic Data (FRED)') +
  ylab("Percent Change Since 1984") +
  xlab("") +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(-1,3),
                     expand = c(0,0),
                     labels = percent) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = 1.1,
                                   vjust = 1.1,
                                   angle = 45)) +
  scale_x_date(breaks = break.vec,
               labels = date_format("%Y"),
               limits = c(as.Date("1984-1-1"), as.Date("2015-1-1")),
               expand = c(0,0)) +
  guides(col = guide_legend(nrow = 2)) +
  GenerateThemeCC(hlines = TRUE) +
  scale_color_manual(values = c(palette_base_cc[[2]], palette_base_cc[[3]], palette_base_cc[[4]], palette_base_cc[[5]]))
plot 

exportPlot(plot, path = "../Graphics", name = "PriceIndices_1984")





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