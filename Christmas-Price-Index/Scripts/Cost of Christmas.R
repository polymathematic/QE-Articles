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
require(PlotCC)
require(svglite)

# Pull in Data
xmas <- read.csv("Data/The Cost of Christmas.csv")
swans <- read.csv("Data/SwanPopulation.csv")

# Load fonts
load_fonts()

# Reshape and Format
xmas$YEAR <- as.Date(paste(xmas$YEAR,"-01-01", sep = ""))
xmas <- melt(xmas, id = "YEAR")
xmas$value = xmas$value - 1 
xmas$variable = revalue(xmas$variable,
                        c("CPI" = " Consumer Price Index ",
                          "SWAN_SCALED" = " Swan Price Level ",
                          "CPIXMAS_SCALED" = " Christmas Price Index (With Swans) ",
                          "CPIXMAS_CORE_SCALED" = " Christmas Price Index (Without Swans) "))

xmas$variable <- ordered(xmas$variable,
                         levels = c(" Swan Price Level ",
                                    " Christmas Price Index (With Swans) ",
                                    " Consumer Price Index ",
                                    " Christmas Price Index (Without Swans) "))

# Set date breaks
break_vec<-c(as.Date("1980-1-1"),
             seq(from=as.Date("1985-1-1"), to=as.Date("2010-1-1"), by="5 year"),
             as.Date("2015-1-1"))

# Plot Indices over time
p1 <- ggplot(xmas, aes(y = value, x = YEAR, color = variable)) +
  labs(title = toupper("Price Index Comparison"),
       subtitle = toupper("Percent change in price levels since 1984"),
       caption = 'Source: PNC Bank, Federal Reserve Economic Data (FRED)') +
  ylab("Percent Change Since 1984") +
  xlab("") +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(-1,3), expand = c(0,0), labels = percent) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(hjust = 1.1, vjust = 1.1, angle = 45)) +
  scale_x_date(breaks = break_vec, labels = date_format("%Y"),
               limits = c(as.Date("1984-1-1"), as.Date("2015-1-1")),
               expand = c(0, 0)) +
  guides(col = guide_legend(nrow = 2)) +
  generate_theme(hlines = TRUE) +
  scale_color_manual(values = create_palette())

export_plot(p1, path = "Graphics", name = "PriceIndices",
            export_type = c("PNG","SVG"), width = 1024, height = 512)

#Model swan quantity/price
swans_lm <- lm(P ~ SWANPOP, data = swans)
summary(swans_lm)

#Plot swan quantity/price
p2 <- ggplot(swans, aes(y = P, x = SWANPOP)) +
  geom_smooth(method = "lm",
              size = 1,
              se = FALSE,
              color = "#383F51",
              linetype="dotted") +
  geom_point(size = 4, color = create_palette()[1]) +
  geom_text(aes(label = YEAR),
            family = "Roboto Mono",
            color = "#383F51",
            size = 4,
            hjust = -0.25,
            vjust = .25,
            angle = 45) +
  labs(title = toupper("Mute Swan Price / Population Relationship"),
       subtitle = toupper("Higher swan populations are associated with lower prices"),
       caption = 'Source: PNC Bank, Mute Swan Mid-Summer Survey') +
  ylab("Price") + xlab("Swans Observed in Survey") + 
  generate_theme(hlines = TRUE, vlines = TRUE) +
  scale_x_continuous(limits = c(75,375),
                     expand = c(0,0),
                     breaks = seq(100,350,50)) +
  scale_y_continuous(limits = c(1500,7500),
                     expand = c(0,0),
                     breaks = (2:7)*1000,
                     labels = dollar)

export_plot(p2, path = "Graphics", name = "SwanMarket",
            export_type = c("PNG","SVG"), width = 1024, height = 768)