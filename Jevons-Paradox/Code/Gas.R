#Clear workspace
rm(list = ls())
gc()

#Load packages
require(dhplot)
require(ggplot2)
require(ggthemes)
require(extrafont)
require(svglite)

#Load font
font_import(pattern = "Exo")
y
loadfonts(device = "win")

# Pull in Data
fuel <- read.csv("../Data/gas.csv", stringsAsFactors = FALSE)

fuel.sub <- fuel[fuel$year > 1980,]

# Scatterplot over time
gas_scatter <- ggplot(aes(x = avgmpg, y = gaspercap), data = fuel.sub) +
  geom_point(size = 4, color = '#2196f3') +
  geom_text(aes(label = year),                         
             family = "Exo 2",
             size = 3,
             hjust = 1.25,
             vjust = .5,
             angle = 45) +
  geom_smooth(method = "lm",
              size = 1,
              se = TRUE,
              color = '#ff9800') +
  scale_x_continuous(name = 'Average Annual Fuel Efficiency (MPG)',
                     limits = c(19,25),
                     expand = c(0,0)) + 
  scale_y_continuous(name = 'Fuel Consumption per Capita (Gallons/Person)',
                     limits = c(415,485),
                     breaks = (((1:5)-1)*20+400),
                     expand = c(0,0)) +
  dh_scatter() +
  ggtitle("An Exception to Jevons' Paradox",
            "Increases in average US fuel efficiency are associated with lower per-capita fuel consumption") +
  labs(caption = 'SOURCE: EPA, EIA, US Census')
gas_scatter

svglite(file = '../Visuals/gas.svg',
        width = 800/72)
gas_scatter
dev.off()


