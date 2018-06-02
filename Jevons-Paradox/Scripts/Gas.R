#Clear workspace
rm(list = ls())
gc()

#Load packages
require(ggplot2)
require(extrafont)
require(PlotCC)
require(ggrepel)

#Load font
LoadFontsCC()

# Pull in Data
fuel <- read.csv("../Data/Gas.csv", stringsAsFactors = FALSE)

# Subset to recent years
fuel.sub <- fuel[fuel$year > 1980,]

#Move 1984 a bit
fuel.sub$labelpos.y <- ifelse(fuel.sub$year == 1984, fuel.sub$gaspercap + 3, fuel.sub$gaspercap)

# Scatterplot over time
gas_scatter <- ggplot(aes(x = avgmpg, y = gaspercap), data = fuel.sub) +
  geom_point(size = 4, color = palette_base_cc$reds['mid']) +
  geom_text(aes(label = year),
             family = "Roboto Mono",
             size = 4,
             hjust = 1.25,
             vjust = .5,
             angle = 45,
            check_overlap = TRUE) +
  scale_x_continuous(name = 'Average Annual Fuel Efficiency (MPG)',
                     limits = c(18.5,24.5),
                     breaks = 19:24,
                     expand = c(0,0)) + 
  scale_y_continuous(name = 'Fuel Consumption per Capita (Gallons/Person)',
                     limits = c(415,485),
                     breaks = (((1:5)-1)*20+400),
                     expand = c(0,0)) +
  GenerateThemeCC(hlines = TRUE, vlines = TRUE) +
  geom_smooth(method = "lm",
              size = 1,
              se = FALSE,
              color = palette_base_cc$midgrey,
              linetype="dotted") +
  labs(title = toupper("An Exception to Jevons' Paradox"),
       subtitle = toupper("Higher average fuel efficiency is correlated with lower consumption"),
       caption = 'SOURCE: EPA, EIA, US Census')

exportPlot(gas_scatter, path = "../Graphics", name = "GasPerCap_FuelEfficiency", height = 600)
