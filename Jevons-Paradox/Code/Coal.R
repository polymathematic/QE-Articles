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
coal_cons <- read.csv("../Data/Coalcons.csv", stringsAsFactors = FALSE)
coal_eff <- read.csv("../Data/Coaleff.csv", stringsAsFactors = FALSE)

# Create work-equivalence variable
coal_eff$eng_wrk <- (max(coal_eff$eng_eff, na.rm = TRUE) / coal_eff$eng_eff) 

# Adjust years 
coal_cons$year_adj <- coal_cons$year - 1

# Scatterplot over time
coal_dual <- ggplot() +
  geom_bar(width = 8,
           stat = "identity", 
           aes(x = year_adj, y = cpercap),
           data = coal_cons,
           fill = '#2196f3') +
  geom_step(size = 1,
            aes(x = year,
                y = KwH_100),
            data = coal_eff,
            color = '#ff9800') +
  geom_text(aes(x = year, y = KwH_100, label = eng_name),
            data = coal_eff,
            family = "Exo 2",
            size = 3.5,
            hjust = -.05,
            vjust = -.7) + 
  geom_text(aes(x = year, y = KwH_100, label = paste0(format(round(KwH_100, 1), nsmall = 1), " Tons")),
            data = coal_eff,
            family = "Exo 2",
            size = 3.5,
            hjust = -.3,
            vjust = 1) + 
  geom_text(aes(x = year, y = cpercap, label = paste0(format(round(cpercap, 1), nsmall = 1), " Tons")),
            data = coal_cons,
            family = "Exo 2",
            color = '#f1f1f1',
            size = 3.5,
            vjust = 1.2) +
  scale_x_continuous(limits = c(1774,1866),    
                     expand = c(0,0),
                     breaks = 1780 + 10*(0:9)) + 
  scale_y_continuous(name = 'Tons of Coal',
                     limits = c(0,13),
                     breaks = (1:13),
                     expand = c(0,0)) +
  dh_bar(xlab = FALSE) +
  ggtitle("Jevons' Paradox",
          "19th century coal consumption increased with advances in steam engine efficiency") +
  labs(caption = 'SOURCE: "The Coal Question" by William Jevons')

coal_dual

svglite(file = '../Visuals/coal.svg',
        width = 800/72)
coal_dual
dev.off()


