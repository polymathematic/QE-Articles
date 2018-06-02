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
coal_cons <- read.csv("../Data/Coalcons.csv", stringsAsFactors = FALSE)
coal_eff <- read.csv("../Data/Coaleff.csv", stringsAsFactors = FALSE)

# Create work-equivalence variable
coal_eff$eng_wrk <- (max(coal_eff$eng_eff, na.rm = TRUE) / coal_eff$eng_eff) 

# Adjust years 
coal_cons$year_adj <- coal_cons$year - 1

#Create engine labels
coal_eff$eng_label <- sprintf("%s\n%s",coal_eff$year,coal_eff$eng_name)

# Stepped line
coal_dual <- ggplot() +
  geom_bar(width = 8,
           stat = "identity", 
           aes(x = year_adj, y = cpercap),
           data = coal_cons,
           fill = palette_base_cc$reds['mid']) +
  geom_step(size = 1,
            aes(x = year,
                y = KwH_100),
            data = coal_eff,
            color = palette_base_cc$blues['mid']) +
  geom_text(aes(x = year, y = KwH_100, label = trimws(eng_label)),
            data = coal_eff[-nrow(coal_eff),],
            family = "Roboto Mono",
            size = 3.5,
            hjust = 0,
            nudge_x = .5,
            vjust = -.3) + 
  geom_text(aes(x = year, y = KwH_100, label = trimws(paste0(format(round(KwH_100, 1), nsmall = 1), " Tons"))),
            data = coal_eff[-nrow(coal_eff),],
            family = "Roboto Mono",
            size = 3.5,
            hjust = 0,
            nudge_x = .5,
            vjust = 1.5) + 
  geom_text(aes(x = year, y = cpercap, label = trimws(paste0(format(round(cpercap, 1), nsmall = 1), " Tons"))),
            data = coal_cons,
            family = "Roboto Mono",
            color = '#ffffff',
            size = 3.5,
            hjust =  .65,
            vjust = 1.7) +
  scale_x_continuous(name = NULL,
                     limits = c(1774,1866.5),    
                     expand = c(0,0),
                     breaks = 1780 + 10*(0:9)) + 
  scale_y_continuous(name = 'Tons of Coal',
                     limits = c(0,13.5),
                     breaks = (1:13),
                     expand = c(0,0)) +
  labs(title = toupper("Jevons' Paradox"),
       subtitle = toupper("Advances in steam engine efficiency and per-capita coal use"),
       caption = 'Source: "The Coal Question" by William Jevons') + 
  GenerateThemeCC()

coal_dual

exportPlot(coal_dual, path = "../Graphics", name = "JevonsParadox")

#piles of Coal blank
pilesofCoal <- ggplot() + labs(title = toupper("Jevons Missed High"),
     subtitle = toupper("Actual 1961 UK coal use versus Jevon's forecast"),
     caption = 'Source: "The Coal Question" by William Jevons, Gov.uk') + 
  GenerateThemeCC()

exportPlot(pilesofCoal, path = "../Graphics", name = "PilesOfCoal_Blank")
