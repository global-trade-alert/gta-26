rm(list = ls())

### REQUEST:
# 1. For the G20 nations, an XY plot of the share of measures implemented in 2009 that were red and harmful (X axis) against the share of measures implemented in 2020
# that are harmful (Y axis). On X axis please use title “Share of 2009 measures that created negative spillovers for trading partners.” On the Y axis please use the
# title “Share of 2020 measures that created negative spillovers for trading partners.”
#
# 2. Map of number of times each country was hit by red and amber measures implemented during 2009.
#
# 3. Map of number of times each country was hit by green measures implemented during 2009. If you can format this map and the map directly above into the same
# graphic (one vertically above the other), then great.
#
# 4. Map of share of nation’s exports that faced red and amber measures implemented during 2009.
#
# 5. Map of share of nation’s exports that faced green measures implemented during 2009. If you can format this map and the map directly above into the same
# graphic (one vertically above the other), then great.

library(gtalibrary)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(stringr)
library(openxlsx)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/G20 shares & measures/"
out.path = "tables & figures/1 - G20 shares & measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "g20 shares & measures.Rdata"))

### Save Excel files of the data
table1 <- merge(subset(table1, year.implemented == 2009), subset(table1, year.implemented == 2020), by = "implementing.jurisdiction", all = T)
table1 <- select(table1, implementing.jurisdiction, "harmful.2009" = harmful.share.x, "harmful.2020" = harmful.share.y)
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 data.xlsx"))
write.xlsx(table2, file = paste0(gta26.path, out.path, "Figure 2 & 3 data.xlsx"))
write.xlsx(table3, file = paste0(gta26.path, out.path, "Figure 4 & 5 data.xlsx"))

### Functions
# Map plotting function
map.function <- function(data, caption, plottitle, limits, breaks, labels, colourvalues, legend.name, subtitle = NULL){
  plot = ggplot() +
    geom_polygon(data= subset(data, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
    geom_polygon(data= subset(data, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=value),na.rm = T, size = 0.15, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    scale_x_continuous(limits=c(-13900000,17000000))+
    labs(x="", y="",caption=caption, subtitle = subtitle) +
    ggtitle(plottitle)+
    scale_fill_gradientn(name=str_wrap(legend.name,width = 40), 
                         na.value="#c6c6c6",
                         limits=limits,
                         colors = colourvalues,
                         breaks=breaks,
                         guide=guide_colorbar(barwidth=13, label.hjust = 0.5, title.position = "top"),
                         labels = labels)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.55,0),
          legend.justification = c(0.5,0.4),
          legend.direction = "horizontal",
          plot.title = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          plot.subtitle = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 9, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5),hjust=0.5),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.75, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
          legend.text.align = 0.6,
          legend.background = element_rect(fill="transparent"),
          plot.background = element_rect(fill="#F9F9F9"),
          plot.caption = element_text(hjust = 0.5, vjust = 0.8, margin = margin(t=30),size=8, color="#777777",lineheight = 1)
          
    )
  
  return(plot)
}

### Figure 1
# Plot
fig1 <- ggplot(data=table1)+
  geom_text_repel(aes(x=harmful.2009, y=harmful.2020, label=implementing.jurisdiction), hjust = 1.2, vjust=0.5, color = gta_colour$grey[1], size=3)+
  geom_point(aes(x=harmful.2009, y=harmful.2020), color = gta_colour$blue[1], size=2)+
  scale_y_continuous(name=str_wrap("Share of 2020 measures that created negative spillovers for trading partners", width = 40),
                     limits = c(0,1), labels = label_percent(accuracy = 1L), breaks=seq(0,1,0.1),
                     sec.axis = sec_axis(trans = ~., name = str_wrap("Share of 2020 measures that created negative spillovers for trading partners", width = 40),
                                         labels = label_percent(accuracy = 1L), breaks=seq(0,1,0.1)))+
  scale_x_continuous(name="Share of 2009 measures that created negative spillovers for trading partners", labels = label_percent(accuracy = 1L), limits=c(0,1), breaks=seq(0,1,0.1))+
  labs(caption = "Source: Global Trade Alert.") +
  gta_theme() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(size = 8),
        plot.caption.position = "plot")

fig1

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 1 - G20 shares of harmful measures",
               png = T,
               pdf = T,
               jpg = T)



### Figures 2 and 3
# Prepare plotting data
world.fig2 <- gta_plot_map_df(subset(table2, gta.evaluation == "harmful"), countries="a.un",values="intervention.id")
world.fig3 <- gta_plot_map_df(subset(table2, gta.evaluation == "liberalising"), countries="a.un",values="intervention.id")

# Plot
fig2 <- map.function(data = world.fig2, caption = "Source: Global Trade Alert.", plottitle = NULL,
                     limits = c(0, max(world.fig2$value, na.rm = T)), breaks = round(seq(0, max(world.fig2$value, na.rm = T), max(world.fig2$value, na.rm=T) / 4)),
                     labels = round(seq(0, max(world.fig2$value, na.rm = T), max(world.fig2$value, na.rm=T) / 4)),
                     colourvalues = c(gta_colour$amber[1], gta_colour$red[1], "#bf1b46", "#7d0c2a"),
                     legend.name = "Number of harmful measures affected by in 2009", subtitle = NULL)
fig2

fig3 <- map.function(data = world.fig3, caption = "Source: Global Trade Alert.", plottitle = NULL,
                     limits = c(0, max(world.fig3$value, na.rm = T)), breaks = round(seq(0, max(world.fig3$value, na.rm = T), max(world.fig3$value, na.rm=T) / 4)),
                     labels = round(seq(0, max(world.fig3$value, na.rm = T), max(world.fig3$value, na.rm=T) / 4)),
                     colourvalues = c("#b5f5bd", gta_colour$green[4], gta_colour$green[2], "#298535", "#1c6625"),
                     legend.name = "Number of liberalising measures affected by in 2009", subtitle = NULL)
fig3

fig2.and.fig3 <- grid.arrange(fig2, fig3, nrow = 2)

# Save the combined maps
gta_plot_saver(plot = fig2.and.fig3,
               path = paste0(gta26.path, out.path),
               name = "Figure 2 & 3 - Number of harmful and liberalising measures maps",
               png = T,
               pdf = T,
               jpg = T,
               height = 30,
               width = 27)



### Figures 4 and 5
# Prepare plotting data
table3 <- select(table3, "exporter" = "Exporting country", "trade.coverage" = "Trade coverage estimate for 2009", gta.evaluation)
table3 <- merge(table3, select(country.names, name, un_code), by.x = "exporter", by.y = "name", all.x = T)

world.fig4 <- gta_plot_map_df(subset(table3, gta.evaluation == "harmful"), countries="un_code", values="trade.coverage")
world.fig5 <- gta_plot_map_df(subset(table3, gta.evaluation == "liberalising"), countries="un_code", values="trade.coverage")

# Plot
fig4 <- map.function(data = world.fig4, caption = "Source: Global Trade Alert.", plottitle = NULL,
                     limits = c(0, 1), breaks = seq(0,1,0.25),
                     labels = scales::percent,
                     colourvalues = c( gta_colour$amber[1], gta_colour$red[1], "#bf1b46", "#7d0c2a"),
                     legend.name = "Share of exports affected by harmful measures implemented in 2009", subtitle = NULL)
fig4

fig5 <- map.function(data = world.fig5, caption = "Source: Global Trade Alert.", plottitle = NULL,
                     limits = c(0, 1), breaks = seq(0,1,0.25),
                     labels = scales::percent,
                     colourvalues = c(gta_colour$green[4], gta_colour$green[2], "#298535", "#1c6625"),
                     legend.name = "Share of exports affected by liberalising measures implemented in 2009", subtitle = NULL)
fig5

fig4.and.fig5 <- grid.arrange(fig4, fig5, nrow = 2)

# Save the combined maps
gta_plot_saver(plot = fig4.and.fig5,
               path = paste0(gta26.path, out.path),
               name = "Figure 4 & 5 - Affected share of trade maps",
               png = T,
               pdf = T,
               jpg = T,
               height = 30,
               width = 27)
