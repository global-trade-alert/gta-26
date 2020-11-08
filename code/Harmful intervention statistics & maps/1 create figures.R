rm(list = ls())

### REQUEST:
# 1. Please produce a line chart for the years 2009 to 2020 with the following variable on the primary vertical axis: Total number of commercial policy interventions
# worldwide documented by 21 October. On the second axis: please plot the average number of hours between policy intervention announcements (calculated as the ratio
# of 7080 divided by the total number of commercial policy interventions documented by 21 October). For the primary axis title use the text highlighted in bold. For
# the secondary axis title please use “Average number of hours between each commercial policy intervention.” Please add a note at the bottom stating
# “Source: Global Trade Alert.” ### SE 06.11.: Use announcement date
#
# 2. Map of number of times each country was hit by red and amber measures implemented during 2020.
#
# 3. Map of number of times each country was hit by green measures implemented during 2020. If you can format this map and the map directly above into the same
# graphic (one vertically above the other), then great.
#
# 4. Map of share of nation’s exports that faced red and amber measures implemented during 2020.
#
# 5. Map of share of nation’s exports that faced green measures implemented during 2020. If you can format this map and the map directly above into the same
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
data.path = "data/Harmful intervention statistics & maps/"
out.path = "tables & figures/4 - Harmful intervention statistics & maps/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "harmful intervention statistics & maps.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 data.xlsx"))
write.xlsx(table2, file = paste0(gta26.path, out.path, "Figure 2 & 3 data.xlsx"))
write.xlsx(table3, file = paste0(gta26.path, out.path, "Figure 4 & 5 data.xlsx"))


### Functions
map.function <- function(data, caption, plottitle, limits, breaks, labels, colourvalues, legend.name, subtitle = NULL){
  plot = ggplot() +
    geom_polygon(data= subset(data, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
    geom_polygon(data= subset(data, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=value),na.rm = T, size = 0.15, color = "white") +
    geom_polygon(data=subset(data, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
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
fig1 <- ggplot(data=table1)+
  geom_line(aes(x=forcats::fct_inorder(as.character(year.announced), ordered = T), y=nr.of.interventions, group=1), size=1, colour=gta_colour$qualitative[1])+
  # geom_text(aes(x=month.group, y=index, label=index), vjust=1, colour="#555555", size=2.5)+
  geom_line(aes(x=forcats::fct_inorder(as.character(year.announced), ordered = T), y=avg.hours.between.int*100, group=1), size=1, colour=gta_colour$qualitative[3]) +
  scale_y_continuous(name=str_wrap("Total number of commercial policy interventions worldwide announced by 31 October", 50),
                     labels=seq(0,2100,300), limits = c(0,2100), breaks=seq(0,2100,300),
                     sec.axis = sec_axis(trans = ~./100, name=str_wrap("Average number of hours between each commercial policy intervention", 50),
                                         breaks=seq(0,21,3),labels=seq(0,21,3)))+
  labs(x = "", caption = "Source: Global Trade Alert.")+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(size=.1, color="#555555"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption.position = "plot",
        axis.text.y.left = element_text(colour = gta_colour$qualitative[1]),
        axis.text.y.right = element_text(colour = gta_colour$qualitative[3]),
        axis.title.y.left = element_text(colour = gta_colour$qualitative[1]),
        axis.title.y.right = element_text(colour = gta_colour$qualitative[3]))

fig1

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 1 - Worldwide interventions published & intervention frequency",
               png = T,
               pdf = T,
               jpg = T,
               eps = T)


### Figures 2 & 3
# Prepare plotting data
world.fig2 <- gta_plot_map_df(subset(table2, gta.evaluation == "harmful"), countries="affected.un",values="nr.of.interventions")
world.fig3 <- gta_plot_map_df(subset(table2, gta.evaluation == "liberalising"), countries="affected.un",values="nr.of.interventions")

# Plot
fig2 <- map.function(data = world.fig2, caption = "Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020", plottitle = NULL,
                     limits = c(0, max(world.fig2$value, na.rm = T)), breaks = c(seq(0, max(world.fig2$value, na.rm = T), max(world.fig2$value, na.rm=T) / 4)),
                     labels = c(round(seq(0, max(world.fig2$value, na.rm = T), max(world.fig2$value, na.rm=T) / 4))),
                     colourvalues = c(gta_colour$amber[4], gta_colour$amber[1], gta_colour$red[1], "#bf1b46", "#7d0c2a"),
                     legend.name = "Number of harmful measures affected by in 2020", subtitle = NULL)
fig2

fig3 <- map.function(data = world.fig3, caption = "Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020", plottitle = NULL,
                     limits = c(0, max(world.fig3$value, na.rm = T)), breaks = c(seq(0, max(world.fig3$value, na.rm = T), max(world.fig3$value, na.rm=T) / 4)),
                     labels = c(round(seq(0, max(world.fig3$value, na.rm = T), max(world.fig3$value, na.rm=T) / 4))),
                     colourvalues = c(gta_colour$green[4], gta_colour$green[2], "#298535", "#1c6625"),
                     legend.name = "Number of liberalising measures affected by in 2020", subtitle = NULL)
fig3

fig2.and.fig3 <- grid.arrange(fig2, fig3, nrow = 2)

# Save the combined maps
gta_plot_saver(plot = fig2.and.fig3,
               path = paste0(gta26.path, out.path),
               name = "Figure 2 & 3 - Number of measures countries are affected by in 2020",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               height = 30,
               width = 27)


### Figures 4 & 5
# Prepare plotting data
world.fig4 <- gta_plot_map_df(subset(table3, evaluation == "harmful"), countries="un_code",values="trade.coverage")
world.fig5 <- gta_plot_map_df(subset(table3, evaluation == "liberalising"), countries="un_code",values="trade.coverage")

# Plot
fig4 <- map.function(data = world.fig4, caption = "Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020", plottitle = NULL,
                     limits = c(0, max(world.fig4$value, na.rm = T)), breaks = c(seq(0, max(world.fig4$value, na.rm = T), max(world.fig4$value, na.rm=T) / 4)),
                     labels = scales::percent,
                     colourvalues = c(gta_colour$amber[4], gta_colour$amber[1], gta_colour$red[1], "#bf1b46", "#7d0c2a"),
                     legend.name = "Share of exports affected by harmful measures implemented in 2020", subtitle = NULL)
fig4

fig5 <- map.function(data = world.fig5, caption = "Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020", plottitle = NULL,
                     limits = c(0, max(world.fig5$value, na.rm = T)), breaks = c(seq(0, max(world.fig5$value, na.rm = T), max(world.fig5$value, na.rm=T) / 4)),
                     labels = scales::percent,
                     colourvalues = c(gta_colour$green[4], gta_colour$green[2], "#298535", "#1c6625"),
                     legend.name = "Share of exports affected by liberalising measures implemented in 2020", subtitle = NULL)
fig5

fig4.and.fig5 <- grid.arrange(fig4, fig5, nrow = 2)

# Save the combined maps
gta_plot_saver(plot = fig4.and.fig5,
               path = paste0(gta26.path, out.path),
               name = "Figure 4 & 5 - Affected export share 2020 map",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               height = 30,
               width = 27)
