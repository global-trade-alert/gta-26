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
#
# Added 07.11.: I have been thinking the results that you've tabulated and produced charts for. What is striking this year is that the global averages are potentially
# misleading as there is a huge diversity in outcomes across nations. I am going to figure out how to explain that in the report and the following charts
# would help in this regard.
#
# 6. For the data in the figure 2 & 3 excel file, please produce two kernel distributions across nations of the number of times their commercial interests have been
# (a) hit or (b) benefited from foreign reforms. If possible, please put both kernels on the same chart.
# 
# 7. Please repeat the same request immediately above for the harmful and liberalising export exposure statistics in the attached file called figure 4 & 5. 
# 
# 8. Next, using the data in figure 4 & 5 please plot the % of exports facing harmful measures (on the Y axis) against the % of exports facing beneficial
# measures (on the X axis). Please make the size of the dot associated with each nation proportional to the logarithm of total goods exports of that nation in
# the last year for which we have trade data before the pandemic (either 2018 or 2019). 
# 
# 9. Please produce another version of the same plot (mentioned directly above) where the size of the dot associated with each nation is the logarithm of the per
# capita GDP.
# 
# 10. Please produce another version of the same plot (mentioned two paragraphs above) where the dot is red for a G20 member.
# 
# 11. Please produce another version of the same plot (mentioned three paragraphs above) where the dot is red for a Least Developed country. 

library(gtalibrary)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(stringr)
library(openxlsx)
library(ggforce)

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
write.xlsx(table8, file = paste0(gta26.path, out.path, "Figure 8 data.xlsx"))
write.xlsx(table9, file = paste0(gta26.path, out.path, "Figure 9 data.xlsx"))
write.xlsx(table10, file = paste0(gta26.path, out.path, "Figure 10 data.xlsx"))
write.xlsx(table11, file = paste0(gta26.path, out.path, "Figure 11 data.xlsx"))


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
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.9, margin = margin(r=10,b=5),hjust=0.5),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
          legend.text.align = 0.6,
          legend.background = element_rect(fill="transparent"),
          plot.background = element_rect(fill="#F9F9F9"),
          plot.caption = element_text(hjust = 0.5, vjust = 0.8, margin = margin(t=40),size=8, color="#777777",lineheight = 1),
          plot.margin = margin(t=20, b=30, l=10, r=10)
          
    )
  
  return(plot)
}


### Figure 1

# Simon: In the primary vertical axis please replace the word announced with implemented

fig1 <- ggplot(data=table1)+
  geom_line(aes(x=forcats::fct_inorder(as.character(year.announced), ordered = T), y=nr.of.interventions, group=1), size=1, colour=gta_colour$qualitative[1])+
  # geom_text(aes(x=month.group, y=index, label=index), vjust=1, colour="#555555", size=2.5)+
  geom_line(aes(x=forcats::fct_inorder(as.character(year.announced), ordered = T), y=avg.hours.between.int*100, group=1), size=1, colour=gta_colour$qualitative[3]) +
  scale_y_continuous(name=str_wrap("Total number of commercial policy interventions worldwide implemented by 31 October", 50),
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
fig2 <- map.function(data = world.fig2, caption = str_wrap("Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020",60), plottitle = NULL,
                     limits = c(0, max(world.fig2$value, na.rm = T)), breaks = c(seq(0, max(world.fig2$value, na.rm = T), max(world.fig2$value, na.rm=T) / 4)),
                     labels = c(round(seq(0, max(world.fig2$value, na.rm = T), max(world.fig2$value, na.rm=T) / 4))),
                     colourvalues = c(gta_colour$amber[4], gta_colour$amber[1], gta_colour$red[1], "#bf1b46", "#7d0c2a"),
                     legend.name = "Number of harmful measures\naffected by in 2020", subtitle = NULL)
fig2

fig3 <- map.function(data = world.fig3, caption = str_wrap("Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020",60), plottitle = NULL,
                     limits = c(0, max(world.fig3$value, na.rm = T)), breaks = c(seq(0, max(world.fig3$value, na.rm = T), max(world.fig3$value, na.rm=T) / 4)),
                     labels = c(round(seq(0, max(world.fig3$value, na.rm = T), max(world.fig3$value, na.rm=T) / 4))),
                     colourvalues = c(gta_colour$green[4], gta_colour$green[2], "#298535", "#1c6625"),
                     legend.name = "Number of liberalising measures\naffected by in 2020", subtitle = NULL)
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
fig4 <- map.function(data = world.fig4, caption = str_wrap("Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020", 60), plottitle = NULL,
                     limits = c(0, max(world.fig4$value, na.rm = T)), breaks = c(seq(0, max(world.fig4$value, na.rm = T), max(world.fig4$value, na.rm=T) / 4)),
                     labels = scales::percent,
                     colourvalues = c(gta_colour$amber[4], gta_colour$amber[1], gta_colour$red[1], "#bf1b46", "#7d0c2a"),
                     legend.name = "Share of exports affected by harmful measures implemented in 2020", subtitle = NULL)
fig4

fig5 <- map.function(data = world.fig5, caption = str_wrap("Source: Global Trade Alert. Countries marked in grey had zero export exposure to reforms implemented during 2020", 60), plottitle = NULL,
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



### Figure 6
# Make sure all combinations are represented
temp <- data.frame("affected.name" = unique(table3$exporter),
                   "gta.evaluation" = "harmful, liberalising")
temp <- cSplit(temp, "gta.evaluation", ", ", "long")
temp <- merge(temp, select(country.names, "affected.name" = name, "affected.un" = un_code), by = "affected.name", all.x = T)

table2 <- merge(temp, select(table2, affected.name, gta.evaluation, nr.of.interventions), by = c("affected.name", "gta.evaluation"), all.x = T); rm(temp)
table2[is.na(table2)] <- 0

# Plot
fig6 <- ggplot(table2, aes(x=nr.of.interventions, color = gta.evaluation)) +
  geom_density() +
  labs(x = "Nr. of interventions affected by/benefiting from", y = "Density", caption = "Source: Global Trade Alert.") +
  scale_color_manual(values=c(gta_colour$harmful[1], gta_colour$liberalising[1])) +
  guides(colour=guide_legend(title="GTA evaluation", 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right"))+
  gta_theme() +
  theme(legend.position = "bottom")

fig6

gta_plot_saver(plot = fig6,
               path = paste0(gta26.path, out.path),
               name = "Figure 6 - Kernel density for number of announced interventions",
               png = T,
               pdf = T,
               jpg = T,
               eps = T)


### Figure 7
# Make sure all combinations are represented
temp <- data.frame("exporter" = unique(table3$exporter),
                   "evaluation" = "harmful, liberalising")
temp <- cSplit(temp, "evaluation", ", ", "long")
temp <- merge(temp, select(country.names, "exporter" = name, un_code), by = "exporter", all.x = T)

table3 <- merge(temp, select(table3, exporter, evaluation, trade.coverage), by = c("exporter", "evaluation"), all.x = T); rm(temp)
table3[is.na(table3)] <- 0

# Plot
fig7 <- ggplot(table3, aes(x=trade.coverage, color = evaluation)) +
  geom_density() +
  labs(x = "Share of trade covered", y = "Density", caption = "Source: Global Trade Alert.") +
  scale_color_manual(values=c(gta_colour$harmful[1], gta_colour$liberalising[1])) +
  guides(colour=guide_legend(title="GTA evaluation", 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right"))+
  gta_theme() +
  theme(legend.position = "bottom")

fig7

gta_plot_saver(plot = fig7,
               path = paste0(gta26.path, out.path),
               name = "Figure 7 - Kernel density for affected export shares",
               png = T,
               pdf = T,
               jpg = T,
               eps = T)


### Figure 8
# Pivot wider to acomodate for facet_zoom
table8.plot <- merge(select(subset(table8, evaluation == "liberalising"), un_code, "trade.coverage.liberalising" = trade.coverage), select(subset(table8, evaluation == "harmful"), un_code, "trade.coverage.harmful" = trade.coverage), by = "un_code", all.x = T)
table8.plot <- merge(table8.plot, select(subset(table8, evaluation == "liberalising"), un_code, log.trade.value))

fig8 <- ggplot(data = table8.plot, aes(x=trade.coverage.liberalising, y=trade.coverage.harmful, size = log.trade.value))+
  geom_point(color = gta_colour$blue[1], alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0) +
  facet_zoom(ylim = c(0,0.1)) +
  # scale_y_continuous(name=str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                    limits = c(0,0.7), labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1),
  #                    sec.axis = sec_axis(trans = ~., name = str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                                        labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1)))+
  scale_y_continuous(name = "Percentage of exports facing harmful measures", labels = label_percent(accuracy = 1L)) +
  scale_x_continuous(name="Percentage of exports facing beneficial measures", labels = label_percent(accuracy = 1L), limits=c(0,0.3), breaks=seq(0,0.3,0.05))+
  labs(caption = "Source: Global Trade Alert.") +
  guides(size=guide_legend(title="Log of 2019 trade value", 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right")) +
  gta_theme() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(size = 8),
        plot.caption.position = "plot",
        legend.position = "bottom")

fig8

gta_plot_saver(plot = fig8,
               path = paste0(gta26.path, out.path),
               name = "Figure 8 - Comparison of harmful and liberalising trade share affected",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 30,
               height = 21)


### Figure 9
# Remove countries that do not have GDP data
table9 <- subset(table9, is.finite(log.gdp.per.capita))

# Pivot wider to acomodate for facet_zoom
table9.plot <- merge(select(subset(table9, evaluation == "liberalising"), un_code, "trade.coverage.liberalising" = trade.coverage), select(subset(table9, evaluation == "harmful"), un_code, "trade.coverage.harmful" = trade.coverage), by = "un_code", all.x = T)
table9.plot <- merge(table9.plot, select(subset(table9, evaluation == "liberalising"), un_code, log.gdp.per.capita))

# Plot
fig9 <- ggplot(data = table9.plot, aes(x=trade.coverage.liberalising, y=trade.coverage.harmful, size = log.gdp.per.capita))+
  geom_point(color = gta_colour$blue[1], alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0) +
  facet_zoom(ylim = c(0,0.1)) +
  # scale_y_continuous(name=str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                    limits = c(0,0.7), labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1),
  #                    sec.axis = sec_axis(trans = ~., name = str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                                        labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1)))+
  scale_y_continuous(name = "Percentage of exports facing harmful measures", labels = label_percent(accuracy = 1L)) +
  scale_x_continuous(name="Percentage of exports facing beneficial measures", labels = label_percent(accuracy = 1L), limits=c(0,0.3), breaks=seq(0,0.3,0.05))+
  labs(caption = "Source: Global Trade Alert. Source of GDP data: World Bank.") +
  guides(size=guide_legend(title="Log of GDP per capita", 
                           label.hjust = 0, label.vjust = 0.5, 
                           title.position = "top", title.hjust = 0, 
                           direction = "horizontal", 
                           label.position = "right")) +
  gta_theme() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(size = 8),
        plot.caption.position = "plot",
        legend.position = "bottom")

fig9

gta_plot_saver(plot = fig9,
               path = paste0(gta26.path, out.path),
               name = "Figure 9 - Comparison of harmful and liberalising trade share affected - GDP version",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 30,
               height = 21)


### Figure 10
# Pivot wider to acomodate for facet_zoom
table10.plot <- merge(select(subset(table10, evaluation == "liberalising"), un_code, "trade.coverage.liberalising" = trade.coverage), select(subset(table10, evaluation == "harmful"), un_code, "trade.coverage.harmful" = trade.coverage), by = "un_code", all.x = T)
table10.plot <- merge(table10.plot, select(subset(table10, evaluation == "liberalising"), un_code, is.g20))

fig10 <- ggplot(data = table10.plot, aes(x=trade.coverage.liberalising, y=trade.coverage.harmful, color = is.g20))+
  geom_point(size = 2, alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0) +
  facet_zoom(ylim = c(0,0.1)) +
  # scale_y_continuous(name=str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                    limits = c(0,0.7), labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1),
  #                    sec.axis = sec_axis(trans = ~., name = str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                                        labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1)))+
  scale_y_continuous(name = "Percentage of exports facing harmful measures", labels = label_percent(accuracy = 1L)) +
  scale_x_continuous(name="Percentage of exports facing beneficial measures", labels = label_percent(accuracy = 1L), limits=c(0,0.3), breaks=seq(0,0.3,0.05))+
  scale_color_manual(values = c("1" = gta_colour$red[1], "0" = gta_colour$blue[1]), labels = c("Non-G20", "G20")) +
  labs(caption = "Source: Global Trade Alert.") +
  guides(color=guide_legend(title=NULL, 
                           label.hjust = 0, label.vjust = 0.5, 
                           title.position = "top", title.hjust = 0, 
                           direction = "horizontal", 
                           label.position = "right")) +
  gta_theme() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(size = 8),
        plot.caption.position = "plot",
        legend.position = "bottom")

fig10

gta_plot_saver(plot = fig10,
               path = paste0(gta26.path, out.path),
               name = "Figure 10 - Comparison of harmful and liberalising trade share affected - G20 highlighted",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 30,
               height = 21)


### Figure 11
# Pivot wider to acomodate for facet_zoom
table11.plot <- merge(select(subset(table11, evaluation == "liberalising"), un_code, "trade.coverage.liberalising" = trade.coverage), select(subset(table11, evaluation == "harmful"), un_code, "trade.coverage.harmful" = trade.coverage), by = "un_code", all.x = T)
table11.plot <- merge(table11.plot, select(subset(table11, evaluation == "liberalising"), un_code, is.ldc))

fig11 <- ggplot(data = table11.plot, aes(x=trade.coverage.liberalising, y=trade.coverage.harmful, color = is.ldc))+
  geom_point(size = 2, alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0) +
  facet_zoom(ylim = c(0,0.1)) +
  # scale_y_continuous(name=str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                    limits = c(0,0.7), labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1),
  #                    sec.axis = sec_axis(trans = ~., name = str_wrap("Percentage of exports facing harmful measures", width = 50),
  #                                        labels = label_percent(accuracy = 1L), breaks=seq(0,0.7,0.1)))+
  scale_y_continuous(name = "Percentage of exports facing harmful measures", labels = label_percent(accuracy = 1L)) +
  scale_x_continuous(name="Percentage of exports facing beneficial measures", labels = label_percent(accuracy = 1L), limits=c(0,0.3), breaks=seq(0,0.3,0.05))+
  scale_color_manual(values = c("1" = gta_colour$red[1], "0" = gta_colour$blue[1]), labels = c("Other", "Least developed country")) +
  labs(caption = "Source: Global Trade Alert.") +
  guides(color=guide_legend(title=NULL, 
                            label.hjust = 0, label.vjust = 0.5, 
                            title.position = "top", title.hjust = 0, 
                            direction = "horizontal", 
                            label.position = "right")) +
  gta_theme() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(size = 8),
        plot.caption.position = "plot",
        legend.position = "bottom")

fig11

gta_plot_saver(plot = fig11,
               path = paste0(gta26.path, out.path),
               name = "Figure 11 - Comparison of harmful and liberalising trade share affected - LDCs highlighted",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 30,
               height = 21)
