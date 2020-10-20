rm(list = ls())

### REQUEST:
# 1. Download the World Trade Monitor’s (https://www.cpb.nl/en/worldtrademonitor) latest monthly data (the series is available through to July 2020) and for the
# following nations/groups: World trade, USA (exports), Japan (exports), Euro zone (exports). Setting the value of each nation’s index at 100 at the maximum value
# available from January 2019 through to July 2020, please plot the month by month evolution over time from the start of 2019. Please use following title for Y axis
# “Volume of trade: For each series the index was set at 100 for the month with the maximum recorded volume since January 2019”.  For the X axis title please use “Month”.
#
# 2. Repeat the above for the above charts but for World trade, China (exports), Emerging Asia (excluding China) (exports), Eastern Europe/CIS region (exports),
# and Africa and Middle East (exports). For these two graphs please add a note at the bottom stating “Source: World Trade Monitor.”
#
# 3. Please make a R chart out the Excel file I will attach to the email with this file. The excel file relates to Chinese exports of masks to the United States.
#
# 4. Please confirm the 6 digit UN COMTRADE code for human (not animal) vaccines is 300220. For the years 2015 until the latest year COMTRADE is available, please
# identify the countries that are net importers of human vaccines. Please produce a map identifying those net importers and colour code the countries by the total
# imports per capita of population. For this map please add a note at the bottom stating “Source: UN COMTRADE database used to extract data on HS code 300200.”

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
data.path = "data/World trade monitor & vaccines/"
out.path = "tables & figures/2 - World trade monitor & vaccines/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "world trade monitor & vaccines.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 data.xlsx"))
write.xlsx(table2, file = paste0(gta26.path, out.path, "Figure 2 data.xlsx"))
write.xlsx(table4, file = paste0(gta26.path, out.path, "Figure 4 data.xlsx"))


### Functions
bar.function <- function(data, x=year, y=value, group=group, colours, labels, title, y.axis.label, x.axis.label=NULL, legend.title=NULL, filename, x.breaks=c(2009:2020), x.labels=waiver(), labels.group=NULL, ncols=1){
  plot=ggplot()+
    geom_bar(data=data, aes(x=year, y=value, fill=forcats::fct_inorder(group, ordered = T)), width=.65, stat = "identity")+
    geom_text(data=subset(data, group == labels.group), aes(x=year, y=value, label=scales::percent(value)), size = 3) +
    labs(x=x.axis.label, y=y.axis.label, fill=labels)+
    scale_y_continuous(name=str_wrap(y.axis.label, (nchar(y.axis.label)+20)/2), breaks=c(0,.1,.2,.3,.4,.5,.6), labels=scales::percent(c(0,.1,.2,.3,.4,.5,.6)),
                       sec.axis = sec_axis(trans = ~., name=str_wrap(y.axis.label, (nchar(y.axis.label)+20)/2), breaks=seq(0,1,0.1),labels=scales::percent))+
    scale_fill_manual(values=colours)+
    guides(guide_legend(title = legend.title))+
    ggtitle(title)+
    gta_theme()+
    theme(axis.text.x.bottom = element_text(hjust=0.5, size=10),
          axis.title.y.left = element_text(size=10),
          axis.title.y.right = element_text(size=10),
          legend.text = element_text(size=10),
          legend.title = element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent", color="transparent"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.background = element_rect(fill="transparent", colour = "transparent"),
          legend.margin = margin(t=10,b=0,r=10,l=10),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank())
  
  plot
  
  gta_plot_saver(plot=plot,
                 path=out.path,
                 name=filename,
                 png=T,
                 pdf=T,
                 jpg=T)
}


### Figure 1
# Transform data slightly for nicer x axis labels
# ggplot continually gives "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?" error
table1 <- merge(table1, data.frame("period" = c("2019m01", "2019m02", "2019m03", "2019m04", "2019m05", "2019m06", "2019m07", "2019m08", "2019m09", "2019m10",
                                                "2019m11", "2019m12", "2020m01", "2020m02", "2020m03", "2020m04", "2020m05", "2020m06", "2020m07"),
                                   "month" = c("Jan 19", "Feb 19", "Mar 19", "Apr 19", "May 19", "Jun 19", "Jul 19", "Aug 19", "Sep 19", "Oct 19", "Nov 19",
                                               "Dec 19", "Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20"),
                                   "month.group" = seq(as.Date("2019-01-01"), as.Date("2020-07-31"), by = "1 month")),
                by = "period", all.x = T)


# Plot
fig1 <- ggplot(data=table1)+
  geom_line(aes(x=month.group, y=index, colour=region), size=1)+
  # geom_text(aes(x=month.group, y=index, label=index), vjust=1, colour="#555555", size=2.5)+
  scale_y_continuous(name=str_wrap("Volume of trade: For each series the index was set at 100 for the month with the maximum recorded volume since January 2019", 50),
                     labels=seq(65,100,5), limits = c(65,100), breaks=seq(65,100,5),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Volume of trade: For each series the index was set at 100 for the month with the maximum recorded volume since January 2019", 50),
                                         breaks=seq(65,100,5),labels=seq(65,100,5)))+
  scale_x_date(name="Month", breaks = seq(as.Date("2019-01-01"), as.Date("2020-07-31"), by = "1 month"), labels = unique(table1$month))+
  scale_color_manual(values=c("Euro Area"=gta_colour$qualitative[1],"World trade"=gta_colour$qualitative[2],"Japan"=gta_colour$qualitative[3],
                              "United States"=gta_colour$qualitative[4]), labels=c("Euro Area", "Japan", "United States", "World trade"))+
  labs(caption = "Source: World Trade Monitor.")+
  guides(colour=guide_legend(title=NULL, ncol = 4, 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(size=.1, color="#555555"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption.position = "plot")

fig1

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 1 - WTM volume of trade - developed regions",
               png = T,
               pdf = T,
               jpg = T)


### Figure 2
# Transform data slightly for nicer x axis labels
# ggplot continually gives "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?" error
table2 <- merge(table2, data.frame("period" = c("2019m01", "2019m02", "2019m03", "2019m04", "2019m05", "2019m06", "2019m07", "2019m08", "2019m09", "2019m10",
                                                "2019m11", "2019m12", "2020m01", "2020m02", "2020m03", "2020m04", "2020m05", "2020m06", "2020m07"),
                                   "month" = c("Jan 19", "Feb 19", "Mar 19", "Apr 19", "May 19", "Jun 19", "Jul 19", "Aug 19", "Sep 19", "Oct 19", "Nov 19",
                                               "Dec 19", "Jan 20", "Feb 20", "Mar 20", "Apr 20", "May 20", "Jun 20", "Jul 20"),
                                   "month.group" = seq(as.Date("2019-01-01"), as.Date("2020-07-31"), by = "1 month")),
                by = "period", all.x = T)


# Plot
fig2 <- ggplot(data=table2)+
  geom_line(aes(x=month.group, y=index, colour=region), size=1)+
  # geom_text(aes(x=month.group, y=index, label=index), vjust=1, colour="#555555", size=2.5)+
  scale_y_continuous(name=str_wrap("Volume of trade: For each series the index was set at 100 for the month with the maximum recorded volume since January 2019", 50),
                     labels=seq(65,100,5), limits = c(65,100), breaks=seq(65,100,5),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Volume of trade: For each series the index was set at 100 for the month with the maximum recorded volume since January 2019", 50),
                                         breaks=seq(65,100,5),labels=seq(65,100,5)))+
  scale_x_date(name="Month", breaks = seq(as.Date("2019-01-01"), as.Date("2020-07-31"), by = "1 month"), labels = unique(table1$month))+
  scale_color_manual(values=c("Africa and Middle East"=gta_colour$qualitative[1],"Emerging Asia (excl China)"=gta_colour$qualitative[2],"China"=gta_colour$qualitative[3],
                              "World trade"=gta_colour$qualitative[4], "Eastern Europe / CIS"=gta_colour$qualitative[5]), labels=c("Africa and Middle East", "China", "Eastern Europe / CIS", "Emerging Asia (excl. China)", "World trade"))+
  labs(caption = "Source: World Trade Monitor.")+
  guides(colour=guide_legend(title=NULL, ncol = 4, 
                             label.hjust = 0, label.vjust = 0.5, 
                             title.position = "top", title.hjust = 0, 
                             direction = "horizontal", 
                             label.position = "right"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(size=.1, color="#555555"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption.position = "plot")

fig2

gta_plot_saver(plot = fig2,
               path = paste0(gta26.path, out.path),
               name = "Figure 2 - WTM volume of trade - developing regions",
               png = T,
               pdf = T,
               jpg = T)


### Figure 3
# Plot
fig3 <- ggplot(data = table3)+
  geom_bar(aes(x=forcats::fct_inorder(date, ordered = T), y=trade.value, fill=forcats::fct_reorder(exporter, desc(-trade.value))), width=.65, stat = "identity") +
  labs(x="Month", y="", caption = "Source: Global Trade Alert.", title = "Trade delivered")+
  scale_y_continuous(name=str_wrap("mUSD value of Mask Imports by the United States", 30), breaks=seq(0,4e9,5e8), labels=seq(0,4000,500),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("mUSD value of Mask Imports by the United States", 30), breaks=seq(0,4e9,5e8), labels=seq(0,4000,500)))+
  scale_fill_manual(values=c("China" = gta_colour$qualitative[1], "ROW" = gta_colour$qualitative[2]), labels = c("China", "Rest of the world"))+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1)+
  theme(axis.text.x.bottom = element_text(hjust=0.5, size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color="transparent"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill="transparent", colour = "transparent"),
        legend.margin = margin(t=10,b=0,r=10,l=10),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank())

fig3

gta_plot_saver(plot = fig3,
               path = paste0(gta26.path, out.path),
               name = "Figure 3 - US mask imports - China vs ROW",
               png = T,
               pdf = T,
               jpg = T)


### Figure 4
# Prepare plotting data
world.fig4 <- gta_plot_map_df(table4, countries = "country", values = "net.import.per.capita")

# Plot
fig4 <- ggplot() +
  geom_polygon(data= subset(world.fig4, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
  geom_polygon(data= subset(world.fig4, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=value),na.rm = T, size = 0.15, color = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits=c(-13900000,17000000))+
  labs(x="", y="",caption="Source: Global Trade Alert.") + ### JF: use GTA data not COMTRADE -> caption adjusted
  scale_fill_gradientn(name=str_wrap("Combined net import value in USD per capita",width = 40), 
                       na.value="#c6c6c6",
                       limits=c(0, max(world.fig4$value, na.rm = T)),
                       colors = c(gta_colour$qualitative[2], gta_colour$qualitative[1], "#0c3a52"),
                       breaks=c(round(seq(0, max(world.fig4$value, na.rm = T), max(world.fig4$value, na.rm=T) / 4))),
                       guide=guide_colorbar(barwidth=13, label.hjust = 0.5, title.position = "top"),
                       labels = c(round(seq(0, max(world.fig4$value, na.rm = T), max(world.fig4$value, na.rm=T) / 4))))+
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
        plot.caption = element_text(hjust = 0.5, vjust = 0.8, margin = margin(t=30),size=8, color="#777777",lineheight = 1))

fig4

# Save the map
gta_plot_saver(plot = fig4,
               path = paste0(gta26.path, out.path),
               name = "Figure 4 - Vaccine import map",
               png = T,
               pdf = T,
               jpg = T)

