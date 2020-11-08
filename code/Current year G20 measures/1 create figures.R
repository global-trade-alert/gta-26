rm(list = ls())

### REQUEST:
# 1. Please produce a line chart for the years 2009 to 2020 with the following variable on the primary vertical axis: Total number of G20 commercial policy
# interventions worldwide documented by 21 October. On the second axis: please plot the percentage of G20 commercial policy interventions that are harmful.
# For the primary axis title use the text highlighted in bold. For the secondary axis title please use “Percentage of G20 measures that harm trading partners.”
#
# 2. For each G20 country please add up the total number of harmful measures implemented this year, the total number of liberalising measures implemented this
library(gtalibrary)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(scales)
library(stringr)
library(openxlsx)
#year, and the total number of contingent protection investigations started this year but which have not resulted in duties imposed. Rank the G20 nations by
# these totals. Please produce a 2-D bar chart with G20 country as a separate entry where red is used to indicate the number of harmful interventions, green is
# used to indicate the liberalising interventions, and yellow to indicate the contingent protection investigations started this year. Please indicate in the chart
# the number of measures in each category for each G20 member. So for each G20 member, the number associated with each colour segment in the bar will be reported.
#
# 3. For each G20 member, focusing only on the harmful interventions implemented this year, please produce a 2-D 100% stacked bar which indicates the % of measures
# that have already lapsed (by 21 October), the % of measures due to lapse during the remainder of this year, the % of measures due to lapse in 2021, the % of
# measures due to lapse after 2021, and the % of measures without phase out dates.



gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Current year G20 measures/"
out.path = "tables & figures/7 - Current year G20 measures/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "current year g20 measures.Rdata"))

### Save Excel files of the data
write.xlsx(table1, file = paste0(gta26.path, out.path, "Figure 1 data.xlsx"))
write.xlsx(table2, file = paste0(gta26.path, out.path, "Figure 2 data.xlsx"))
write.xlsx(table3, file = paste0(gta26.path, out.path, "Figure 3 data.xlsx"))


### Figure 1
fig1 <- ggplot(data=table1)+
  geom_line(aes(x=forcats::fct_inorder(as.character(year.implemented), ordered = T), y=total.nr.of.int, group=1), size=1, colour=gta_colour$qualitative[1])+
  geom_line(aes(x=forcats::fct_inorder(as.character(year.implemented), ordered = T), y=harmful.perc*1400, group=1), size=1, colour=gta_colour$red[1]) +
  scale_y_continuous(name=str_wrap("Total number of G20 commercial policy interventions worldwide implemented by 31 October", 55),
                     labels=seq(0,1400,200), limits = c(0,1400), breaks=seq(0,1400,200),
                     sec.axis = sec_axis(trans = ~./1400, name=str_wrap("Percentage of G20 measures that harm trading partners", 55),
                                         breaks=seq(0,1,0.1),labels=label_percent(accuracy = 1L)))+
  labs(x = "", caption = "Source: Global Trade Alert.")+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 1) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(size=.1, color="#555555"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption.position = "plot",
        axis.text.y.left = element_text(colour = gta_colour$qualitative[1]),
        axis.text.y.right = element_text(colour = gta_colour$red[1]))

fig1

gta_plot_saver(plot = fig1,
               path = paste0(gta26.path, out.path),
               name = "Figure 1 - G20 interventions implemented by 21 October",
               png = T,
               pdf = T,
               jpg = T,
               eps = T)


### Figure 2
# Transform data to longer format
table2 <- pivot_longer(table2, cols = c("nr.of.harmful.int", "nr.of.liberalising.int", "nr.of.cont.protection.int"), names_to = "intervention.type", values_to = "nr.of.interventions")

# Abbreviate USA and UK
table2$implementing.jurisdiction <- as.character(table2$implementing.jurisdiction)
table2$implementing.jurisdiction[table2$implementing.jurisdiction == "United States of America"] <- "USA"
table2$implementing.jurisdiction[table2$implementing.jurisdiction == "United Kingdom"] <- "UK"

# Plot
fig2 <- ggplot(data = table2)+
  geom_bar(aes(x=forcats::fct_reorder(implementing.jurisdiction, desc(-total.nr.of.interventions)), y=nr.of.interventions, fill=factor(intervention.type, levels=c("nr.of.cont.protection.int", "nr.of.liberalising.int", "nr.of.harmful.int"))), width=0.65, stat = "identity") +
  geom_label(aes(x=forcats::fct_reorder(implementing.jurisdiction, desc(-total.nr.of.interventions)), y=nr.of.interventions, label=nr.of.interventions), colour="#3c3c3c", label.size = 0.2, label.padding = unit(0.15, "lines"), size=2.5, position = position_stack(vjust = 0.5)) +
  labs(x="", y="", caption = "Source: Global Trade Alert.") +
  scale_y_continuous(name=str_wrap("Number of measures recorded", 40), breaks=seq(0,275,25), labels=seq(0,275,25),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Number of measures recorded", 40), breaks=seq(0,275,25), labels=seq(0,275,25)))+
  scale_fill_manual(values=c("nr.of.harmful.int" = gta_colour$harmful[1], "nr.of.liberalising.int" = gta_colour$liberalising[1],"nr.of.cont.protection.int" = gta_colour$amber[1]), labels = c("contingent protection investigations", "liberalising interventions", "harmful interventions"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 0)+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, nrow = 2))+
  theme(axis.text.x.bottom = element_text(hjust=1, size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color="transparent"),
        legend.position = "bottom",
        legend.background = element_rect(fill="transparent", colour = "transparent"),
        legend.margin = margin(t=10,b=0,r=100,l=100),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank())

fig2

gta_plot_saver(plot = fig2,
               path = paste0(gta26.path, out.path),
               name = "Figure 2 - G20 interventions by evaluation and contingent protection investigations",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               width = 21,
               height = 21)


### Figure 3
# Transform the data to longer format
table3 <- pivot_longer(table3, cols = names(table3)[-1], names_to = "intervention.type", values_to = "perc.of.interventions")
table3$implementing.jurisdiction <- as.character(table3$implementing.jurisdiction)

# Abbreviate USA and UK
table3$implementing.jurisdiction[table3$implementing.jurisdiction == "United States of America"] <- "USA"
table3$implementing.jurisdiction[table3$implementing.jurisdiction == "United Kingdom"] <- "UK"

# Plot
fig3 <- ggplot(data = table3)+
  geom_bar(aes(x=forcats::fct_inorder(implementing.jurisdiction, ordered = T), y=perc.of.interventions, fill=forcats::fct_inorder(intervention.type)), width=0.65, stat = "identity") +
  labs(x="", y="", caption = "Source: Global Trade Alert.") +
  scale_y_continuous(name=str_wrap("Percentage of measures recorded", 40), breaks=seq(0,1,0.1), labels=label_percent(accuracy = 1L),
                     sec.axis = sec_axis(trans = ~., name=str_wrap("Percentage of measures recorded", 40), breaks=seq(0,1,0.1), labels=label_percent(accuracy = 1L)))+
  scale_fill_manual(values=c("perc.of.lapsed.interventions" = gta_colour$qualitative[5], "perc.of.interventions.to.lapse.rest.of.year" = gta_colour$qualitative[4], "perc.of.interventions.to.lapse.2021" = gta_colour$qualitative[3],
                             "perc.of.interventions.to.lapse.after.2021" = gta_colour$qualitative[2], "perc.of.interventions.without.phaseout.date" = gta_colour$qualitative[1]),
                    labels = c("Lapsed by 31 October 2020", "Lapses in last two months of 2020", "Lapses in 2021",     
                               "Lapses after 2021", "Has no phase-out date"))+
  gta_theme(x.bottom.angle = 90, x.bottom.align = 0)+
  guides(guide_legend(title = NULL, label.hjust = 0, label.vjust = 0.5, title.position = "top", title.vjust = 0.5, nrow = 2))+
  theme(axis.text.x.bottom = element_text(hjust=1, size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color=gta_colour$qualitative[1]),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank())

fig3

gta_plot_saver(plot = fig3,
               path = paste0(gta26.path, out.path),
               name = "Figure 3 - G20 interventions by phase out date",
               png = T,
               pdf = T,
               jpg = T,
               eps = T,
               height = 21,
               width = 25)
