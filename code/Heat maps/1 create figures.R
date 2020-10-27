rm(list = ls())

### REQUEST:
# 1. Please produce a heat map where for each G20 country the number of hits to every other G20 members commercial interests are shown. This heat map should be
# based only on harmful policy interventions implemented this year.
#
# 2. Please repeat 1 but for liberalising policy interventions.
#
# 3. Please repeat 1 (again for harmful policy interventions) but the countries affected in this heat map are the following groups: the Least Developed Countries,
# Sub-Saharan Africa, African Union, Latin American and Caribbean nations, EBRD countries of operation, ASEAN, East Asia and Pacific, and Eurasian Economic Union.
# As the number of countries in each of group of nations differs, then please produce the heat map based on the total number of hits to a group divided by the total
# number of members of each group. Please take account of the fact that a G20 member’s policy intervention can hurt more than one member of a group—therefore, could
# each harmed country towards the total number of hits by a G20 country on a given group’s commercial interests.
#
# 4. Please repeat 3 but for liberalising policy interventions.

library(gtalibrary)
library(tidyverse)
library(openxlsx)

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/Heat maps/"
out.path = "tables & figures/9 - Heat maps/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

# Load data
load(paste0(gta26.path, data.path, "heat maps.Rdata"))

### Save Excel files of the data
# Convert to more readable format
t1 <- pivot_wider(select(subset(table1, gta.evaluation == "harmful"), -gta.evaluation), id_cols = c("implementing.jurisdiction", "affected.jurisdiction"), names_from = "affected.jurisdiction", values_from = "nr.of.interventions")
t2 <- pivot_wider(select(subset(table1, gta.evaluation == "liberalising"), -gta.evaluation), id_cols = c("implementing.jurisdiction", "affected.jurisdiction"), names_from = "affected.jurisdiction", values_from = "nr.of.interventions")
names(t1) <- c("implementer/affected", names(t1)[-1])
names(t2) <- c("implementer/affected", names(t2)[-1])

t3 <- pivot_wider(select(subset(table2, gta.evaluation == "harmful"), -gta.evaluation), id_cols = c("implementing.jurisdiction", "affected.jurisdiction"), names_from = "affected.jurisdiction", values_from = "nr.of.interventions.per.member")
t4 <- pivot_wider(select(subset(table2, gta.evaluation == "liberalising"), -gta.evaluation), id_cols = c("implementing.jurisdiction", "affected.jurisdiction"), names_from = "affected.jurisdiction", values_from = "nr.of.interventions.per.member")
names(t3) <- c("implementer/affected", names(t3)[-1])
names(t4) <- c("implementer/affected", names(t4)[-1])

# Save
write.xlsx(list("harmful" = t1, "liberalising" = t2), file = paste0(gta26.path, out.path, "Figure 1 & 2 data.xlsx")); rm(t1, t2)
write.xlsx(list("harmful" = t3, "liberalising" = t4), file = paste0(gta26.path, out.path, "Figure 3 & 4 data.xlsx")); rm(t3, t4)


### Functions
heatmap.function <- function(data, aux.table, z.aes, scale.colours, diagonal.fill, legend.breaks, legend.labels, legend.no.value.label, legend.limits, x.axis.title, y.axis.title, plot.title, plot.name, plot.width, plot.height){
  plot <- ggplot(data = data) +
    geom_tile(aes(x = forcats::fct_reorder(affected.jurisdiction, desc(affected.jurisdiction)), y = forcats::fct_inorder(implementing.jurisdiction, ordered = T), color = as.factor(1)), fill = gta_colour$grey[4], size = 0.2, na.rm = F)+
    geom_tile(aes(x = forcats::fct_reorder(affected.jurisdiction, desc(affected.jurisdiction)), y = forcats::fct_inorder(implementing.jurisdiction, ordered = T), fill = z.aes), color = "#FFFFFF", size = 0.2, na.rm = F)+
    geom_tile(data = aux.table, aes(x = forcats::fct_reorder(affected.jurisdiction, desc(affected.jurisdiction)), y = forcats::fct_inorder(implementing.jurisdiction, ordered = T)), fill = diagonal.fill, color = "#FFFFFF", size = 0.2, na.rm = F)+
    gta_theme(x.bottom.angle = 90, x.bottom.align = 1)+
    scale_fill_gradientn(name = "", colours = scale.colours, breaks = legend.breaks, labels = legend.labels,
                         limits = legend.limits, guide = guide_colorbar(barwidth = 15, label.hjust = 0.5))+
    scale_colour_manual(values = "#FFFFFF", label = legend.no.value.label)+
    labs(x = x.axis.title, y = y.axis.title)+
    ggtitle(plot.title)+
    guides(colour = guide_legend(title = NULL, position = "right", barwidth = 1, label.position = "bottom", keywidth = 0, hjust = 0, label.hjust = 0))+
    theme(panel.background = element_blank(), 
          panel.border = element_rect(size = 1, colour = gta_colour$grey[4], fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1, vjust = 0.5),
          legend.position = "bottom")
  
  plot
  
  gta_plot_saver(plot = plot,
                 path = paste0(gta26.path, out.path),
                 name = plot.name,
                 png = T,
                 pdf = T,
                 jpg = T,
                 width = plot.width,
                 height = plot.height)
}

### Figure 1
# Create auxiliary table
diagonal.table1 <- subset(table1, implementing.jurisdiction == affected.jurisdiction)

# Plot
heatmap.function(data = subset(table1, gta.evaluation == "harmful"), z.aes = subset(table1, gta.evaluation == "harmful")$nr.of.interventions, diagonal.fill = "#FFFFFF",
                 aux.table = diagonal.table1, plot.title = paste0("Number of harmful interventions implemented until ", format(as.Date(cutoff.date), "%d %B %Y")),
                 scale.colours = c(gta_colour$red[4], gta_colour$red[1], "#b3143d"), legend.breaks = seq(0,45,5), legend.labels = seq(0,45,5),
                 legend.limits = c(0,45), legend.no.value.label = "No interventions", x.axis.title = "Affected country", y.axis.title = "Implementing country",
                 plot.name = "Figure 1 - Harmful G20 interventions heatmap", plot.width = 21, plot.height = 21)


### Figure 2
# Plot
heatmap.function(data = subset(table1, gta.evaluation == "liberalising"), z.aes = subset(table1, gta.evaluation == "liberalising")$nr.of.interventions, diagonal.fill = "#FFFFFF",
                 aux.table = diagonal.table1, plot.title = paste0("Number of liberalising interventions implemented until ", format(as.Date(cutoff.date), "%d %B %Y")),
                 scale.colours = c(gta_colour$green[4], gta_colour$green[1], "#1d6626"), legend.breaks = seq(0,70,10), legend.labels = seq(0,70,10),
                 legend.limits = c(0,70), legend.no.value.label = "No interventions", x.axis.title = "Affected country", y.axis.title = "Implementing country",
                 plot.name = "Figure 2 - Liberalising G20 interventions heatmap", plot.width = 21, plot.height = 21)


### Figure 3
# Create auxiliary table
diagonal.table2 <- unique(select(table2, implementing.jurisdiction, affected.jurisdiction)) ## NOTE: there is no diagonal here, so the diagnonal.fill is "transparent"
                                                                                            ## there still needs to be an auxiliary table argument regardless

# Plot
heatmap.function(data = subset(table2, gta.evaluation == "harmful"), z.aes = subset(table2, gta.evaluation == "harmful")$nr.of.interventions.per.member, diagonal.fill = "transparent",
                 aux.table = diagonal.table2, plot.title = paste0("Number of harmful interventions per country in the\nrespective group implemented until ", format(as.Date(cutoff.date), "%d %B %Y")),
                 scale.colours = c(gta_colour$red[4], gta_colour$red[1], "#b3143d"), legend.breaks = seq(0,21,3), legend.labels = seq(0,21,3),
                 legend.limits = c(0,21), legend.no.value.label = "No interventions", x.axis.title = "Affected country group", y.axis.title = "Implementing country",
                 plot.name = "Figure 3 - Harmful G20 interventions per country group heatmap", plot.width = 21, plot.height = 21)


### Figure 4
# Plot
heatmap.function(data = subset(table2, gta.evaluation == "liberalising"), z.aes = subset(table2, gta.evaluation == "liberalising")$nr.of.interventions.per.member, diagonal.fill = "transparent",
                 aux.table = diagonal.table2, plot.title = paste0("Number of liberalising interventions per country in the\nrespective group implemented until ", format(as.Date(cutoff.date), "%d %B %Y")),
                 scale.colours = c(gta_colour$green[4], gta_colour$green[1], "#1d6626"), legend.breaks = seq(0,30,5), legend.labels = seq(0,30,5),
                 legend.limits = c(0,30), legend.no.value.label = "No interventions", x.axis.title = "Affected country group", y.axis.title = "Implementing country",
                 plot.name = "Figure 4 - Liberalising G20 interventions per country group heatmap", plot.width = 21, plot.height = 21)
