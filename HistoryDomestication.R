library(tidyverse)
library(readxl)
library(gridExtra)
library(ggalt)
library(scales)
library(bbplot)
library(GGally)
library(PerformanceAnalytics)
library(ggrepel)
library(RColorBrewer)
library(Taxonstand)
library(ggradar)
library(vegan)



setwd("~/Dropbox/JANO/2019/CONABIO/Valeria_Alvarez/Meyer/")
dir()


Tabla1 <- read_excel("TablaParaR.xlsx", sheet = "Table S5_Annotated matrix", col_names = T)


#TablaPT <- Tabla1 %>%
#  select(Species) %>%
#  as.data.frame()

#names(TablaPT)
#
#val <- TPLck("Capsicum frutescens L.")
#
#INFYS1 <- TPL(TablaPT$Species, corr = T)
#head(INFYS1)
#INFYS2 <- INFYS1 %>%
#  select(New.Genus, New.Species)

#write.table(INFYS2, file = "ResultTaxon.txt", sep = "\t")

Species1 <- read.table("ResultTaxon.txt", sep = "\t", header = T)
dim(Species1)

##### Birds

head(Tabla1)
str(Tabla1)
dim(Tabla1)

Tabla1a <- bind_cols(Species1, Tabla1)
head(Tabla1a)

Tabla2 <- Tabla1a %>%
  #filter(Central_America == 1) %>%
  #filter(Central_Andean == 1) %>%
  #filter(Domesticated == 1) %>%
  #filter(Semidomesticated == 1) %>%
  select(Crop_common_name,New.Genus, New.Species, Earliest_evidence, Earliest_evidence_domestication) %>%
  unite(Species, New.Genus, New.Species, sep = " ") %>%
  filter(Earliest_evidence != "NA") %>%
  filter(Earliest_evidence_domestication != "NA") %>%
  mutate(tiempo_medio = Earliest_evidence + (Earliest_evidence_domestication - Earliest_evidence)/2 ) %>%
  mutate(val = 1)
  
  
head(Tabla2)
summary(Tabla2)
dim(Tabla2)

TablaTT <- Tabla2 %>%
  select

Tabla2[Tabla2$Species == "Capsicum annuum",]

#Del año 0 al 1000
Tabla3.1 <- Tabla2 %>%
  filter(tiempo_medio <= 1000) %>%
  summarise(avg = sum(val))
Tabla3.1
# DEl año 1000 - 2000
Tabla3.2 <- Tabla2 %>%
  filter(tiempo_medio <= 2000) %>%
  filter(tiempo_medio > 1000) %>%
  summarise(avg = sum(val))
Tabla3.2

# DEl año 2000 - 3000
Tabla3.3 <- Tabla2 %>%
  filter(tiempo_medio <= 3000) %>%
  filter(tiempo_medio > 2000) %>%
  summarise(avg = sum(val))
Tabla3.3

# DEl año 3000 - 4000
Tabla3.4 <- Tabla2 %>%
  filter(tiempo_medio <= 4000) %>%
  filter(tiempo_medio > 3000) %>%
  summarise(avg = sum(val))
Tabla3.4

# DEl año 4000 - 5000
Tabla3.5 <- Tabla2 %>%
  filter(tiempo_medio <= 5000) %>%
  filter(tiempo_medio > 4000) %>%
  summarise(avg = sum(val))
Tabla3.5

# DEl año 5000 - 6000
Tabla3.6 <- Tabla2 %>%
  filter(tiempo_medio <= 6000) %>%
  filter(tiempo_medio > 5000) %>%
  summarise(avg = sum(val))
Tabla3.6

# DEl año 6000 - 7000
Tabla3.7 <- Tabla2 %>%
  filter(tiempo_medio <= 7000) %>%
  filter(tiempo_medio > 6000) %>%
  summarise(avg = sum(val))
Tabla3.7

# DEl año 7000 - 8000
Tabla3.8 <- Tabla2 %>%
  filter(tiempo_medio <= 8000) %>%
  filter(tiempo_medio > 7000) %>%
  summarise(avg = sum(val))
Tabla3.8

# DEl año 8000 - 9000
Tabla3.9 <- Tabla2 %>%
  filter(tiempo_medio <= 9000) %>%
  filter(tiempo_medio > 8000) %>%
  summarise(avg = sum(val))
Tabla3.9

# DEl año 8000 - 9000
Tabla3.10 <- Tabla2 %>%
  filter(tiempo_medio <= 10000) %>%
  filter(tiempo_medio > 9000) %>%
  summarise(avg = sum(val))
Tabla3.10

names(Tabla2)

ggplot(Tabla2) + 
  geom_dumbbell(aes(x = Earliest_evidence, xend = Earliest_evidence_domestication, y = reorder(Crop_common_name, -tiempo_medio, family = "Helvetica")),
                colour = "#bababa",
                size = 0.5,
                dot_guide = T,
                dot_guide_size = 0.03,
                colour_x = "#d7191c",
                colour_xend = "#2b83ba") +
  theme_classic() +
  scale_x_continuous(trans = "reverse") +
  geom_point(aes(x = tiempo_medio, y = reorder(Crop_common_name, -tiempo_medio)), size = 1, colour = "#9fb059") +
  geom_text(aes(x = 20000, y = reorder(Crop_common_name, -tiempo_medio), vjust = 0, hjust = 0.8, label = Species),size = 1.4, family = "Helvetica", colour = "#636363") +
  labs(title = "18 mil años de domesticación", x = "Años", y = "") +
  #plot.title = element_text(color = "black", size = 14, face = "bold.italic") + 
  theme(axis.text.x = element_text(size = 8, family = "Helvetica"),
        axis.text.y = element_text(size = 4.5, family = "Helvetica"), axis.title = element_text(size = 12)) +
  geom_vline(xintercept = 1000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 2000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 3000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 4000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 5000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 6000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 7000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 8000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 9000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_vline(xintercept = 10000, linetype = "dotted", colour = "#969696", size = 0.7) +
  geom_label(aes(x = 500, y = 10, label = "8"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 1500, y = 2, label = "22"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 2500, y = 2, label = "15"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 3500, y = 2, label = "16"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 4500, y = 2, label = "19"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 5500, y = 2, label = "13"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 6500, y = 2, label = "16"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 7500, y = 2, label = "16"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 8500, y = 2, label = "10"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 9500, y = 2, label = "11"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_label(aes(x = 17000, y = 80, label = "Valor \n medio"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 10000, y = 12, xend = 17000, yend = 80),
             curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_label(aes(x = 17000, y = 30, label = "Primeros\nregistros"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 14000, y = 11, xend = 17000, yend = 30),
             curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_label(aes(x = 14000, y = 60, label = "Primeras\nevidencia de domesticación"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 8000, y = 19, xend = 14000, yend = 60),
             curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_label(aes(x = 500, y = 21, label = "Especies domésticadas\nen mil años"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#9fb059", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 4500, y = 4.5, xend = 500, yend = 26.5),
             curvature = -0.5, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#9fb059", size = 0.1) + 
  geom_label(aes(x = 15000, y = 145, label = "153 plantas domésticadas\n por la agricultura"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 5) + 
  geom_label(aes(x = 700, y = 3, label = "Fuente:\nMeyer et al. (2012)"), hjust = 0, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 2.5) + 
  geom_segment(aes(x = 9000, y = 140, xend = 8000, yend = 140), colour = "#bababa") + 
  geom_curve(aes(x = 8500, y = 139, xend = 14000, yend = 100),
             curvature = -0.5, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#555555", size = 0.1) + 
  geom_label(aes(x = 15000, y = 100, label = "Período de\n mil años"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3)
  #annotate("text", x = 1, y = 15.5, label = "***")
  
  
head(Tabla1a)

Tabla4 <- Tabla1a %>%
  add_column(Uso = "Uso_Previo") %>%
  select(Crop_common_name,New.Genus, New.Species, Uso, OUcurrency:OUpoison) %>%
  unite(Species, New.Genus, New.Species, sep = " ") %>%
  rename(Currency = OUcurrency, Ritual = OUritual, Cosmetic = OUcosmetic,
         Ornamental = OUornamental, Food = OUfood, Fodder = OUfodder,
         Fiber = OUfiber, Fuel = Oufuel, Alcohol = OUalcohol, 
         Medicine = OUmedicine, Poison = OUpoison)

Tabla5 <- Tabla1a %>%
  add_column(Uso = "Uso_Actual") %>%
  select(Crop_common_name,New.Genus, New.Species, Uso, CUcurrency:CUpoison) %>%
  unite(Species, New.Genus, New.Species, sep = " ") %>%
  rename(Currency = CUcurrency, Ritual = CUritual, Cosmetic = CUcosmetic,
         Ornamental = CUornamental, Food = CUfood, Fodder = CUfodder,
         Fiber = CUfiber, Fuel = CUfuel, Alcohol = CUalcohol, 
         Medicine = CUmedicine, Poison = CUpoison) %>%
  bind_rows(Tabla4)

head(Tabla4)
head(Tabla5)

dim(Tabla4)
dim(Tabla5)
Tabla5$Species

TablaRadar <- Tabla5 %>%
  filter(Species == "Cyperus esculentus") %>%
  tibble::rowid_to_column(var = "Usos") %>%
  select(Uso:Poison) %>%
  rename(group = Uso)
  
TablaRadar
#data.frame(row.names = "Uso" )

ggradar(TablaRadar, grid.max = 1, axis.label.size = 4, 
        label.gridline.min = F, label.gridline.max = F,
        label.gridline.mid =  F, label.centre.y = F,
        background.circle.transparency = 0,
        axis.label.offset = 1.15,
        grid.label.size = 4, axis.line.colour = "grey", 
        group.line.width = 1, group.point.size = 2, 
        background.circle.colour = "grey",
        gridline.min.colour = "grey", gridline.mid.colour = "grey",
        gridline.max.colour = "grey", legend.title = "Maíz")

Tabla5
attach(Tabla5)
Tabla6 <- aggregate(Tabla5[,4:14], by = list(Uso), FUN = sum, na.rm = T)
Tabla6
Tabla6.1 <- decostand(Tabla6[,-1], method = "hellinger", MARGIN = 1)
Tabla6.1 <- decostand(Tabla6.1, method = "total", MARGIN = 2)
Tabla6.1 <- data.frame(Tabla6[,1], Tabla6.1)
names(Tabla6.1)[1] <- c("Group.1")
detach(Tabla5)
Tabla6.2 <- Tabla6.1 %>%
  rename(group = Group.1)

ggradar(Tabla6.1, grid.max = 1, axis.label.size = 4, 
        label.gridline.min = F, label.gridline.max = F,
        label.gridline.mid =  F, label.centre.y = F,
        background.circle.transparency = 0,
        axis.label.offset = 1.15,
        grid.label.size = 4, axis.line.colour = "grey", 
        group.line.width = 1, group.point.size = 2, 
        background.circle.colour = "grey",
        gridline.min.colour = "grey", gridline.mid.colour = "grey",
        gridline.max.colour = "grey", legend.title = "Maíz")
