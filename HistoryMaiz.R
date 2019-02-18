library(tidyverse)
library(readxl)
library(gridExtra)
library(ggalt)
library(scales)
library(bbplot)
library(GGally)
library(PerformanceAnalytics)
library(ggrepel)



setwd("~/Dropbox/GitHub/Conabio-PGMaices/")
dir()

##### Birds
Tabla1 <- read_excel("PGM_update2017.xlsx", sheet = "PGM_maices_Alex", col_names = T)

dim(Tabla1)
names(Tabla1)
str(Tabla1)

Tabla1a <- Tabla1 %>%
  select(Raza_primaria,AltitudProfundidad) %>%
  filter(Raza_primaria != "ND") %>%
  #filter(AltitudProfundidad > 20) %>%
  filter(AltitudProfundidad < 5000) %>%
  as.data.frame()

Tabla1a
Tabla2 <- aggregate(x = Tabla1a$AltitudProfundidad, by = list(Tabla1a$Raza_primaria), FUN = min)  
names(Tabla2)[2] <- c("Min")
Tabla3 <- aggregate(x = Tabla1a$AltitudProfundidad, by = list(Tabla1a$Raza_primaria), FUN = max)  
names(Tabla3)[2] <- c("Max")
Tabla3a <- aggregate(x = Tabla1a$AltitudProfundidad, by = list(Tabla1a$Raza_primaria), FUN = median)  
names(Tabla3a)[2] <- c("Median")
  
Tabla4 <- full_join(Tabla2,Tabla3, by= "Group.1") %>%
  full_join(Tabla3a, by= "Group.1")
names(Tabla4)[1] <- c("Raza")

Tabla4 <- Tabla4 %>%
  mutate(gap = Max - Min)

head(Tabla4)
#Make plot
ggplot(Tabla4) + 
  geom_dumbbell(aes(x = Min, xend = Max, y = reorder(Raza, Max)),
                colour = "#dddddd",
                size = 1,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  theme_classic() +
  #geom_point(data =  Tabla4, aes(x = Median y =reorder(Raza, -gap)))
  labs(title = "Razas de maíces y el rango de altitud de crecimiento", x = "Altitud (m)", y = "", fill = "") +
  coord_flip() +
  theme(legend.position = "", axis.text.x = element_text(angle = 90, size = 10, hjust = 1, vjust = 0),
        axis.text.y = element_text(size = 11), axis.title = element_text(size = 11), legend.text = element_text(size = 11)) +
  geom_vline(aes(xintercept = 2065), linetype = "dotted") +
  geom_label(aes(x = 2700, y = 20, label = "Cueva de \n Guilá Naquitz \n Oaxaca (6229BP)"), hjust = 0, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 2065, y = 13, xend = 2700, yend = 20),
             curvature = -0.3, arrow = arrow(length = unit(0.02, "npc")), 
             colour = "#555555", size = 0.1) + 
  geom_label(aes(x = 3400, y = 49, label = "Felipe Neri, \nMorelos"), hjust = 0, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 3564, y = 63, xend = 3500, yend = 54),
             curvature = 0.2, arrow = arrow(length = unit(0.02, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_vline(aes(xintercept = 1346), linetype = "dotted") +
  geom_label(aes(x = 3000, y = 6, label = "Xihuatoxtla Huitzuco de los \n Figueroa \n Guerrero (8750BP)"), hjust = 0, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 1346, y = 2, xend = 2800, yend = 7),
             curvature = -0.1, arrow = arrow(length = unit(0.02, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_vline(aes(xintercept = 57), linetype = "dotted") +
  geom_label(aes(x = 2400, y = 7, label = "Pijijiapan (SOC05-2) \n Pijijiapan \n Chiapas (5789BP)"), hjust = 0, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 57, y = 3, xend = 2200, yend = 8),
             curvature = -0.1, arrow = arrow(length = unit(0.02, "npc")), 
             colour = "#555555", size = 0.1) + 
  geom_label(aes(x = 3400, y = 9, label = "Sitios Arqueológicos \n origen del maíz\n(años antes del presente BP) "), hjust = 0, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 4)
  

#Altura de la cueva Guilá Naquitz 2065 metros
  

  bbc_style()
  labs(title="We're living longer",
       subtitle="Biggest life expectancy rise, 1967-2007")

  

Maiz1 <- read_excel("~/Dropbox/JANO/2019/CONABIO/Maices/BaseMaicesNativos.xlsx", sheet = "MaicesNativos22931", col_names = T)

dim(Maiz1)
head(Maiz1)
names(Maiz1)
Maiz1a <- Maiz1 %>%
  select(Raza_primaria, Complejo_racial, Longitud_de_mazorca:Diámetro_de_olote) %>%
  filter(Raza_primaria != "ND") %>%
  filter(Diametro_de_mazorca < 37) %>%
  as.data.frame()
head(Maiz1a)
summary(Maiz1a)
#Correlación


Maiz2 <- data.frame(Maiz1a[,-c(1:2)])
head(Tabla2)

chart.Correlation(Maiz2,
                  method = "kendall",
                  histogram = TRUE,
                  pch = 16)



ggcorr(Maiz2, palette = "RdBu", label = TRUE)

#######


Maiz3a <- aggregate(x = Maiz1a$Longitud_de_mazorca, by = list(Maiz1a$Raza_primaria, Maiz1a$Complejo_racial), FUN = min)  
names(Maiz3a)[3] <- c("Min")
Maiz3b <- aggregate(x = Maiz1a$Longitud_de_mazorca, by = list(Maiz1a$Raza_primaria, Maiz1a$Complejo_racial), FUN = max)  
names(Maiz3b)[3] <- c("Max")
Maiz3c <- aggregate(x = Maiz1a$Longitud_de_mazorca, by = list(Maiz1a$Raza_primaria, Maiz1a$Complejo_racial), FUN = mean)  
names(Maiz3c)[3] <- c("Median")

Maiz4 <- full_join(Maiz3a,Maiz3b, by= c("Group.1", "Group.2")) %>%
  full_join(Maiz3c, by= c("Group.1", "Group.2")) %>%
  na.omit()
names(Maiz4)[1] <- c("Raza")
names(Maiz4)[2] <- c("Complejo_Racial")

Maiz4 <- Maiz4 %>%
  mutate(gap = Max - Min)

head(Maiz4)
#Make plot
ggplot(Maiz4) + 
  geom_dumbbell(aes(x = Min, xend = Max, y = reorder(Raza, Max)),
                colour = "#dddddd",
                size = 1,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1",
                dot_guide = T,
                dot_guide_size = 0.05) +
  geom_point(aes(x = Median, y = reorder(Raza, Max)), size = 1, colour = "#9fb059") +
  theme_classic() +
  labs(title = "Tamaño de mazorcas", x = "Longitud (cm)", y = "Razas de maíces", fill = "",
       family = "Helvetica") +
  #coord_flip() +
  theme(legend.position = "", axis.text.x = element_text(angle = 0, size = 8, hjust = 1, vjust = 0),
        axis.text.y = element_text(size = 8), axis.title = element_text(size = 11), legend.text = element_text(size = 11)) +
  geom_vline(xintercept = c(10, 15, 20, 25), linetype = "dotted") +
  geom_label(aes(x = 5, y = 27, label = "Muy \ncorta"), hjust = 1, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 4) +
  geom_label(aes(x = 13.5, y = 27, label = "Corta"), hjust = 1, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 4) +
  geom_label(aes(x = 18.5, y = 27, label = "Medio"), hjust = 1, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 4) +
  geom_label(aes(x = 23.5, y = 27, label = "Largo"), hjust = 1, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 4) +
  geom_label(aes(x = 28, y = 27, label = "Muy \n largo"), hjust = 1, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 4) +
  geom_label(aes(x = 5, y = 47, label = "Valor \n mínimo"), hjust = 1, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 5, y = 47, xend = 8.7, yend = 49),
                 curvature = -0.3, arrow = arrow(length = unit(0.02, "npc")), 
                 colour = "#555555", size = 0.1) +
  geom_label(aes(x = 31, y = 37, label = "Valor \n máximo"), hjust = 1, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 29.5, y = 37, xend = 23.6, yend = 39),
             curvature = -0.3, arrow = arrow(length = unit(0.02, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_label(aes(x = 24, y = 17, label = "Promedio"), hjust = 1, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 22.3, y = 17, xend = 17.1, yend = 19.9),
             curvature = -0.3, arrow = arrow(length = unit(0.02, "npc")), 
             colour = "#555555", size = 0.1)





head(Maiz1a)
Maiz5 <- aggregate(x = c(Maiz1a$Longitud_de_mazorca), by = list(Maiz1a$Raza_primaria), FUN = mean)  
names(Maiz5) <- c("Raza", "Longitud")
Maiz5a <- aggregate(x = c(Maiz1a$Diametro_de_mazorca), by = list(Maiz1a$Raza_primaria), FUN = mean)  
names(Maiz5a) <- c("Raza", "Diametro")

Maiz5b <- full_join(Maiz5,Maiz5a, by= c("Raza")) %>%
  na.omit()

names(Maiz5b)
g <- ggplot(data = Maiz5b, mapping = aes(x = Longitud, y = Diametro))
g <- g + geom_point(pch = 21, size = 2.5, fill = "red", color = "red", alpha = 0.7) +
  #theme_classic() +
  theme_bw(base_line_size = 0) +
  geom_smooth(method = 'loess') +
  geom_text_repel(aes(label = Raza),  hjust = 0.5, vjust = 0.5, color = "black", size = 4) +
  labs(title = "", x = "Longitud (cm)", y = "Diametro (cm)", fill = "") +
  #scale_x_continuous( labels = scales::number_format(accuracy = 0.1)) +
  #theme(legend.position = "", axis.text.x = element_text(angle = 0, size = 12, hjust = 0, vjust = 0.5),
  #      axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), axis.title = element_text(size = 15))
g

  
  
#Datos Arqueológicos

library(tidyverse)
library(readxl)
library(gridExtra)
library(ggalt)
library(scales)
library(bbplot)
library(GGally)
library(PerformanceAnalytics)
library(ggrepel)

Arqueo <- read_excel("~/Dropbox/JANO/2019/CONABIO/Maices/MuestrasMaiz.xls", sheet = "MexicoArqueo", col_names = T)
names(Arqueo)
attach(Arqueo)

Arqueo1 <- aggregate(Arqueo[,8:9], by = list(Nombre, País, Estado, Ubicación), FUN = mean, na.rm = T)
names(Arqueo1)
names(Arqueo1)[1:4] <- c("Nombre", "País", "Estado", "Ubicación")

detach(Arqueo)  

head(Arqueo1)
dim(Arqueo)
dim(Arqueo1)


ggplot(Arqueo1) + 
  geom_dumbbell(aes(xend = FechaCalibradaMenor, x = FechaCalibradaMayor, y = reorder(Nombre, FechaCalibradaMenor)),
                colour = "#cccccc",
                size = 1,
                colour_x = "#fdae6b",
                colour_xend = "#fdae6b",
                dot_guide = T,
                dot_guide_size = 0.1) +
  scale_x_continuous(trans = "reverse") +
  theme_classic() +
  geom_text(aes(x = 10000, y = Nombre, vjust = 0, hjust = 0.4, label = Estado),size = 3, family = "Helvetica", colour = "#636363") +
  labs(title = "Maíz de México en el tiempo", x = "Años hasta nuestros días", y = "Sitios Arqueológicos", fill = "") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 10), axis.title = element_text(size = 11)) +
  geom_label(aes(x = 600, y = 24, label = "Fuente:\n http://es.ancientmaize.com/ \n & Meyer et al. (2012)"), hjust = 0, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 2.5) +
  geom_hline(yintercept = 12.5, linetype = "solid", colour = "#969696", size = 0.7) +
  geom_segment(aes(x = 508, y = 12.5, xend = 508, yend = 9), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 508, y = 8, label = "Descubrimiento de \n América"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 2000, y = 12.5, xend = 2000, yend = 9), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 2000, y = 8, label = "Nacimiento de \n Cristo"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 2100, y = 12.5, xend = 2100, yend = 11), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 2100, y = 10, label = "Fundación de Palenque"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 3250, y = 12.5, xend = 3250, yend = 4), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 3250, y = 3, label = "Guerra de\n Troya"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 6100, y = 12.5, xend = 6100, yend = 4), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 6100, y = 3, label = "Primeros\nSistemas de escritura"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 6030, y = 12.5, xend = 6030, yend = 9), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 6030, y = 8, label = "Inicio de\nla Muralla China"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_segment(aes(x = 7000, y = 12.5, xend = 7000, yend = 11), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 7000, y = 10, label = "Primeros habitantes\nen la Venta, Tabs"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) + 
  geom_segment(aes(x = 675, y = 12.5, xend = 675, yend = 4), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 675, y = 3, label = "Fundación de\nTenochtitlán"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 8000, y = 12.5, xend = 8000, yend = 21), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 8000, y = 21, label = "Papa"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 8000, y = 12.5, xend = 8000, yend = 19), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 8000, y = 19, label = "Frijol"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 8000, y = 12.5, xend = 8000, yend = 17), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 8000, y = 17, label = "Calabaza"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 8000, y = 12.5, xend = 8000, yend = 14), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 8000, y = 14, label = "Trigo"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 10000, y = 12.5, xend = 9000, yend = 16),
             curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_label(aes(x = 9000, y = 16, label = "Cebada"), hjust = 0, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 9000, y = 12.5, xend = 9000, yend = 15), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 9000, y = 15, label = "Agave"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 7000, y = 12.5, xend = 7000, yend = 16), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 7000, y = 16, label = "Aguacate"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_label(aes(x = 7000, y = 14, label = "Plátano"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 8600, y = 12.5, xend = 8600, yend = 14), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 8600, y = 14, label = "Centeno"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 1500, y = 12.5, xend = 1500, yend = 14), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 1500, y = 14, label = "Cacao"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 1000, y = 12.5, xend = 1000, yend = 14), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 1000, y = 14, label = "Tomate"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 6000, y = 12.5, xend = 6000, yend = 16), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 6000, y = 16, label = "Piña"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 6000, y = 12.5, xend = 6000, yend = 14), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 6000, y = 14, label = "Chile"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 4000, y = 12.5, xend = 4000, yend = 14), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 4000, y = 14, label = "Pimienta"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_segment(aes(x = 3300, y = 12.5, xend = 3300, yend = 17), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 3300, y = 17, label = "Canela"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
geom_segment(aes(x = 1400, y = 12.5, xend = 1400, yend = 17), colour = "#969696", size = 0.2) +
  geom_label(aes(x = 1400, y = 17, label = "Café"), hjust = 0.4, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 4926, y = 21, xend = 3000, yend = 23),
             curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#cccccc", size = 0.1) +
  geom_label(aes(x = 3000, y = 22, label = "Fecha calibrada \n menor"), hjust = 0, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) +
  geom_curve(aes(x = 4362, y = 19, xend = 2000, yend = 21),
             curvature = -0.3, arrow = arrow(length = unit(0.01, "npc")), 
             colour = "#cccccc", size = 0.1) +
  geom_label(aes(x = 2000, y = 20, label = "Fecha calibrada \n superior"), hjust = 0, vjust = 0, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3) 

  
  
  
  
  
  

  