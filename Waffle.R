
#Datos de
# Meyer, R. S., & Purugganan, M. D. (2013). Evolution of crop species: genetics of domestication and diversification. Nature reviews genetics, 14(12), 840.
# y
#Gruber, K. (2017). Agrobiodiversity: The living library. Nature, 544(7651), S8-S8.

library(googleVis)
library(tidyverse)
library(readxl)
library(waffle)


value <- c(5000, 25, 25, 15, 3)

value <- c(47634, 2500, 250, 15, 3)/3
waffle(value, rows=100, size = 0.1, reverse = F) 


  geom_label(aes(x = 50, y = 50, label = "50,000 especies \n comestibles"), hjust = 0, vjust = 0.5, 
            lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
            family = "Helvetica", size = 3) +
  geom_curve(aes(x = 150, y = 50, xend = 100, yend = 100),
             curvature = 0.2, arrow = arrow(length = unit(0.02, "npc")), 
             colour = "#555555", size = 0.1) +
  geom_label(aes(x = 100, y = 100, label = "2,500 especies\n semidomesticadas"), hjust = 0, vjust = 0.5, 
             lineheight = 0.8, colour = "#555555", fill = "white", label.size = NA, 
             family = "Helvetica", size = 3)


library(vegan)
  
uno <- sample(1:100, 15)/3
uno
dos <- floor(uno)
ceiling(dos)

tres <- sum(100 * (dos / sum(dos, na.rm = T)))

waffle(tres, rows = 10, size = 0.1, reverse = F) 

parts <- sample(1:100, 8)
sum(parts)
parts1 <- 100 * (parts/sum(parts * 1.05))
parts2 <- ceiling(parts1)
parts2
Diff <- 100 - sum(parts2)
Diff
parts2[1] <- parts2[1] + Diff
parts2

sum(parts2)
waffle(parts2, rows = 10, size = 0.1) +
  theme(legend.title = element_text(color = "black", size = 9),
        legend.text = element_text(size = 5))
  

  
  
  
  
  
  
  
  
  
    