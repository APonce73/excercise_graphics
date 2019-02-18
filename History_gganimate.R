
#Data Camp courses
# https://projects.datacamp.com/projects/62

library(tidyverse)
library(gganimate)
library(RColorBrewer)

GA <- pi*(3-sqrt(5))
puntos <- 4000
t <- (1:puntos)*GA
x <- sin(t)
y <- cos(t)
df <- data.frame(t,x,y) %>%
  arrange(t) %>%
  mutate(t1 = t)

df1 <- df %>%
  mutate(t1 = replace(t1, t1 < quantile(t, 0.2), "#d73027")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.2) & t1 <= quantile(t, 0.3), "#f46d43")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.3) & t1 <= quantile(t, 0.4), "#fdae61")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.4) & t1 <= quantile(t, 0.5), "#fee090")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.5) & t1 <= quantile(t, 0.6), "#8c510a")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.6) & t1 <= quantile(t, 0.7), "#bf812d")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.7) & t1 <= quantile(t, 0.8), "#abd9e9")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.8) & t1 <= quantile(t, 0.9), "#74add1")) %>%
  mutate(t1 = replace(t1, t1 > quantile(t, 0.9), "#4575b4"))


head(df1)
p <- ggplot(df1, aes(x*t,y*t))
p1 <- p + geom_point(size = 4, color = df1$t1, alpha = 0.6) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'black', colour = NA), 
  panel.border = element_blank()) +
  transition_reveal(t,t)

animate(p1, nframes = 24, renderer = gifski_renderer("gganim.gif"))

getwd()

