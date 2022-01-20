http://fronkonstin.com

library(tidyverse)
seq(from=-10, to=10, by = 0.05) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x^2+pi*cos(y)^2), y=(y+pi*sin(x)))) +
  geom_point(alpha=.05, shape=20, size=0)+
  theme_void()+coord_fixed()


library(tidyverse)
seq(from=-5, to=5, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x+pi*sin(-y)), y=(y+pi*sin(x)))) +
  geom_point(alpha=.1, shape=20, size=0) + 
  theme_void()

library(ggplot2)
n=500
ggplot(data.frame(r=sqrt(1:n),t=(1:n)*pi*(3-sqrt(5))),
       aes(x=r*cos(t),y=r*sin(t)))+
  geom_point(aes(size=n-r, alpha=n-r),
             shape=19,colour=(1:n))+
  coord_equal()+cowplot::theme_nothing()


library(tidyverse)
seq(-3,3,by=.01) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(1-x-sin(y^2)), y=(1+y-cos(x^2)))) +
  geom_point(alpha=.05, shape=20, size=0,colour="pink")+
  theme_void()+
  coord_polar()


library(tidyverse)
seq(-3,3,by=.01) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x^3+sin(x)), y=(y^3-cos(y)))) +
  geom_point(alpha=.1, shape=20, size=0)+
  theme_void()+
  coord_polar()


library(tidyverse)
tibble(x=accumulate(1:230,~.x+((0.98)^.y)*cos(.y*(pi/2)^1.015),.init=0),y=accumulate(1:230,~.x+((0.98)^.y)*sin(.y*(pi/2)^1.015),.init=0))%>%
  ggplot(aes(x,y)) + geom_polygon() + coord_equal() + theme_void()


library(tidyverse)
library(imager)

# Point to the place where your image is stored
file <- "/Users/kamranafzali/Sacha.jpeg"
load.image(file) %>%
  grayscale() -> img


file <- "/Users/kamranafzali/LH.png"

lh=load.image(file) 
as.cimg(lh[,,,1:3])%>%
  grayscale() -> img
# The image is summarized into s x s squares 
s <- 10

# Resume pixels using mean: this decreases drastically the resolution of the image
img %>% 
  as.data.frame() %>%
  mutate(x = cut(x, round(dim(img)[1]/s, 0), labels = FALSE),
         y = cut(y, round(dim(img)[2]/s, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) -> df

n=50
# Create new variable to be used to define size and color of the lines of tiles
df %>% mutate(z = cut(value, breaks = n, labels = FALSE)) -> df

# Initialize plot 
plot <- ggplot()

# Resulting plot will be build with 20 layers: one layer per each different value of z 
for (i in 1:n){
  sub_data = df %>% filter(z==i)
  plot <- plot + geom_tile(aes(x, y),
                           size = 2*i/(n-1)-2/(n-1),
                           fill = "black",
                           col = paste0("gray", abs(round(((100-5)*i)/(n-1)+5-(100-5)/(n-1), 0))),
                           data = df %>% filter(z==i))
}

# Last tweaks
plot +
  coord_fixed() +
  scale_y_reverse() +
  theme_void() -> plot

plot

ggsave("LH_tiled.png", plot, height =  8 , width =  6)

library(dplyr) # or install.packages("dplyr") first
library(jasmines)
p0 <- use_seed(100) %>% # Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
  scene_discs(
    rings = 10, 
    points = 50000, 
    size = 50
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 10,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 5,
    scale = .01
  ) %>%
  style_ribbon(
    color = "#E0542E",
    colour = "ind",
    alpha = c(1,1),
    background = "#4D7186"
  )
p0


p1 <- use_seed(100) %>% # Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
  scene_discs(
    rings = 10, 
    points = 50000, 
    size = 50
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 10,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_breeze(
    iterations = 5,
    scale = .01
  )%>%
  style_pop(
    palette = "acton",
    color = "#E0542E",
    colour = "ind",
    background = "warhol"
  )
p1



p2 <- use_seed(100) %>% # Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
  scene_discs(
    rings = 10, 
    points = 5000, 
    size = 50
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 10,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 5,
    scale = .01
  )%>%
  style_pop(
    palette = "acton",
    background = "warhol"
  )
p2



p3 <- use_seed(100) %>% # Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
  scene_bubbles(
    n = 4, 
    grain = 500
  ) %>%
  unfold_warp(
    iterations = 10,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 5,
    scale = .01
  )%>%
  style_pop(
    palette = "acton",
    background = "warhol"
  )
p3

p4 <- use_seed(100) %>% # Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
  scene_bubbles(
    n = 5, 
    grain = 50
  ) %>%
  unfold_loop(
    points = 10,
    radius = 1
  ) %>%
  mutate(seed = 100) %>%
  unfold_tempest(
    iterations = 10,
    scale = .01
  )%>%
  style_walk(
  )
p4