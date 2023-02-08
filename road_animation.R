library(tidyverse)
library(gganimate)

d <- readr::read_delim("input/input_file2.csv", ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  drop_na(c(query_age_min,query_age_max)) %>% 
  mutate(steps=as.integer(cut(query_age_min, # cut in 10ka-slices
                              breaks= seq(-1000,3000000,10000), label=FALSE)*10)) %>% 
  mutate(steps_exp=log(steps))

## World map from natural earth (rendering 10x faster)
world_shape <- rnaturalearth::ne_countries(scale = 110, type = 'countries', continent = c('europe', 'africa', 'asia', 'oceania'), returnclass = 'sp')

world <- ggplot() +
  borders(world_shape, colour = '#B9BCB8', fill = '#B9BCB8') +
  #coord_proj("+proj=robin +lon_0=50", xlim = c(-20, 180), ylim = c(-50, 90)) +
  coord_map(projection="mollweide", orientation=c(90,50,0), xlim=c(-20,180), ylim = c(-50, 90))+
  theme_void()
plot(world)


## Rendering with exponential speed
map <- world +
  geom_point(data=d, aes(x=locality.x, y=locality.y, group=locality.idlocality, col=locality.idlocality),
             size=2, colour = '#A51E37', alpha = 1)+
  transition_time(time=-d$steps_exp) +
  shadow_mark(past=TRUE, size=2, colour='#32414B', alpha=.1) +
  enter_fade() +
  exit_fade() +
  labs(title = '{format(round(exp(-frame_time))*1000, big.mark = ",", scientific=F)} years ago ') +
  theme(plot.title = element_text(size=18, colour='#32414B'))

animate(map, fps=5, duration=5, start_pause = 0, end_pause = 0)

#Export
animate(map, fps=24, duration=20, start_pause = 48, end_pause = 0, bg='white', width=20, height=15, units='cm', res=300)
anim_save(filename="output/road_animation.gif", animation = last_animation())
