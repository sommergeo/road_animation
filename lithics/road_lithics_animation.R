setwd("C:/data/roceeh/road_locality_animation")

library(ggplot2)
library(readr)
library(ggthemes)
library(gganimate)
library(gifski)
library(ggalt)
library(rnaturalearth)
library(patchwork)

### Load ROAD exported csv and preprocess
ROAD_all <- read_delim("CS_ROAD_ages_lithics.csv", ";", escape_double = FALSE, trim_ws = TRUE)
ROAD_all$query_age_min <- as.integer(ROAD_all$query_age_min/1000) 
ROAD_all$query_age_max <- as.integer(ROAD_all$query_age_max/1000)
ROAD_all$range <- as.integer(ROAD_all$query_age_max-ROAD_all$query_age_min) # calculate legth of occupation
#ROAD_all <- subset(ROAD_all, query_age_min >= 40)
ROAD_all <- tidyr::drop_na(ROAD_all, c(query_age_min,query_age_max)) # remove NAs from age columns
ROAD_all$steps <- as.integer(cut(ROAD_all$query_age_min, # categorize in 10ka-slices
                breaks= seq(-1,3000,10), label=FALSE)*10)
ROAD_all$steps_exp <- log(ROAD_all$steps) #optional step for sped-up animation


### World map from natural earth (rendering 10x faster)
world_shape <- ne_countries(scale = 110, type = 'countries', continent = c('europe', 'africa', 'asia', 'oceania'), returnclass = 'sp')

world <- ggplot() +
  borders(world_shape, colour = 'grey', fill = 'grey') +
  coord_proj("+proj=robin +lon_0=50", xlim = c(-20, 180), ylim = c(-50, 90)) +
  theme_map()
plot(world)


### Create Map with ggplot and gganimate
## Linear time (first 10 seconds = 1 Ma are super boring)
map <- world +
  geom_point(data=ROAD_all, aes(x=locality.x, y=locality.y, group=assemblage.name, col=assemblage.name),
             size=2, colour = 'red', alpha = 1) +
  transition_time(time=-ROAD_all$steps) +
  #transition_states(ROAD_all$steps)+
  #transition_components(time=ROAD_all$query_age_min, range=ROAD_all$range)+
  #transition_events(start=ROAD_all$query_age_min, end=ROAD_all$query_age_max, range = ROAD_all$range, enter_length = as.integer(0), exit_length = as.integer(0))+
  shadow_mark(past=TRUE, size=2, colour="black", alpha=.1) +
  enter_fade() +
  exit_fade() +
  labs(title = '{-frame_time} ka BP') +
  theme(plot.title = element_text(size=18))

# Test animation
animate(map, fps=5, duration=10, start_pause = 0, end_pause = 0)

# Export as gif
anim <- animate(map, fps=30, duration=31, end_pause = 1, renderer = gifski_renderer(loop = FALSE)) # ca. 5 mins rendering
anim_save(filename="road_lithics3MaLIN_30sec_30fps.gif", animation = anim)


## Exponential
map <- world +
  geom_point(data=ROAD_all, aes(x=locality.x, y=locality.y, group=assemblage.name, col=assemblage.name),
             size=2, colour = 'red', alpha = 1) +
  transition_time(time=-ROAD_all$steps_exp) +
  shadow_mark(past=TRUE, size=2, fill="black", colour=NA, alpha=.1) +
  enter_fade() +
  exit_fade() +
  labs(title = '{round(exp(-frame_time))} ka BP') +
  theme(plot.title = element_text(size=18))

# Test animation
animate(map, fps=5, duration=10, start_pause = 0, end_pause = 0)

# Export as gif
anim <- animate(map, fps=30, duration=31, end_pause = 1, renderer = gifski_renderer(loop = FALSE))
anim_save(filename="road_lithics3MaEXP_30sec_30fps.gif", animation = anim)

     