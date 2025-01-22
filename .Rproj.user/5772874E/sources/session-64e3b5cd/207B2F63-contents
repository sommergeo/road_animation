library(tidyverse)
library(gganimate)
library(assertthat)
library(RPostgres)
library(sf)

# Option 1: Connect directly to ROAD
# The example clause queries stone tools (raw material + technology + typology) between 20 ka and 3 Ma bp
road_connection <- dbConnect(RPostgres::Postgres(), dbname = "roceeh", host="134.2.216.14", port=5432, user=rstudioapi::askForPassword("Database username"), password=rstudioapi::askForPassword("Database password"))
query <- "SELECT DISTINCT on (assemblage.name, locality.idlocality, locality.x, locality.y, query_age_min, query_age_max, age_comments) assemblage.name, locality.idlocality, locality.x, locality.y, query_age_min, query_age_max, age_comments FROM assemblage, locality,all_age(20000,3000000) as (locality varchar,assemblage int, assemblage_name varchar,query_age_min int, query_age_max int, age_comments varchar) WHERE ((locality.idlocality = assemblage.locality_idlocality and (assemblage.category ilike '%raw material%' or assemblage.category ilike '%technology%' or assemblage.category ilike '%typology%')) and locality = assemblage.locality_idlocality and assemblage = assemblage.idassemblage) ORDER BY locality.idlocality"
d <- dbGetQuery(road_connection, query)

# Option 2: Load a csv downloaded from the ROAD Website
d <- readr::read_delim('input/input_file.csv', ';', escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(x = locality.x, y = locality.y, idlocality = locality.idlocality)

# Preprocess  
d <- d %>% 
  drop_na(c(query_age_min,query_age_max)) %>%                                          # Remove all assemblages with missing ages
  mutate(steps=as.integer(cut(query_age_min,                                           # convert to 10ka-slices
                              breaks= seq(20000,3000000,10000), label=FALSE)*10)) %>%  
  mutate(steps_exp=log(steps)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
d <- st_transform(d, crs = "+proj=robin +lon_0=50")

# Load world map from natural earth (rendering 10x faster)
world_shape <- rnaturalearth::ne_countries(scale = 110, type = 'countries', continent = c('europe', 'africa', 'asia', 'oceania'), returnclass = 'sf') %>% 
  filter(!name %in% c("French Guiana", "Guadeloupe", "Martinique", "Réunion", "Mayotte", 'New Caledonia'))

world <- ggplot() +
  geom_sf(data=world_shape, colour='#B9BCB8', fill='#B9BCB8')+
  theme_void()

plot(world)


# Rendering with exponential speed
map <- world +
  geom_sf(data = d, aes(col=idlocality, group=idlocality), size = 2, colour = '#A51E37', alpha = 1) +
  coord_sf(crs="+proj=robin +lon_0=50", xlim=c(-6000000,12000000))+
  #geom_point(data=d, aes(x=x, y=y, group=idlocality, col=idlocality),
  #           size=2, colour = '#A51E37', alpha = 1)+
  transition_time(time=-d$steps_exp) +
  shadow_mark(past=TRUE, size=2, colour='#32414B', alpha=.1) +
  enter_fade() +
  exit_fade() +
  labs(title = '{format(round(exp(-frame_time))*1000, big.mark = ",", scientific=F)} years ago ') +
  theme(plot.title = element_text(size=18, colour='#32414B'))

animate(map, fps=5, duration=5, start_pause = 0, end_pause = 0) # short test render

# Export
animate(map, fps=24, duration=20, start_pause = 48, end_pause = 0, bg='white', width=20, height=15, units='cm', res=300)  #render
anim_save(filename="output/road_animation.gif", animation = last_animation())
