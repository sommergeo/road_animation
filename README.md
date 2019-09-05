# road_animation
 This repo provides tools for the animation of <a href="http://www.roceeh.uni-tuebingen.de/roadweb/smarty_road_simple_search.php" target="_blank">ROAD</a> content in space and time. The ROCEEH out of Africa Database (ROAD) is being assembled since 2008 by the <a href="http://www.roceeh.net" target="_blank">ROCEEH</a> and serves information about human origins from Africa and Eurasia between ~3 Ma and ~20 ka. 

## Content
 - <a href="/lithics" target="_blank">Occurence of lithic tools</a> over the past 3 Ma 
 - <a href="/input" target="_blank">Growth of the ROAD database</a> since 2009 animated by the first creation of assemblages
 
## Example
 This animation shows the occurence of lithic tools between 3 Ma and recent. The associated ROAD query is  
  ```sql
 SELECT DISTINCT on (assemblage.name, locality.idlocality, locality.x, locality.y, query_age_min, query_age_max, age_comments) assemblage.name, locality.idlocality, locality.x, locality.y, query_age_min, query_age_max, age_comments FROM assemblage, locality,all_age(0,3000000) as (locality varchar,assemblage int, assemblage_name varchar,query_age_min int, query_age_max int, age_comments varchar) WHERE ((locality.idlocality = assemblage.locality_idlocality and (assemblage.category ilike '%raw material%' or assemblage.category ilike '%technology%' or assemblage.category ilike '%typology%')) and locality = assemblage.locality_idlocality and assemblage = assemblage.idassemblage) ORDER BY locality.idlocality
 ``` 
 Red dots indicate the occurence of lithic tools at archaeological sites at the respective time-slice (based on assemblage dates). Black dots mark abandoned sites. Since evidence is rather sparse in the first ~1.5 Ma, the animation runs fast in the beginning and becomes exponentially slower. 
 We used a <a href="https://geography.wisc.edu/maplibrary/the-robinson-projection/" target="_blank">Robinson projection</a> centered around 50°E to show a suitable trade-off between level of distortion and visual appeal. 
  
![Fast animation of human tool use](/lithics/road_lithics3MaEXP_30sec_30fps_loop.gif)
 
 
 *Occurence of lithic tools between 3 Ma and 10 ka*