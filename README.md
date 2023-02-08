# road_animation
 This repo provides tools for the spatio-temporal visualization of contents queried from <a href="http://www.roceeh.uni-tuebingen.de/roadweb/smarty_road_simple_search.php" target="_blank">ROAD</a>. The ROCEEH out of Africa Database (ROAD) is being assembled since 2008 by the <a href="http://www.roceeh.net" target="_blank">ROCEEH</a> and serves information about human origins from Africa and Eurasia between ~3 Ma and ~20 ka. 

## Example
 This animation shows the occurence of lithic tools between 3 Ma and 20 ka. 
 
 ![Animation of human lithic tool use](/output/road_animation.gif)
 
 Red dots indicate the occurence of lithic tools at archaeological sites at the respective time-slice (based on assemblage dates). Black dots mark abandoned sites. Since evidence is rather sparse in the first ~1.5 Ma, the animation runs faster in the beginning and becomes exponentially slower. 
 We used the <a href="https://geography.wisc.edu/maplibrary/the-robinson-projection/" target="_blank">Robinson projection</a> centered around 50°E to show a suitable trade-off between level of distortion and visual appeal. 

 The visualization results from the following ROAD SQL query:  
  ```sql
 SELECT DISTINCT on (assemblage.name, locality.idlocality, locality.x, locality.y, query_age_min, query_age_max, age_comments) assemblage.name, locality.idlocality, locality.x, locality.y, query_age_min, query_age_max, age_comments FROM assemblage, locality,all_age(0,3000000) as (locality varchar,assemblage int, assemblage_name varchar,query_age_min int, query_age_max int, age_comments varchar) WHERE ((locality.idlocality = assemblage.locality_idlocality and (assemblage.category ilike '%raw material%' or assemblage.category ilike '%technology%' or assemblage.category ilike '%typology%')) and locality = assemblage.locality_idlocality and assemblage = assemblage.idassemblage) ORDER BY locality.idlocality
 ``` 

## How to use
 With this tool you can visualize any CSV files queried from the <a href="http://www.roceeh.uni-tuebingen.de/roadweb/smarty_road_simple_search.php" target="_blank">ROAD</a> database, as long as it includes the following attributes:
* locality.x
* locality.y
* query_age_min
* query_age_max

Save it to `input/input_file.csv` (or adjust the import function) and run the script.


## Citation
This work is distributed under <a href="https://creativecommons.org/licenses/by/4.0/" target="_blank">CC-BY 4.0</a> license. This means you are allowed to **share** and **adapt** data and products. In return, you are requested to give appropriate **credit**.

Please cite `Christian Sommer` or this repository `https://github.com/sommergeo/road_animation`.

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a>