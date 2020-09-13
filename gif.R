library(tidyverse)
library(ggmap)
library(ggthemes)
library(gganimate)
```


WHO <- readr::read_csv(here::here("content/post/_data/WHO-COVID-19-09-09-2020.csv")) %>% 
  janitor::clean_names() %>% 
  filter(country == "Mexico")




world <- map_data("world")

mexico_usa <- world %>% 
  filter(region %in% c("Mexico", "USA", "Canada", "Cuba"))



mexico_usa %>% 
  ggplot(aes(long, lat, group = group))+
  xlim(c(min(mexico_usa$long), -50)) +
  theme_map() +
  geom_polygon(color = 'white')+
  annotate("label", x = mean(mexico$long), y = mean(mexico$lat), label = "Mexico") +
  annotate("label", x = mean(mexico$long), y = (mean(mexico$lat)+ 15), label = "USA")


WHO %>% 
  pivot_longer(cols = c("new_cases","cumulative_cases", "new_deaths", "cumulative_deaths"), names_to = "type", values_to = "cases") %>% 
  mutate(cases = as.numeric(cases)) %>% 
  filter(type %in% c("new_cases", "new_deaths")) %>% 
  ggplot(aes(x = date_reported, y = cases, group = type, color = type)) +
  geom_line() +
  geom_point() +
  transition_reveal(date_reported)

anim_save(here::here("new_cases_and_new_deaths_in_mexico.gif"))


