hub_locations %>% filter(geo_type=="state" | abbreviation == "CA") %>% 
  filter(!(fips %in% c("US", "74", "78"))) %>% 
  mutate(name = fct_reorder(location_name, population)) -> dat
  
ggplot(dat, aes(x = name, y = population)) + geom_col() + facet_wrap(~geo_type)
