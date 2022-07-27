library(tidyverse)
library(plotly)
library(countrycode)
library(ggrepel)
library(scales)
library(RColorBrewer)

#load table that connects country names with continents (library countrycode)
continents <- codelist %>% 
  select(continent, fao, country.name.en) %>% 
  rename(country=country.name.en)

#load data from FAOSTAT about strawberry production in the world
df <- read.csv('FAOSTAT_data_6-15-2022.csv') %>% 
  select(Area, Element, Value, Area.Code..FAO.) %>% 
  pivot_wider(names_from = Element, values_from = Value) %>% 
  drop_na() %>% 
  mutate('Yield'=Yield/5000)

#rename column names
cols <- c("country","fao","area_harvested","yield",'production')
colnames(df) <- cols


#join dataframes into one, delete China data and choose top 10 producers
df <- left_join(df, continents, by='fao') %>% 
  drop_na()%>% 
  filter(country.y!='China') %>% 
  top_n(10, production)

ggplot(df, aes(y=area_harvested, x=production, size=yield, color=continent))+
  geom_point(alpha=0.25)+
  geom_label_repel(mapping = aes(label=country.y), cex=4, max.overlaps=20, seed = 12)+
  scale_size(range = c(3, 20), labels = comma)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  scale_color_brewer(palette = 'Set1')+
  labs(title = 'Top 10 producers of strawberry in 2020 in the world (except China)',
    y='area harvested [ha]',
       x='production [tonnes]',
       size='yield [tonnes/ha]',
       color='continent')+
  theme_bw()

ggsave('production.png', height=7, width = 9)
