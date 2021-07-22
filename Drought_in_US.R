# title: drought conditions in the US
# author: Smitom Borah
# date: 07/22/2021

# background----
# This R script contains code that was used to generate a plot of drought conditions
# in the US over a given time period. Most of the code is developed following
# https://kpress.dev/blog/2021-07-20-tidy-tuesday-drought-conditions/ .


# clearing the global environment----
rm(list = ls())

# loading necessary packages----
library(tidyverse)
library(janitor)
library(maps)
library(gganimate)
library(gifski)
library(viridis)

# loading the .csv file and clean up----
dr_county <- read_csv('Drought US data/dm_export_20190820_20210713.csv') %>%
    pivot_longer(cols = None:D4, names_to = 'drought_lvl', values_to = 'area_pct') %>%
    clean_names() %>%
    mutate(drought_lvl = factor(drought_lvl, levels = c('D4', 'D3', 'D2', 'D1', 'D0', 'None')))

temp_drought <- dr_county %>%
    group_by(fips, valid_start) %>%
    arrange(fips, valid_start, desc(area_pct), drought_lvl) %>%
    slice(1)

# mapping----
data('county.fips') # from maps package

county.fips <- county.fips %>%
    mutate(region = word(polyname, 1, sep = ","),
           subregion = word(polyname, 2, sep = ",")) %>%
    mutate(subregion = word(subregion, 1, sep = ":")) %>%
    mutate(fips = str_pad(as.character(fips), side = "left", width = 5, "0"))

## US map
map_usa <- map_data("county")

map_usa <- map_usa %>%
    left_join(county.fips)

## Correction for Shannon county, SD
series <- tibbletime:: create_series("2019-08-20"~"2021-07-13", "weekly") %>%
    mutate(join_col = 1) %>%
    rename("valid_start" = date)

map_usa <- map_usa %>%
    mutate(join_col = 1) %>%
    left_join(series)

map_usa <- map_usa %>%
    left_join(temp_drought)

temp_nas <- map_usa %>%
    filter(region == "south dakota", subregion %in% c("shannon", "bennett")) %>%
    arrange(valid_start, subregion)

temp_nas <- temp_nas %>%
    fill(c(map_date, state:area_pct)) %>%
    mutate(county = replace_na(county, "Shannon County")) %>%
    filter(county == "Shannon County")

map_usa <- map_usa %>%
    filter(fips != '46113') %>%
    bind_rows(temp_nas)

map_usa <- map_usa %>%
    mutate(drought_lvl = fct_recode(drought_lvl,
                                    "Abnormally Dry" = "D0",
                                    "Moderate" = "D1",
                                    "Severe" = "D2",
                                    "Extreme" = "D3",
                                    "Exceptional" = "D4"))


# plotting the data----
map_usa %>%
    ungroup() %>%
    filter(valid_start == max(valid_start)) %>%
    ggplot(aes(long,lat, group = group)) +
    geom_polygon(aes(fill=as.integer(fct_rev(drought_lvl))))+
    borders("state")+
    coord_map()+
    scale_fill_viridis( option = "B",
                        breaks = c(1,6),
                        labels = c('None', 'Exceptional'))+
    theme_minimal()+
    labs(title = "Drought level in US",
         x = "",
         y = "")+
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          line = element_blank())



# Animating the plot----
p <- map_usa %>%
    ungroup() %>%
    filter(valid_start > "2020-01-01") %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill=as.integer(fct_rev(drought_lvl))))+
    borders("state")+
    coord_map()+
    scale_fill_viridis( option = "B",
                        breaks = c(1,6),
                        labels = c('None', 'Exceptional'))+
    theme_minimal()+
    transition_manual(frames = valid_start)+
    labs(title = "Drought level in US",
         subtitle = "Week:{current_frame}",
         x = "",
         y = "")+
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          line = element_blank())
animate(p, renderer = gifski_renderer("drought.gif"))
