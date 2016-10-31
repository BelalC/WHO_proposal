setwd('~/Documents/Coursera/data_incubator/project_idea/')

### download data

library(WHO)
library(dplyr)
library(magrittr)
library(stringr)

library(ggplot2)

library(scales)
library(showtext)
library(scatterD3) # note - this is the GitHub version so we have ellipses
library(RColorBrewer)
library(dygraphs)

codes <- get_codes()

child_codes <- codes[grepl("[Cc]hild", codes$display), ]
df <- get_data("WHOSIS_000001")
df1 = df[df$region=="Africa",]

df1 %>% 
    group_by(region, year) %>%
    summarise(value = mean(value)) %>% 
    ggplot(aes(x = year, y = value, color = region, linetype = region)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "Life expectancy at birth (years)\n", 
         linetype = NULL, color = NULL,
         title = "Evolution of life expectancy (by region)\n")

####### EDUCATION

mean_years = read.csv('./wicdf (1).csv', row.names = NULL, skip = 8, header = T)
attainment = read.csv('./wicdf (2).csv', row.names = NULL, skip = 8, header = T)
africa = read.csv('./wicdf (5).csv', row.names = NULL, skip = 8, header = T)
world = read.csv('./wicdf (6).csv', row.names = NULL, skip = 8, header = T)

africa_cut = africa[africa$Year<=2015,]
africa_cut = africa_cut[africa_cut$Year>=1990,]

africa_cut %>% 
    group_by(Education) %>%
    ggplot(aes(x = Year, y = Distribution, color = Education, linetype = Education)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "school attainment\n", 
         linetype = NULL, color = NULL,
         title = "Education\n")

subset(world, world$Education=="Post Secondary") %>% 
    group_by(Education, Area) %>%
    ggplot(aes(x = Year, y = Distribution, color = Area, linetype = Area)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "school attainment\n", 
         linetype = NULL, color = NULL,
         title = "Education\n")
