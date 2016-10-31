setwd('~/Documents/Coursera/data_incubator/challenge_proposal/')


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

### download data from WHO

codes <- get_codes()

child_codes <- codes[grepl("[Cc]hild", codes$display), ]

### INFANT MORTALITY DATA - WHO
mdg_codes <- codes[grepl("^MDG", codes$label), ]
mdg_codes$number <- as.numeric(str_sub(mdg_codes$label, start = -2))

mdg1 <- get_data("MDG_0000000001")

### LIFE EXPECTANCY DATA - WHO
df <- get_data("WHOSIS_000001")
df1 = df[df$region=="Africa",]

####### EDUCATION DATA - downloaded from Wittgenstein Centre for Demography & Global Human Capital

africa_a = read.csv('./wicdf (7).csv', row.names = NULL, skip = 8, header = T)
africa_b = read.csv('./wicdf (8).csv', row.names = NULL, skip = 8, header = T)

africa = read.csv('../project_idea/wicdf (5).csv', row.names = NULL, skip = 8, header = T)
world = read.csv('../project_idea/wicdf (6).csv', row.names = NULL, skip = 8, header = T)
europe_b = africa_b = read.csv('./wicdf (9).csv', row.names = NULL, skip = 8, header = T)

africa_cut = africa[africa$Year<=2015,]
africa_cut = africa_cut[africa_cut$Year>=1990,]

#income grouping - derived from WHO data

income = mdg1[,4:5]
income = income[!duplicated(income$country),]
income = data.frame(income)
income = na.omit(income)

nam = unique(africa_b$Area) 
nam_2 = income$country
names = nam[(nam %in% nam_2)]

africa_b$incomegroup = NA
africa_b_cut = africa_b[africa_b$Area %in% names,]

for (i in 1:nrow(africa_b_cut)){
    
    africa_b_cut[i,5] = income[africa_b_cut[i,1] == income$country,1]
    
}

africa_b_cut = na.omit(africa_b_cut)

nam = unique(europe_b$Area)
nam_2 = income$country
names = nam[(nam %in% nam_2)]

#to-do: make this into function....

europe_b$incomegroup = NA
europe_b_cut = europe_b[europe_b$Area %in% names,]

for (i in 1:nrow(europe_b_cut)){
    
    europe_b_cut[i,5] = income[europe_b_cut[i,1] == income$country,1]
    
}

europe_b_cut = na.omit(europe_b_cut)

### PLOTS

df1 %>% 
    group_by(region, year) %>%
    summarise(value = mean(value)) %>% 
    ggplot(aes(x = year, y = value, color = region, linetype = region)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "Life expectancy at birth (years)\n", 
         linetype = NULL, color = NULL,
         title = "Evolution of life expectancy (by region)\n")

subset(world, world$Education=="Post Secondary") %>% 
    group_by(Education, Area) %>%
    ggplot(aes(x = Year, y = Distribution, color = Area, linetype = Area)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "school attainment\n", 
         linetype = NULL, color = NULL,
         title = "Education\n")

africa_cut %>% 
    group_by(Education) %>%
    ggplot(aes(x = Year, y = Distribution, color = Education, linetype = Education)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "school attainment\n", 
         linetype = NULL, color = NULL,
         title = "Education\n")

X = africa_b_cut[africa_b_cut$Education!="Total",]

    X[X$Education=="Primary",] %>%
    group_by(incomegroup) %>%
    ggplot(aes(x = Year, y = Distribution, color = incomegroup, linetype = incomegroup)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "school attainment\n", 
         linetype = NULL, color = NULL,
         title = "Education\n")

    ### BOXPLOTS
    
boxplot( (subset(X$Distribution, X$Education=="Primary"))~subset(X$incomegroup, X$Education=="Primary"))
boxplot( (subset(X$Distribution, X$Education=="Under 15"))~subset(X$incomegroup, X$Education=="Under 15"))
boxplot( (subset(X$Distribution, X$Education=="Lower Secondary"))~subset(X$incomegroup, X$Education=="Lower Secondary"))
boxplot( (subset(X$Distribution, X$Education=="Upper Secondary"))~subset(X$incomegroup, X$Education=="Upper Secondary"))
boxplot( (subset(X$Distribution, X$Education=="Post Secondary"))~subset(X$incomegroup, X$Education=="Post Secondary"))

Y = europe_b_cut[europe_b_cut$Education!="Total",]

boxplot( (subset(Y$Distribution, Y$Education=="Primary"))~subset(Y$incomegroup, Y$Education=="Primary"), horizontal = TRUE)
boxplot( (subset(Y$Distribution, Y$Education=="Under 15"))~subset(Y$incomegroup, Y$Education=="Under 15"))
boxplot( (subset(Y$Distribution, Y$Education=="Lower Secondary"))~subset(Y$incomegroup, Y$Education=="Lower Secondary"))
boxplot( (subset(Y$Distribution, Y$Education=="Upper Secondary"))~subset(Y$incomegroup, Y$Education=="Upper Secondary"))
boxplot( (subset(Y$Distribution, Y$Education=="Post Secondary"))~subset(Y$incomegroup, Y$Education=="Post Secondary"))