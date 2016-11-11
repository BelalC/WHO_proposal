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

### INFANT MORTALITY DATA - WHO
mdg_codes <- codes[grepl("^MDG", codes$label), ]
mdg_codes$number <- as.numeric(str_sub(mdg_codes$label, start = -2))

mdg1 <- get_data("MDG_0000000001")

### LIFE EXPECTANCY DATA - WHO
life_expectancy <- get_data("WHOSIS_000001")
life_expectancy <- life_expectancy[life_expectancy$region==c("Africa","Europe"),]

### Water + Air DATA - WHO

water_1 <- get_data("WSH_1")
water_1 <- water_1[water_1$region==c("Africa","Europe"),]

water_1K <- get_data("WSH_3")
water_1K <- water_1K[water_1K$region==c("Africa","Europe"),]

water_daly <- get_data("WSH_7")
water_daly <- water_daly[water_daly$region==c("Africa","Europe"),]

air_1 <- get_data("AIR_5")
air_1 <- air_1[air_1$region==c("Africa","Europe"),]

nam = unique(water_1K$country) 
nam_2 = income$country
names = nam[(nam %in% nam_2)]

water_1K = water_1K[water_1K$country %in% names,]
water_1K = data.frame(water_1K)

for (i in 1:nrow(water_1K)){
    water_1K[i,7] = income[water_1K[i,3] == income$country,1]
}

#
nam = unique(air_1$country) 
nam_2 = income$country
names = nam[(nam %in% nam_2)]

air_1K <- air_1[air_1$country %in% names,]
air_1K <- data.frame(air_1K)

for (i in 1:nrow(air_1K)){
    air_1K[i,7] = income[air_1K[i,3] == income$country,1]
}


####### EDUCATION DATA - downloaded from Wittgenstein Centre for Demography & Global Human Capital

world = read.csv('../project_idea/wicdf (6).csv', row.names = NULL, skip = 8, header = T)

africa_b = read.csv('data/wicdf (8).csv', row.names = NULL, skip = 8, header = T)
europe_b = read.csv('data/wicdf (9).csv', row.names = NULL, skip = 8, header = T)

#income grouping - derived from WHO data

income = mdg1[,c(1,5)]
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

# world education attainment
subset(world, world$Education=="Upper Secondary") %>% 
    group_by(Education, Area) %>%
    ggplot(aes(x = Year, y = Distribution, color = Area, linetype = Area)) +
    geom_line(size = 1) +
    theme_light(9) +
    labs(x = NULL, y = "primary school attainment\n", 
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

boxplot( (subset(Y$Distribution, Y$Education=="Primary"))~subset(Y$incomegroup, Y$Education=="Primary"), horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
title("European primary education attainement ")
boxplot((subset(Y$Distribution, Y$Education=="Lower Secondary"))~subset(Y$incomegroup, Y$Education=="Lower Secondary"),horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
         title("European lower secondary school education attainement ")
boxplot( (subset(Y$Distribution, Y$Education=="Upper Secondary"))~subset(Y$incomegroup, Y$Education=="Upper Secondary"),horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
title("European upper secondary school education attainement ")
boxplot( (subset(Y$Distribution, Y$Education=="Post Secondary"))~subset(Y$incomegroup, Y$Education=="Post Secondary"),horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
title("European post secondary school education attainement")

boxplot( (subset(X$Distribution, X$Education=="Primary"))~subset(X$incomegroup, X$Education=="Primary"), horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
title("African primary education attainement ")
boxplot((subset(X$Distribution, X$Education=="Lower Secondary"))~subset(X$incomegroup, X$Education=="Lower Secondary"),horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
title("African lower secondary school education attainement ")
boxplot( (subset(X$Distribution, X$Education=="Upper Secondary"))~subset(X$incomegroup, X$Education=="Upper Secondary"),horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
title("African upper secondary school education attainement ")
boxplot( (subset(X$Distribution, X$Education=="Post Secondary"))~subset(X$incomegroup, X$Education=="Post Secondary"),horizontal = TRUE, ylab = "WHO-defined income", xlab = "Percentage achieved")
title("African post secondary school education attainement")
