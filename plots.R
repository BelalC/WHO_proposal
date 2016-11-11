library(scatterD3)
library(ggplot2)
library(plotly)

Sys.setenv("plotly_username"="Belal_C")
Sys.setenv("plotly_api_key"="eaxmlmghdu")

setwd("~/Documents/Coursera/data_incubator/challenge_proposal/data/")

load(file = "./africa.RDA")
load(file = "./europe.RDA")

X$incomegroup = substr(X$incomegroup,1,nchar(X$incomegroup)-7)
Y$incomegroup = substr(Y$incomegroup,1,nchar(Y$incomegroup)-7)

plot_ly(data=X, x = ~Distribution, color = ~incomegroup, type = "scatter")

plotly_POST(p, filename = "midwest-boxplots")

###

plot_ly(X, x = ~Distribution, y = ~interaction(incomegroup,Education), color = ~Education, type = "box") %>%
    layout(boxmode = "group", title = "African education attainment", yaxis = list(title = "income group",tickfont=list(size=7)),xaxis = list(title="pop distribution"), margin = list(l = 130,r=30))

plot_ly(Y, x = ~Distribution, y = ~interaction(incomegroup,Education), color = ~Education, type = "box") %>%
    layout(boxmode = "group", title = "European education attainment" ,yaxis = list(title = "income group",tickfont=list(size=7)), xaxis = list(title="pop distribution"),margin = list(l = 130,r=30))


###

plot_ly(X, x = ~Distribution, y = ~incomegroup, color = ~Education, type = "box") %>%
    layout(boxmode = "group",title = "African education attainment", yaxis = list(title = "income group",tickfont=list(size=7)), xaxis = list(title="pop distribution"),margin = list(l = 130,r=30))

plot_ly(Y, x = ~Distribution, y = ~incomegroup, color = ~Education, type = "box") %>%
    layout(boxmode = "group",title="European education attainment",yaxis = list(title = "income group",tickfont=list(size=7)), xaxis = list(title="pop distribution"),margin = list(l = 130,r=30))


plotly_POST(p, filename = "midwest-boxplots")

     
 