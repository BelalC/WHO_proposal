shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Hello! Data Incubator proposal"),
    
    # Sidebar with controls to select the variable to plot against mpg
    # and to specify whether outliers should be included
    sidebarPanel(
        selectInput("variable", "Variable:",
                    list("incomegroup "="incomegroup"))
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
        h3(textOutput("caption")),
        
        plotOutput("africaPlot")
    )
))
