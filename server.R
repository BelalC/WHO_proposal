library(shiny)

load("data/africa.RDA")
load("data/europe.RDA")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
    
    # Compute the forumla text in a reactive expression since it is 
    # shared by the output$caption and output$mpgPlot expressions
    formulaText <- reactive({
        paste("Distribution ~", input$variable)
    })
    
    # Return the formula text for printing as a caption
    output$caption <- renderText({
        formulaText()
    })
    
    # Generate a plot of the requested variable against mpg and only 
    # include outliers if requested
    output$africaPlot <- renderPlot({
        boxplot(as.formula(formulaText()), 
                data = X)
    })
})