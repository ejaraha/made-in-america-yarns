library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage(
        theme = shinytheme("spacelab"),
        title = 'MiAY Order Explorer',
        # define layout
        sidebarLayout(
            # space for variables
            sidebarPanel(
                sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
            ),
    
            # space for plot
            mainPanel(
               plotOutput("order-vs-time")
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
