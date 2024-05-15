library(shiny)

# Define the user interface
ui <- fluidPage(
    titlePanel("Dynamic Multiplication Calculator"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
            sliderInput("y", label = "and y is", min = 1, max = 10, value = 2)
        ),
        mainPanel(
            textOutput("product")
        )
    )
)

# Define the server logic
server <- function(input, output, session) {
    output$product <- renderText({
        input$x * input$y  # Compute the product of x and y
    })
}

# Run the application
shinyApp(ui, server)
