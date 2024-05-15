# product: Mostra o produto de x por y.
# product_plus5: Mostra o produto de x por y somado a 5.
# product_plus10: Mostra o produto de x por y somado a 10.

library(shiny)

# Define the user interface
ui <- fluidPage(
    sliderInput("x", "Se x é", min = 1, max = 50, value = 30),
    sliderInput("y", "e y é", min = 1, max = 50, value = 5),
    "então, (x * y) é", textOutput("product"),
    "(x * y) + 5 é", textOutput("product_plus5"),
    "(x * y) + 10 é", textOutput("product_plus10")
)

# Define the server logic
server <- function(input, output, session) {
    # Reactive expression to compute the product of x and y
    product_reactive <- reactive({
        input$x * input$y
    })
    
    output$product <- renderText({
        product_reactive()
    })

    output$product_plus5 <- renderText({
        product_reactive() + 5
    })

    output$product_plus10 <- renderText({
        product_reactive() + 10
    })
}

# Run the application
shinyApp(ui, server)

shiny::runApp()