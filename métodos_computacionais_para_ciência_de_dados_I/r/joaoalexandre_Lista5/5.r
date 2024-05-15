library(shiny)
library(ggplot2)

# Define available datasets
datasets <- c("economics", "faithfuld", "seals")

# User interface
ui <- fluidPage(
    selectInput("dataset", "Dataset", choices = datasets),
    verbatimTextOutput("summary"),
    plotOutput("plot")  # Corrected to display plots properly
)

# Server logics
server <- function(input, output, session) {
    # Reactive expression to fetch the dataset
    dataset <- reactive({
        get(input$dataset, "package:ggplot2")
    })
    
    # Generate and display the summary of the dataset
    output$summary <- renderPrint({
        summary(dataset())  # Corrected to use the reactive expression
    })
    
    # Generate and display the plot of the dataset
    output$plot <- renderPlot({
        plot(dataset())  # Corrected to use the reactive expression
    }, res = 96)
}

# Run the application
shinyApp(ui, server)

shiny::runApp()