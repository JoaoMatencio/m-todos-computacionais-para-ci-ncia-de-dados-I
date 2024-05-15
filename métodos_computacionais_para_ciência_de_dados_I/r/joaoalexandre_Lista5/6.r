library(shiny)

# Define the user interface (UI) layout
ui <- fluidPage(
    titlePanel("Client Registration and Lottery"), # Title of the application
    sidebarLayout(
        sidebarPanel(
            textInput("name", "Full Name:"), # Input for full name
            dateInput("dob", "Date of Birth:", value = Sys.Date()), # Input for date of birth
            selectInput("gender", "Gender:", choices = c("Male", "Female", "Other")), # Dropdown for selecting gender
            numericInput("lucky_number", "Lucky Number:", value = 50, min = 1, max = 100) # Input for lucky number between 1 and 100
        ),
        mainPanel(
            textOutput("result") # Output where the result will be displayed
        )
    )
)

# Define server logic to process inputs and compute the output
server <- function(input, output) {
    output$result <- renderText({
        set.seed(Sys.Date()) # Set seed for reproducibility based on the current date
        random_number <- sample(1:100, 1) # Generate a random number between 1 and 100
        # Determine if the user won or lost based on the distance between lucky number and random number
        if (abs(input$lucky_number - random_number) <= 10) {
            result <- "won"
        } else {
            result <- "lost"
        }
        paste("Hello,", input$name, "you", result, "!") # Construct the greeting message based on the result
    })
}

# Run the Shiny application
shinyApp(ui, server)

shiny::runApp()