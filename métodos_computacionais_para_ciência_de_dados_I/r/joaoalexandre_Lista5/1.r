library(shiny)

ui <- fluidPage(
   
   # Application title
   titlePanel("Aplicativo de Cumprimento"),
   
   # Sidebar with a text input field for entering a name
   sidebarLayout(
      sidebarPanel(
         textInput("name", "Digite seu nome:", "")
      ),
      
      # Show a greeting output on the main panel
      mainPanel(
         textOutput("greeting")
      )
   )
)

# Define server logic required to greet the user
server <- function(input, output) {
   output$greeting <- renderText({
       if (input$name != "") {
           paste("Olá", input$name, "!")
       }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

shiny::runApp()