library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Habit Tracker"),
    
    selectInput("category", "Category", choices = c("Symptoms", "Chores")),
    selectInput("activity", "Activity", choices = c("Mood", "Headache", "Laundry")),
    dateInput("dateTracked", "Date", format = "dd.mm.yyyy", weekstart = 1),
    textInput("activityNote", "Notes"),
    actionButton("save", "Save", class = "btn-primary"),
    
    textOutput("recordMessage")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$recordMessage <- renderText({
    paste0(c("You recorded: ", input$category))
  })
  
  observeEvent(input$save, {
    message("New entry saved!")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
