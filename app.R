library(shiny)
library(readr)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Habit Tracker"),
    
    uiOutput("category"),
    uiOutput("activity"),
    dateInput("dateTracked", "Date", format = "dd.mm.yyyy", weekstart = 1),
    textInput("activityNote", "Notes"),
    actionButton("save", "Save", class = "btn-primary"),
    
    textOutput("recordMessage")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  categoryOptions <- reactive({read_csv("data/category.csv")})
  activityOptions <- reactive({read_csv("data/activity.csv")})
  
  output$category <- renderUI({
    selectInput(
      "category", "Category",
      choices = c("Any", sort(categoryOptions()$category)),
      selected = "Any"
    )
  })
  output$activity <- renderUI({
    if ("Any" %in% input$category) {
      filteredOptions <- activityOptions()
    } else {
      filteredOptions <- activityOptions() %>% filter(category %in% input$category)
    }
    selectInput("activity", "Activity", filteredOptions$activity)
  })
  
  activity <- reactive(
    data.frame(input$category, input$activity, input$dateTracked, input$activityNote)
  )
  
  output$recordMessage <- renderText({
    paste0(c("You recorded: ", input$category))
  })
  
  observeEvent(input$save, {
    write_csv(
      activity(),
      "data/test_activities_tracked.csv",
      append = TRUE,
      eol = "\r\n"
    )
    message("New entry saved!")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
