library(shiny)
library(shinyjs)
library(readr)
library(tidyverse)
library(plyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),

    # Application title
    titlePanel("Habit Tracker"),
    
    uiOutput("category"),
    uiOutput("activity"),
    dateInput("dateTracked", "Date", format = "dd.mm.yyyy", weekstart = 1),
    textInput("activityNote", "Notes"),
    actionButton("save", "Save", class = "btn-primary"),
    actionButton("addActivity", "New", class = "btn-info"),
    
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
    selectizeInput(
      "activity", "Activity",
      choices = filteredOptions$activity,
      options = list(
        placeholder = "x"
      )
    )
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
  
# Adding new Activities (and Categories)
  observeEvent(input$addActivity, {
    showModal(modalDialog(
      textInput("newActivity", "Activity"),
      selectizeInput(
        "associatedCategory", "Category",
        choices = c("Choose or add a category" = "", sort(categoryOptions()$category)),
        options = list(
          create = TRUE
        )
      ),
      actionButton("saveNewActivity", "Save", class = "btn-primary"),
      title = "Add a new Activity",
      easyClose = TRUE,
      footer = observeEvent(input$newActivity, {
        output$newActivityError <- renderText({errorMessage})
      }),
      observe({
        if (input$newActivity == "" | input$associatedCategory == "") {
          inputState <- "incomplete"
        } else if (
          match_df(activityOptions(), data.frame(input$associatedCategory, input$newActivity))
        ) {
          inputState <- "duplicate"
        } else {
          inputState <- "satis"
        }
        toggleState("saveNewActivity", inputState == "satis")
        errorMessage <- ifelse(
          inputState == "incomplete",
          "Please complete all fields",
          ifelse(
            inputState == "duplicate",
            "This activity already exists",
            ""
          )
        )
      })
    ))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
