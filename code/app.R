library(shiny)

# Define UI
ui <- fluidPage(
  
  # Add a title to the app
  titlePanel("Math Quiz"),
  
  # Add a sidebar with instructions
  sidebarLayout(
    sidebarPanel(
      h4("Instructions:"),
      p("Enter your answer to each problem in the text box provided. Click the 'Submit' button to check your answer. You will complete 10 questions. Your score will be displayed below the button.")
    ),
    
    # Add the main content area
    mainPanel(
      h2("Math Quiz"),
      
      # Display a random math problem
      h3(textOutput("problem")),
      
      # Allow user to enter an answer
      textInput("answer", "Enter your answer:"),
      
      # Submit button to check answer
      actionButton("submit", "Submit"),
      
      # Display user's score
      h4(textOutput("score")),
      
      # Display score card at end of quiz
      verbatimTextOutput("score_card", placeholder = FALSE)
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Generate random math problem
  generate_problem <- function() {
    repeat {
      x <- sample(0:20, 1)
      y <- sample(0:20, 1)
      operator <- sample(c("+", "-"), 1)
      if (operator == "+") {
        answer <- x + y
      } else {
        answer <- x - y
      }
      if (answer >= 0) {
        break
      }
    }
    problem <- paste(x, operator, y, "=", "", sep = " ")
    return(list(problem = problem, answer = answer))
  }
  
  # Display random math problem
  problem_data <- reactiveVal(generate_problem())
  output$problem <- renderText({
    problem_data()$problem
  })
  
  # Check user's answer and update score
  score <- reactiveValues(value = 0)
  responses <- reactiveValues(data = data.frame())
  
  observeEvent(input$submit, {
    response <- input$answer
    if (as.numeric(response) == problem_data()$answer) {
      score$value <- score$value + 1
      response_status <- "Correct"
    } else {
      response_status <- "Incorrect"
    }
    new_response <- data.frame(Problem = problem_data()$problem,
                               Response = response,
                               Answer = problem_data()$answer,
                               Status = response_status)
    responses$data <- rbind(responses$data, new_response)
    if (nrow(responses$data) < 10) {
      problem_data(generate_problem())
      updateTextInput(session, "answer", value = "")
    } else {
      showModal(modalDialog(
        title = "Quiz complete!",
        paste("Your score is", score$value, "out of 10."),
        easyClose = TRUE
      ))
    }
  })
  
  # Display user's score
  output$score <- renderText({
    paste("Score:", score$value)
  })
  
  # Display score card at end of quiz
  output$score_card <- renderPrint({
    if (nrow(responses$data) == 10) {
      responses$data
    }
  })
  
}

# Run the app
shinyApp(ui, server)
