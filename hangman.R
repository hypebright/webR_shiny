library(shiny)

# Define a list of words for the game
words <- c("hangman", "computer", "programming", "openai", "shiny", "game")

ui <- fluidPage(
  titlePanel("Hangman Game"),
  
  mainPanel(
    h3("Guess the word!"),
    verbatimTextOutput("word_display"),
    textInput("guess_input", "Enter a letter:"),
    actionButton("guess_button", "Guess"),
    br(),
    h4("Incorrect Guesses:"),
    verbatimTextOutput("incorrect_guesses"),
    h4("Remaining Chances:"),
    verbatimTextOutput("remaining_chances")
  )
)

server <- function(input, output) {
  # Initialize game state
  game_state <- reactiveValues(
    word = sample(words, 1),  # Randomly select a word from the list
    guessed_letters = character(0),  # Store guessed letters
    incorrect_guesses = 0,  # Count of incorrect guesses
    remaining_chances = 7  # Total chances before game over
  )
  
  # Function to update game state based on user guess
  updateGameState <- function() {
    guess <- tolower(substr(input$guess_input, 1, 1))  # Extract first character of user's guess
    
    if (guess %in% game_state$guessed_letters) {
      # Letter has already been guessed, do nothing
      return()
    }
    
    game_state$guessed_letters <- c(game_state$guessed_letters, guess)
    
    if (!(guess %in% strsplit(game_state$word, "")[[1]])) {
      # Incorrect guess
      game_state$incorrect_guesses <- game_state$incorrect_guesses + 1
    }
    
    if (game_state$incorrect_guesses >= game_state$remaining_chances) {
      # Game over
      showGameOverMessage()
    }
  }
  
  # Action when the guess button is clicked
  observeEvent(input$guess_button, {
    updateGameState()
  })
  
  # Function to display the word with guessed letters filled in
  output$word_display <- renderPrint({
    word <- game_state$word
    guessed_letters <- game_state$guessed_letters
    
    displayed_word <- sapply(strsplit(word, "")[[1]], function(x) {
      if (x %in% guessed_letters) {
        x
      } else {
        "_"
      }
    })
    
    cat(paste(displayed_word, collapse = " "))
  })
  
  # Display incorrect guesses
  output$incorrect_guesses <- renderPrint({
    cat(paste(game_state$guessed_letters[!(game_state$guessed_letters %in% strsplit(game_state$word, "")[[1]])], collapse = ", "))
  })
  
  # Display remaining chances
  output$remaining_chances <- renderPrint({
    game_state$remaining_chances - game_state$incorrect_guesses
  })
  
  # Function to display game over message
  showGameOverMessage <- function() {
    showModal(modalDialog(
      title = "Game Over",
      paste("You ran out of chances! The word was", game_state$word),
      easyClose = TRUE
    ))
    
    # Reset game state
    game_state$word <- sample(words, 1)
    game_state$guessed_letters <- character(0)
    game_state$incorrect_guesses <- 0
  }
}

shinyApp(ui, server)
