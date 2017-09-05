library(shiny)
library(hash)
load("hashtable.Rdata")

ui <- fluidPage(
  titlePanel("Word Prediction via Ngram"),
  sidebarLayout(
    sidebarPanel(
      textInput("userInput", "Please type here ...")
    ),
    mainPanel(
      textOutput(outputId = "distPrediction")
    )
  )
)

server <- function(input, output) {
  output$distPrediction <- renderText({
    words <- strsplit(tolower(input$userInput), split = ' ')[[1]]
    if (length(words) >= 2) {
      key <- paste0(tail(words, 2), collapse = ' ')
      pred.list <- h[[key]]
      paste(input$userInput, pred.list[[1]], sep = ' ')
      # paste("..dfasdf.this is userinput: ", input$userInput,
      #       "...this is the key: ", key,
      #       "...this is the value: ", value,
      #       "...this is the output: ", paste(input$userInput, value, sep = ' '))
    }})
}

shinyApp(ui = ui, server = server)