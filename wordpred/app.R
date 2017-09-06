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
      h3(verbatimTextOutput(outputId = "distPrediction"))
    )
  )
)

server <- function(input, output) {
  output$distPrediction <- renderText({
    userInput <- trimws(input$userInput)
    userInput <- gsub('\\s+', ' ', userInput)
    pred.sentense <- userInput
    words <- strsplit(tolower(userInput), split = ' ')[[1]]
    if (length(words) >= 2) {
      key <- paste0(tail(words, 2), collapse = ' ')
      pred.char <- h[[key]]
      pred.char[pred.char == 'i'] <- 'I'
      if (length(pred.char)) {
        maxDisplay <- min(5, length(pred.char))
        for (i in 1:maxDisplay) {
          pred.sentense <- paste(pred.sentense,
                                 paste(userInput, pred.char[i], sep = ' '),
                                 sep = '\n')
        }
      }
      pred.sentense
    } else {
      userInput
    }
    })
  
}

shinyApp(ui = ui, server = server)