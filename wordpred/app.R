library(shiny)
library(hash)
load("hashtable.Rdata")

ui <- fluidPage(
  
  titlePanel("Word Prediction via Ngram"),
  
  hr(),
  
  fluidRow(
    column(5, textInput("userInput", "Please type here ... type at least 
                        two words to enable prediction.")),
    column(7, h3(verbatimTextOutput(outputId = "distPrediction")))
  ),
  
  hr(),
  
  fluidRow(
    column(8, h3("What is this?"),
              h4("This app predicts the next word you will most probably type, based on 
                 previous two words that you already typed. This app uses a tri-gram model
                 that is trained over ~500 MB text data from blogs, news articles and 
                 twitter. Get the source code ", 
                 tags$a(href="https://github.com/chunjiw/wordpred", "here!")))
  ),
  
  hr(),
  
  fluidRow(
    column(8, h3("Join my webinar!"),
           h4("As a member of LEAP-SIG-DS (Special Interest Group of Data Science), 
              I will host a webinar on 14th Sep, Thursday at 9pm to share my experience 
              making this app. Join us!"),
           h4(tags$a(href="https://zoom.us/j/935885916", "Click here at 9pm, 14th Sep.")))
  )

)

server <- function(input, output) {
  output$distPrediction <- renderText({
    userInput <- trimws(input$userInput)
    userInput <- gsub('\\s+', ' ', userInput)
    pred.sentense <- userInput
    words <- strsplit(tolower(userInput), split = ' ')[[1]]
    if (length(words) >= 2) {
      history <- paste0(tail(words, 2), collapse = ' ')
      candidates <- h[[history]]$candidate
      candidates[candidates == 'i'] <- 'I'
      if (length(candidates)) {
        maxDisplay <- min(5, length(candidates))
        for (i in 1:maxDisplay) {
          pred.sentense <- paste(pred.sentense,
                                 paste(userInput, candidates[i], sep = ' '),
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