# library(shiny)
# library(bslib)
# 
# # Define UI for app that draws a histogram ----
# ui <- page_sidebar(
#   # App title ----
#   title = "Hello Shiny!",
#   # Sidebar panel for inputs ----
#   sidebar = sidebar(
#     # Input: Slider for the number of bins ----
#     sliderInput(
#       inputId = "bins",
#       label = "Number of bins:",
#       min = 1,
#       max = 50,
#       value = 30
#     )
#   ),
#   # Output: Histogram ----
#   plotOutput(outputId = "distPlot")
# )
# 
# 
# # Define server logic required to draw a histogram ----
# server <- function(input, output) {
#   
#   # Histogram of the Old Faithful Geyser Data ----
#   # with requested number of bins
#   # This expression that generates a histogram is wrapped in a call
#   # to renderPlot to indicate that:
#   #
#   # 1. It is "reactive" and therefore should be automatically
#   #    re-executed when inputs (input$bins) change
#   # 2. Its output type is a plot
#   output$distPlot <- renderPlot({
#     
#     x    <- faithful$waiting
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#     hist(x, breaks = bins, col = "#007bc2", border = "white",
#          xlab = "Waiting time to next eruption (in mins)",
#          main = "Histogram of waiting times")
#     
#   })
#   
# }
# 
# shinyApp(ui = ui, server = server)



# --------------------------------------------------------------------------------------------------------------------------

# https://mastering-shiny.org/scaling-packaging.html


# app.R
library(shiny)


# R/monthFeedback.R
monthFeedbackUI <- function(id) {
  textOutput(NS(id, "feedback"))
}


# R/monthFeedback.R
monthFeedbackServer <- function(id, month) {
  
  stopifnot(is.reactive(month))
  
  moduleServer(
    id, 
    function(input, output, session) {
      output$feedback <- renderText( { if (month() == "October") {"You picked a great month!"} else {"Eh, you could do better."} } )
    }
  )
}


# app.R
stones <- read.csv("birthstones.csv")


# R/birthstone.R
birthstoneUI <- function(id) {
  p("The birthstone for ", textOutput(NS(id, "month"), inline = TRUE), " is ", textOutput(NS(id, "stone"), inline = TRUE))
}

# R/birthstone.R
birthstoneServer <- function(id, month) {
  
  stopifnot(is.reactive(month))
  
  moduleServer(
    id, 
    function(input, output, session) {
    stone <- reactive(stones$stone[stones$month == month()])
    output$month <- renderText(month())
    output$stone <- renderText(stone())
    }
  )
}


# app.R
months <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
  )


# app.R
ui <- navbarPage(
  "Sample app",
  tabPanel("Pick a month", selectInput("month", "What's your favourite month?", choices = months) ),
  tabPanel("Feedback"    , monthFeedbackUI( "tab1") ),
  tabPanel("Birthstone"  , birthstoneUI(    "tab2") )
  )


# app.R
server <- function(input, output, session) {
  monthFeedbackServer( "tab1", reactive(input$month) )
  birthstoneServer(    "tab2", reactive(input$month) )
}


# app.R
shinyApp(ui, server)

