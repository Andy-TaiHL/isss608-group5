library(shiny)

ui <- fluidPage(
  titlePanel("VAST Challenge 2026 - Group 5"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "Pick a number:", 1, 100, 50)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    paste("You picked:", input$num)
  })
}

shinyApp(ui, server)