library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Wykresy"),
    tabPanel("Kalkulator"),
  )
)

server <- function(input, output) {
  #logika
}

shinyApp(ui, server)