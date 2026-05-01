library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Wykresy"),
    tabPanel("Kalkulator", 
             fluidRow(
              column(6, 
              numericInput(inputId = "age", label = "Wiek", value = 30)),
              column(6, )
             )
    ))
)

server <- function(input, output) {
  #logika
}

shinyApp(ui, server)