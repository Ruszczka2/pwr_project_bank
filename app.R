library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Wykresy"),
    tabPanel("Kalkulator", 
             fluidRow(
              column(6, 
              numericInput(inputId = "age", label = "Wiek", value = 30),
              numericInput(inputId = "MonthlyIncome", label = "Miesięczny dochów", value = 5000),
              numericInput(inputId = "DebtRatio", label = "Wskaźnik zadłużenia", value = 0.3),
              numericInput(inputId = "NumberOfDependents", label = "Liczba osób na utrzymaniu", value = 0),
              numericInput(inputId = "RevolvingUtilizationOfUnsecuredLines", label = "Wykorzystanie limitu kredytowego", value = 0.5),
              numericInput(inputId = "NumberOfOpenCreditLinesAndLoans", label = "Liczba otwartych linii kredytowyc", value = 5),
              numericInput(inputId = "NumberRealEstateLoansOrLines", label = "Liczba kredytów hipotecznych", value = 0),
              numericInput(inputId = "NumberOfTime30-59DaysPastDueNotWorse", label = "Opóźnienia 30-59 dni", value = 0),
              numericInput(inputId = "NumberOfTime60-89DaysPastDueNotWorse", label = "Opóźnienia 60-89 dni", value = 0),
              numericInput(inputId = "NumberOfTimes90DaysLate", label = "Opóźnienia 90+ dni", value = 0),
              actionButton(inputId = "predic-button", label = "Oblicz ryzyko")),
              column(6, )
             )
    ))
)

server <- function(input, output) {
  #logika
}

shinyApp(ui, server)