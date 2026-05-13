library(shiny)
library(xgboost)

df <- readRDS("data/cs-cleaned.rds")
model <- xgb.load("model_credit_scoring.model")

# Wczytanie parametrów skalowania wygenerowanych podczas treningu
skalowanie <- readRDS("scaling_params.rds")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Wykresy",
             selectInput(inputId = "wybor_kolumny", label = "Wybierz kolumnę", choices = names(df)),
             plotOutput(outputId = "wykres")),
    tabPanel("Kalkulator", 
             fluidRow(
               column(6, 
                      numericInput(inputId = "age", label = "Wiek", value = 30),
                      numericInput(inputId = "MonthlyIncome", label = "Miesięczny dochód brutto [PLN]", value = 5000),
                      numericInput(inputId = "DebtRatio", label = "Wskaźnik zadłużenia", value = 0.3),
                      numericInput(inputId = "NumberOfDependents", label = "Liczba osób na utrzymaniu", value = 0),
                      numericInput(inputId = "RevolvingUtilizationOfUnsecuredLines", label = "Wykorzystanie limitu kredytowego", value = 0.5),
                      numericInput(inputId = "NumberOfOpenCreditLinesAndLoans", label = "Liczba otwartych linii kredytowych", value = 5),
                      numericInput(inputId = "NumberRealEstateLoansOrLines", label = "Liczba kredytów hipotecznych", value = 0),
                      numericInput(inputId = "NumberOfTime30-59DaysPastDueNotWorse", label = "Opóźnienia 30-59 dni w ostatnich 2 latach", value = 0),
                      numericInput(inputId = "NumberOfTime60-89DaysPastDueNotWorse", label = "Opóźnienia 60-89 dni w ostatnich 2 latach", value = 0),
                      numericInput(inputId = "NumberOfTimes90DaysLate", label = "Opóźnienia 90+ dni w ostatnich 2 latach", value = 0),
                      actionButton(inputId = "predic_button", label = "Oblicz ryzyko")),
               column(6, textOutput(outputId = "wynik"))
             )
    ))
)

slownik_kolumn <- c(
  "SeriousDlqin2yrs" = "Osoba, u której wystąpiło opóźnienie w płatności o 90 dni lub więcej",
  "RevolvingUtilizationOfUnsecuredLines" = "Wykorzystanie limitu kredytowego",
  "age" = "Wiek",
  "NumberOfTime30.59DaysPastDueNotWorse" = "Opóźnienia 30-59 dni w ostatnich 2 latach",
  "DebtRatio" = "Wskaźnik zadłużenia",
  "MonthlyIncome" = "Miesięczny dochód brutto",
  "NumberOfOpenCreditLinesAndLoans" = "Liczba otwartych linii kredytowych",
  "NumberOfTimes90DaysLate" = "Opóźnienia 90+ dni w ostatnich 2 latach",
  "NumberRealEstateLoansOrLines" = "Liczba kredytów hipotecznych",
  "NumberOfTime60.89DaysPastDueNotWorse" = "Opóźnienia 60-89 dni w ostatnich 2 latach",
  "NumberOfDependents" = "Liczba osób na utrzymaniu"
)

server <- function(input, output) {
  
  wynik_predykcji <- eventReactive(input$predic_button, {
    
    nowe_dane <- data.frame(
      RevolvingUtilizationOfUnsecuredLines = input$RevolvingUtilizationOfUnsecuredLines,
      age = input$age,
      NumberOfTime30.59DaysPastDueNotWorse = input[["NumberOfTime30-59DaysPastDueNotWorse"]],
      DebtRatio = input$DebtRatio,
      MonthlyIncome = input$MonthlyIncome,
      NumberOfOpenCreditLinesAndLoans = input$NumberOfOpenCreditLinesAndLoans,
      NumberOfTimes90DaysLate = input$NumberOfTimes90DaysLate,
      NumberRealEstateLoansOrLines = input$NumberRealEstateLoansOrLines,
      NumberOfTime60.89DaysPastDueNotWorse = input[["NumberOfTime60-89DaysPastDueNotWorse"]],
      NumberOfDependents = input$NumberOfDependents
    )
    
    # Zabezpieczenie poprawnej kolejności kolumn zgodnie ze zbiorem treningowym
    nowe_dane <- nowe_dane[, names(skalowanie$center), drop = FALSE]
    
    # Standaryzacja nowych danych wejściowych
    nowe_dane_scaled <- scale(nowe_dane, center = skalowanie$center, scale = skalowanie$scale)
    
    dmatrix_input <- xgb.DMatrix(data = as.matrix(nowe_dane_scaled))
    
    prob <- predict(model, dmatrix_input)
    
    paste0("Prawdopodobieństwo niewypłacalności: ", round(prob * 100, 2), "%")
  })
  
  output$wynik <- renderText({
    wynik_predykcji()
  })
  
  output$wykres <- renderPlot({
    req(input$wybor_kolumny)
    
    wektor <- na.omit(df[[input$wybor_kolumny]])
    if (length(wektor) == 0) return(NULL)
    
    polska_nazwa <- slownik_kolumn[[input$wybor_kolumny]]
    tytul_zawiniety <- paste(strwrap(polska_nazwa, width = 60), collapse = "\n")
    
    if (input$wybor_kolumny == "DebtRatio") {
      wektor_do_wykresu <- wektor[wektor >= 0 & wektor <= 1.4]
      
      hist(wektor_do_wykresu, 
           main = tytul_zawiniety,
           xlab = polska_nazwa, 
           col = "skyblue", 
           border = "white",
           breaks = 55,
           xlim = c(0, 1.4))
    } else {
      hist(wektor, 
           main = tytul_zawiniety,
           xlab = polska_nazwa, 
           col = "skyblue", 
           border = "white",
           breaks = 50)
    }
  })
}

shinyApp(ui, server)