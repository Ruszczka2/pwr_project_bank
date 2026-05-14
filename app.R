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
                      numericInput(inputId = "age", label = "Wiek", value = 30, min = 0, max = 109),
                      numericInput(inputId = "MonthlyIncome", label = "Miesięczny dochód brutto [PLN]", value = 5000, min = 0, max = 3008750),
                      numericInput(inputId = "DebtRatio", label = "Wskaźnik zadłużenia", value = 0.3, min = 0, max = 329664),
                      numericInput(inputId = "NumberOfDependents", label = "Liczba osób na utrzymaniu", value = 0, min = 0, max = 20),
                      numericInput(inputId = "RevolvingUtilizationOfUnsecuredLines", label = "Wykorzystanie limitu kredytowego", value = 0.5, min = 0, max = 50708),
                      numericInput(inputId = "NumberOfOpenCreditLinesAndLoans", label = "Liczba otwartych linii kredytowych", value = 5, min = 0, max = 58),
                      numericInput(inputId = "NumberRealEstateLoansOrLines", label = "Liczba kredytów hipotecznych", value = 0, min = 0, max = 54),
                      numericInput(inputId = "NumberOfTime30.59DaysPastDueNotWorse", label = "Opóźnienia 30-59 dni w ostatnich 2 latach", value = 0, min = 0, max = 98),
                      numericInput(inputId = "NumberOfTime60.89DaysPastDueNotWorse", label = "Opóźnienia 60-89 dni w ostatnich 2 latach", value = 0, min = 0, max = 98),
                      numericInput(inputId = "NumberOfTimes90DaysLate", label = "Opóźnienia 90+ dni w ostatnich 2 latach", value = 0, min = 0, max = 98),
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
    
    if (input$age < 0 || input$age > 109) {
      return("Nieprawidłowa wartość w kolumnie 'Wiek'. Prawidłowy zakres: 0-109.")
    }
    if (input$MonthlyIncome < 0 || input$MonthlyIncome > 3008750) {
      return("Nieprawidłowa wartość w kolumnie 'Miesięczny dochód'. Prawidłowy zakres: 0-3008750.")
    }
    if (input$DebtRatio < 0 || input$DebtRatio > 329664) {
      return("Nieprawidłowa wartość w kolumnie 'Wskaźnik zadłużenia'. Prawidłowy zakres: 0-329664.")
    }
    if (input$NumberOfDependents < 0 || input$NumberOfDependents > 20) {
      return("Nieprawidłowa wartość w kolumnie 'Liczba osób na utrzymaniu'. Prawidłowy zakres: 0-20.")
    }
    if (input$RevolvingUtilizationOfUnsecuredLines < 0 || input$RevolvingUtilizationOfUnsecuredLines > 50708) {
      return("Nieprawidłowa wartość w kolumnie 'Wykorzystanie limitu kredytowego'. Prawidłowy zakres: 0-50708.")
    }
    if (input$NumberOfOpenCreditLinesAndLoans < 0 || input$NumberOfOpenCreditLinesAndLoans > 58) {
      return("Nieprawidłowa wartość w kolumnie 'Liczba otwartych linii kredytowych'. Prawidłowy zakres: 0-58.")
    }
    if (input$NumberRealEstateLoansOrLines < 0 || input$NumberRealEstateLoansOrLines > 54) {
      return("Nieprawidłowa wartość w kolumnie 'Liczba kredytów hipotecznych'. Prawidłowy zakres: 0-54.")
    }
    if (input$NumberOfTime30.59DaysPastDueNotWorse < 0 || input$NumberOfTime30.59DaysPastDueNotWorse > 98) {
      return("Nieprawidłowa wartość w kolumnie 'Opóźnienia 30-59 dni'. Prawidłowy zakres: 0-98.")
    }
    if (input$NumberOfTime60.89DaysPastDueNotWorse < 0 || input$NumberOfTime60.89DaysPastDueNotWorse > 98) {
      return("Nieprawidłowa wartość w kolumnie 'Opóźnienia 60-89 dni'. Prawidłowy zakres: 0-98.")
    }
    if (input$NumberOfTimes90DaysLate < 0 || input$NumberOfTimes90DaysLate > 98) {
      return("Nieprawidłowa wartość w kolumnie 'Opóźnienia 90+ dni'. Prawidłowy zakres: 0-98.")
    }
    
    nowe_dane <- data.frame(
      RevolvingUtilizationOfUnsecuredLines = input$RevolvingUtilizationOfUnsecuredLines,
      age = input$age,
      NumberOfTime30.59DaysPastDueNotWorse = input[["NumberOfTime30.59DaysPastDueNotWorse"]],
      DebtRatio = input$DebtRatio,
      MonthlyIncome = input$MonthlyIncome,
      NumberOfOpenCreditLinesAndLoans = input$NumberOfOpenCreditLinesAndLoans,
      NumberOfTimes90DaysLate = input$NumberOfTimes90DaysLate,
      NumberRealEstateLoansOrLines = input$NumberRealEstateLoansOrLines,
      NumberOfTime60.89DaysPastDueNotWorse = input[["NumberOfTime60.89DaysPastDueNotWorse"]],
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