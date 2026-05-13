library(shiny)
library(xgboost)

df <- read.csv("data/cs-training.csv")
model <- xgb.load("model_credit_scoring.model")

df$age[df$age < 18] <- median(df$age[df$age >= 18], na.rm = TRUE)
if ("X" %in% colnames(df)) {
  df$X <- NULL
}

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

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Wykresy",
             selectInput(inputId = "wybor_kolumny", label = "Wybierz kolumnę", choices = names(df)),
             plotOutput(outputId = "wykres")),
    tabPanel("Kalkulator", 
             fluidRow(
              column(6, 
              numericInput(inputId = "age", label = "Wiek", value = 30),
              numericInput(inputId = "MonthlyIncome", label = "Miesięczny dochód brutto", value = 5000),
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
    
    dmatrix_input <- xgb.DMatrix(data = as.matrix(nowe_dane))
    
    prob <- predict(model, dmatrix_input)
    
    paste0("Prawdopodobieństwo niewypłacalności: ", round(prob * 100, 2), "%")
  })
  
  output$wynik <- renderText({
    wynik_predykcji()
  })
  
  output$wykres <- renderPlot({
    req(input$wybor_kolumny)
    wektor <- df[[input$wybor_kolumny]]
    
    # Pobranie polskiej nazwy ze słownika
    polska_nazwa <- slownik_kolumn[[input$wybor_kolumny]]
    
    # Zawijanie długiego tekstu dla tytułu (łamanie wiersza co ~60 znaków)
    tytul_zawiniety <- paste(strwrap(polska_nazwa, width = 60), collapse = "\n")
    
    # Usunięcie braków danych
    wektor <- na.omit(wektor)
    
    # Logika warunkowa
    if (input$wybor_kolumny == "DebtRatio") {
      wektor_filtrowany <- wektor[wektor >= 0 & wektor <= 1.4]
      zakres_x <- c(0, 1.4)
    } else {
      limit_gorny <- quantile(wektor, 0.995)
      wektor_filtrowany <- wektor[wektor <= limit_gorny]
      zakres_x <- range(wektor_filtrowany)
    }
    
    # Rysowanie histogramu z nowymi tytułami
    hist(wektor_filtrowany, 
         main = tytul_zawiniety,
         xlab = polska_nazwa, 
         col = "skyblue", 
         border = "white",
         breaks = 50,
         xlim = zakres_x)
  })
}

shinyApp(ui, server)