suppressMessages(library(shiny))
#suppressMessages(library(ECharts2Shiny))
#suppressMessages(library(shinyWidgets))
#con <- dbConnect('driver=MySQL;server=remotemysql.com;database=S72t1V5idn;uid=S72t1V5idn;pwd=dEewXLcrbR;')
require(RMySQL) #if already installed
drv <- dbDriver("MySQL")
con <- RMySQL::dbConnect(RMySQL::MySQL(),
                         host = "remotemysql.com",
                         dbname="S72t1V5idn",
                         user = "S72t1V5idn",
                         password = "dEewXLcrbR")



# pool <- dbPool(
#   drv      = RMySQL::MySQL(),
#   dbname   = "S72t1V5idn",
#   host     = "remotemysql.com",
#   username = "S72t1V5idn", 
#   password = "dEewXLcrbR"
# )
# onStop(function() {
#   poolClose(pool)
# })
predictions <- dbGetQuery(con, "SELECT * FROM predictions")

df_razem <- predictions 


shinyServer(function(input, output) {
  
  source("page_ui.R", local = TRUE, encoding = "UTF-8")
  
  
  output$mwselection <- renderUI({
    selectInput("mwselection_values", "Wybierz kolejkę:", 
                choices = sort(unique(df_razem[df_razem$league == ifelse(input$league_typed_prediction == "Seria A", "I1", 
                                                                    ifelse(input$league_typed_prediction == "Seria B", "I2", 
                                                                           ifelse(input$league_typed_prediction == "Bundesliga", "D1",
                                                                                  ifelse(input$league_typed_prediction == "Bundesliga 2", "D2", 
                                                                                         ifelse(input$league_typed_prediction == "Premier League", "E0",
                                                                                                ifelse(input$league_typed_prediction == "Championship", "E1", 
                                                                                                       ifelse(input$league_typed_prediction == "Portugal 1", "P1", 
                                                                                                              ifelse(input$league_typed_prediction == "La Liga", "SP1", 
                                                                                                                     ifelse(input$league_typed_prediction == "La Liga 2", "SP2", 
                                                                                                                            ifelse(input$league_typed_prediction == "France 1", "F1", 
                                                                                                                                   ifelse(input$league_typed_prediction == "France 2", "F2", 
                                                                                                                                          ifelse(input$league_typed_prediction == "Netherlands 1", "N1","")))))))) )))), 
                                          "MW"], na.rm =T), decreasing = TRUE), 
                selected = max(df_razem[df_razem$league == ifelse(input$league_typed_prediction == "Seria A", "I1", 
                                                                  ifelse(input$league_typed_prediction == "Seria B", "I2", 
                                                                         ifelse(input$league_typed_prediction == "Bundesliga", "D1",
                                                                                ifelse(input$league_typed_prediction == "Bundesliga 2", "D2", 
                                                                                       ifelse(input$league_typed_prediction == "Premier League", "E0",
                                                                                              ifelse(input$league_typed_prediction == "Championship", "E1", 
                                                                                                     ifelse(input$league_typed_prediction == "Portugal 1", "P1", 
                                                                                                            ifelse(input$league_typed_prediction == "La Liga", "SP1", 
                                                                                                                   ifelse(input$league_typed_prediction == "La Liga 2", "SP2", 
                                                                                                                          ifelse(input$league_typed_prediction == "France 1", "F1", 
                                                                                                                                 ifelse(input$league_typed_prediction == "France 2", "F2", 
                                                                                                                                        ifelse(input$league_typed_prediction == "Netherlands 1", "N1","")))))))) )))), 
                                                                                            "MW"], na.rm =T) ,
                multiple = FALSE)
  })
  observeEvent(input$prediction_butt, {
  
  output$prediction_table <- DT::renderDataTable({ DT::datatable(Predykcja_tabela(dane = df_razem,
                                                                                model = input$model_typed_prediction,
                                                                                league =  input$league_typed_prediction,
                                                                                mw = input$mwselection_values)
                                                                ,options = list(
                                                                 autoWidth = TRUE, searching = FALSE, columnDefs = list(list(width = '80px'))
                                                                 ,scrollX = FALSE,  dom = "t", lengthChange = FALSE, 
                                                                 ordering = FALSE), rownames = FALSE) %>%  DT::formatStyle('Poprawność',
                      backgroundColor = DT::styleEqual(c("false", "true"), c('red', 'green')))
    })
  
  output$info_pred <- renderText({ 
    HTML(paste("1 - Wygrana gospodarza", " X2 - remis lub zwycięstwo gości", sep="<br/>"))
  })
  url1 <- a("Model regresji logistycznej", href="https://analizadanychwpilce.wordpress.com/2018/09/15/przewidywanie-wyniku-spotkania-z-wykorzystaniem-regresji-logistycznej")
  output$pred1 <- renderUI({
    tagList("Logistic Regression:", url1)
  })
  url2 <- a("Model drzewa decyzyjnego", href="https://analizadanychwpilce.wordpress.com/2018/10/15/wykorzystanie-modelu-drzewa-decyzyjnego-do-analizy-wynikow-spotkania")
  output$pred2 <- renderUI({
    tagList("Decission Tree:", url2)
  })
  url3 <- a("Model lasów losowych", href="https://analizadanychwpilce.wordpress.com/2018/11/18/model-lasow-losowych")
  output$pred3 <- renderUI({
    tagList("Random Forest:", url3)
    
  })
  })
  
 
  
})


