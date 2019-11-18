suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(scales))
#library(plyr)
suppressMessages(library(data.table))
suppressMessages(library(dplyr))

# library("RODBC")
# 
# con <- dbConnect(
#  odbc(),
#  Driver      = "FreeTDS",
#  Database    = "S72t1V5idn",
#  Uid         = "S72t1V5idn",
#  Pwd         = "dEewXLcrbR",
#  Server      = "remotemysql.com",
#  Trusted_Connection = "True",
#  TDS_Version=7.0
# )
#sort(unique(odbcListDrivers()[[1]]))

#library(DBI)


#require(DBI)


Predykcja_tabela <- function(dane = predictions , 
                             model = "LogisticRegression", 
                             league = "Seria A", mw ){
  
  #league <- ifelse(league=="Premier League", "premier", tolower(gsub(" ", "", league)))
  model_abb <- ifelse(model == "LogisticRegression", "LR", 
                      ifelse(model == "DecisionTrees", "DT", 
                      ifelse(model == "RandomForest", "RFs",
                      ifelse(model == "SupportVectoMachine", "SVC",
                      ifelse(model == "StochasticGradientDescent", "SGD",
                      ifelse(model == "kNearestNeighbors", "KNN","ECL"))))))
  
  league_abb <- ifelse(league == "Seria A", "I1", 
                ifelse(league == "Seria B", "I2", 
                ifelse(league == "Bundesliga", "D1",
                ifelse(league == "Bundesliga 2", "D2", 
                ifelse(league == "Premier League", "E0",
                ifelse(league == "Championship", "E1", 
                ifelse(league == "Portugal 1", "P1", 
                ifelse(league == "La Liga", "SP1", 
                ifelse(league == "La Liga 2", "SP2", 
                ifelse(league == "France 1", "F1", 
                ifelse(league == "France 2", "F2", 
                ifelse(league == "Netherlands 1", "N1","")))))))) ))))
      
  df <- dane[dane$league == league_abb & 
               dane$MW == mw & 
               dane$model == model_abb,
             c("HomeTeam",  "AwayTeam" , "proba", "prediction", "Result" ,"corrected" )]
  
  #df$pred_value <- round(as.numeric(df$pred_value), 3)
  df$prediction <- ifelse(df$prediction == 1, "1", "X2")
  
  names(df) <- c("Gospodarz", "Gość", "Wartość predykcji <0,1>", "Predykcja", "Wynik", "Poprawność")
  
  df$Poprawność <- ifelse(df$Poprawność == 1, "true", "false")
  df
}


