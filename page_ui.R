library(shiny)

shinyUI(fluidPage(
  
  htmlOutput("page")
  
  
))


output$page <- renderUI({
  
  tabsetPanel(id = "inTabset",
              
              #Panel 
              tabPanel("Predykcja", 
                       br(),
                       fluidRow(column(3, 
                                       selectInput(inputId = "league_typed_prediction", label = "Wybierz Ligę", 
                                                    choices = c("Seria A", 
                                                                "Bundesliga", "Bundesliga 2", 
                                                                "Premier League","Championship",
                                                                "La Liga", "La Liga 2",
                                                                "France 1", "France 2", 
                                                                "Portugal 1","Netherlands 1"), 
                                                    selected = "Seria A"),
                                       radioButtons(inputId = "model_typed_prediction", label = "Wybierz model", 
                                                    choices = c("LogisticRegression", 
                                                                "DecisionTrees",
                                                                "RandomForest",
                                                                "SupportVectoMachine",
                                                                "kNearestNeighbors", 
                                                                "VotingClassifier"), 
                                                    selected = "VotingClassifier"),
                                       uiOutput("mwselection"),
                                       actionButton("prediction_butt", "Pokaż predykcję", width = '100%'
                                                    , icon("bar-chart-o"), 
                                                    style="color: #fff; background-color: olivedrab; border-color: black")
                                       
                       ),
                       column(7, 
                              DT::dataTableOutput("prediction_table"),
                              br(),
                              htmlOutput("info_pred"),
                              
                              br(),
                              uiOutput("pred1"),
                              br(),
                              uiOutput("pred2"),
                              br(),
                              uiOutput("pred3")
                              
                       ))
                              
                       
                       
                       )
                       
  )
              
})





#shinyApp(ui, server)
