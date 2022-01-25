library(shinyjs)
library(shinyBS)
library(readxl)
library(fresh)
library(DT)
library(shinythemes)
library(rsconnect)
library(bs4Dash)
library(shinycssloaders)
library(naniar)
source("Mon_R.R")

shinyApp(
  ui = dashboardPage(skin = c("blue", "black",
                              "purple", "green", "red", "yellow"),
                     freshTheme = create_theme(
                       bs4dash_vars(
                         navbar_light_color = "#bec5cb",
                         navbar_light_active_color = "#FFF",
                         navbar_light_hover_color = "#FFF"
                       ),
                       bs4dash_yiq(
                         contrasted_threshold = 10,
                         text_dark = "#FFF",
                         text_light = "#272c30"
                       ),
                       bs4dash_layout(
                         main_bg = "#FFFFFF"
                       ),
                       bs4dash_sidebar_light(
                         bg = "#272c30",
                         color = "#bec5cb",
                         hover_color = "#FFF",
                         submenu_bg = "#272c30",
                         submenu_color = "#FFF",
                         submenu_hover_color = "#FFF"
                       ),
                       bs4dash_status(
                         primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
                       ),
                       bs4dash_color(
                         gray_900 = "#FFF", white ="#272c30"
                       )
                     ),
                     options = NULL,
                     
                     
                     
                     header = dashboardHeader(
                       title = dashboardBrand(
                         title = "Application",
                         color = "white",
                         href = "https://www.master-esa.fr",
                         image = "https://dev.meilleurs-masters.com/logo_ecole/logo-condense-couleur-masteresa-rvbplan-de-travail-1-1614592325.jpg",
                         opacity=1
                       )
                     ),
                     
                     
                     
                     
                     
                     sidebar = dashboardSidebar(
                       skin = "light",
                       inputId = "sidebarState",
                       sidebarMenu(
                         id = "sidebar",
                         menuItem(text = "Introduction",
                                  tabName = "Introduction",
                                  icon = icon("compass")),
                         menuItem(
                           text = "Statistical Analysis",
                           startExpanded = FALSE,
                           menuSubItem(
                             text = "Global",
                             tabName = "Global",
                             icon = icon("connectdevelop")
                           ),
                           menuSubItem(
                             text = "Variable",
                             tabName = "Variable",
                             icon = icon("vial")
                           ),
                           menuSubItem(
                             text = "Feature Engineering",
                             tabName = "Feature_Engineering",
                             icon = icon("brain")
                           ),
                           icon = icon("database")
                         ),
                         menuItem(text = "Logistic Regression",
                                  tabName = "logreg",
                                  icon = icon("chart-line")),
                         menuItem(
                           text = "Penalized Regressions",
                           icon = icon("bars"),
                           startExpanded = FALSE,
                           menuSubItem(
                             text = "Introduction",
                             tabName = "Penalization"
                           ),
                           menuSubItem(
                             text = "Ridge",
                             tabName = "Ridge",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "Lasso",
                             tabName = "Lasso",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "Adaptive Lasso",
                             tabName = "AdaLasso",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "Elastic Net",
                             tabName = "ElasticNet",
                             icon = icon("chart-bar")
                           )
                         ),
                         menuItem(
                           text = "Trees & Aggregation",
                           icon = icon("tree"),
                           startExpanded = FALSE,
                           menuSubItem(
                             text = "Introduction",
                             tabName = "Aggregation"
                           ),
                           menuSubItem(
                             text = "CART",
                             tabName = "CART",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "Bagging",
                             tabName = "Bagging",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "Random Forest",
                             tabName = "Random",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "AdaBoost",
                             tabName = "AdaBoost",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "Gradient Boosting",
                             tabName = "Gradient",
                             icon = icon("chart-bar")
                           )
                         ),
                         menuItem(
                           text = "Alternative Methods",
                           icon = icon("stream"),
                           startExpanded = FALSE,
                           menuSubItem(
                             text = "Introduction",
                             tabName = "Alternative"
                           ),
                           menuSubItem(
                             text = "XGBOOST",
                             tabName = "XGBOOST",
                             icon = icon("chart-bar")
                           ),
                           menuSubItem(
                             text = "Stochatic Boosting",
                             tabName = "Stochastic",
                             icon = icon("chart-bar")
                           )
                         ), 
                         menuItem(
                           text = "Conclusion",
                           tabName = "Conclusion",
                           icon = icon("lightbulb")
                         ),
                         menuItem(
                           text = "Propos",
                           tabName = "Propos",
                           icon = icon("home")
                         )
                       )
                     ),
                     
                     
                     
                     
                     
                     
                     
                     body = dashboardBody(
                       
                       tabItems(
                         tabItem(
                           tabName = "Stochastic",
                           
                           fluidRow(
                             
                             box( title = "Stochastic Boosting Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "650px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  sliderInput("cross_SB", "Number of boosting iterations to perform :", 20, 100, 25),
                                  sliderInput("cross_SB_1", "Shrinkage parameter for boosting :", 1, 10, 1),
                                  sliderInput("cross_SB_2", "Sampling fraction for samples taken out-of-bag", 1, 10, 1),
                                  em("Note : Shrinkage & Sampling are in tenths")),
                             #Kir
                             tabBox(width = 9, height = "650px",
                                    tabPanel("Summary of the Variables",withSpinner(verbatimTextOutput("SB"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("SB_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("SB_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("SB_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("SB_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("SB_3"))),
                                    tabPanel("Variables Importance", withSpinner(plotOutput("SB_der"))))
                           )), 
                         
                         
                         
                         
                         
                         tabItem(
                           tabName = "XGBOOST",
                           
                           fluidRow(  
                             
                             box( title = "XGBOOST Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "825px",
                                  width = 5,
                                  
                                  
                                  sliderInput("eta", "Choose the Learning Rate :", 0, 1, step = 0.1, value = 0.1),
                                  numericInput("max_depth", "Deepness of the tree :", min=1, max = 20, step = 1, value = 10),
                                  numericInput("min_child_weight", "Minimum sum of instance weight needed in a child :", min=1, max = 200, step = 5, value = 100),
                                  numericInput("colsample_bytree", "Subsample ratio of columns when constructing each tree :", min=0.1, max = 1, step = 0.1, value = 0.7),
                                  selectInput("eval_metric", "Metric evaluation :", choices=c("auc", "aucpr"), selected = "aucpr"),
                                  numericInput("scale_pos_weight", "Scaling :", min=1, max = 1000, step = 1, value = 577),
                                  sliderInput("nrounds", "Number of boosting iterations :", 1, 1000, step = 10, value = 500),
                                  sliderInput("early_stopping_rounds", "Early stopping rounds :", 10, 100, step = 10, value = 40)),
                             
                             
                             tabBox(width = 7,
                                    tabPanel("Summary of the Variables",withSpinner(verbatimTextOutput("XG"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("XG_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("XG_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("XG_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("XG_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("XG_3"))),
                                    tabPanel("Variables Importance", withSpinner(plotOutput("XG_der")))   ) )
                         ),
                         
                         
                         
                         tabItem(
                           tabName = "Gradient",
                           
                           fluidRow(
                             
                             box( title = "Gradient Boosting Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "450px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  sliderInput("cross_Gb", "Number of Trees :", 100, 1000, 100),
                                  sliderInput("cross_Gb_1", "Shrinkage Optimization :", 1, 10, 1),
                                  sliderInput("cross_Gb_2", "Number of Folds :", 3, 10, 3),
                                  em("Note : Shrinkage is in thousandths")),
                             
                             tabBox(width = 9,
                                    tabPanel("Summary of the Variables",withSpinner(verbatimTextOutput("Gb"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("Gb_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("Gb_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("Gb_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("Gb_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("Gb_3"))),
                                    tabPanel("Variables Importance", withSpinner(plotOutput("Gb_der"))))
                           )), 
                         
                         
                         
                         tabItem(
                           tabName = "AdaBoost",
                           
                           fluidRow(  
                             
                             box( title = "ADABOOST Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "500px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  selectInput("Optima",
                                              "Choose the type of Value: ",
                                              choices=c(Manual = "Manual", Cross_Validation = "Automatic"),
                                              selected = "Manual"),
                                  conditionalPanel(
                                    condition = "input.Optima == 'Manual'" ,
                                    sliderInput("cross_AdaBoost", "Number of Trees / Iterations for the Boosting :", 20, 100, 20),
                                  sliderInput("cross_AdaBoost_1", "Depth of the Tree :", 2, 10, 3)),
                                  p("Please Wait ...", style = "color:grey"),
                             conditionalPanel(
                               condition = "input.Optima == 'Automatic'" ,
                               sliderInput("cross_AdaBoost_2", "Number of Folds :", 3, 10, 5))),
                             
                             tabBox(width = 9,
                                    tabPanel("Summary of the Variables",withSpinner(verbatimTextOutput("AdaBoost"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("AdaBoost_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("AdaBoost_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("AdaBoost_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("AdaBoost_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("AdaBoost_3"))),
                                    tabPanel("Variables Importance", withSpinner(plotOutput("AdaBoost_der"))))
                           )),
                         
                         
                         tabItem(
                           tabName = "Random",
                           
                           fluidRow(  
                             
                             box( title = "RANDOM FOREST Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "500px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  selectInput("Optimus",
                                              "Choose the type of Value: ",
                                              choices=c(Manual = "Manual", Cross_Validation = "Automatic"),
                                              selected = "Manual"),
                                  conditionalPanel(
                                    condition = "input.Optimus == 'Manual'" ,
                                    sliderInput("cross_Random", "Number of variables randomly sampled at each split :", 3, 7, 3),
                                    sliderInput("cross_Random_1", "Number of trees to grow :", 100, 1500, 250)),
                                  p("Please Wait ...", style = "color:grey")),
                             
                             tabBox(width = 9,
                                    tabPanel("Summary of the Variables",withSpinner(verbatimTextOutput("Random"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("Random_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("Random_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("Random_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("Random_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("Random_3"))),
                                    tabPanel("Variables Importance", withSpinner(plotOutput("Random_der"))))
                           )),
                         
                         
                         tabItem(
                           tabName = "Bagging",
                           
                           fluidRow(
                             
                             box( title = "Bagging Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "200px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  sliderInput("cross_Bagging", "Choose number of bootstrap replications :", 10, 30, 10),
                                  p("Please Wait ...", style = "color:grey")),
                             
                             tabBox(width = 9,
                                    tabPanel("Summary of the Variables",withSpinner(verbatimTextOutput("Bagging"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("Bagging_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("Bagging_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("Bagging_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("Bagging_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("Bagging_3"))),
                                    tabPanel("Variables Importance", withSpinner(plotOutput("Bagging_der"))))
                           )),

                         
                         tabItem(
                           tabName = "CART",
                           
                           fluidRow(  
                             tabBox(width = 12,
                                    tabPanel("CART Plot",withSpinner(plotOutput("Cart"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("Cart_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("Cart_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("Cart_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("Cart_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("Cart_3"))),
                                    tabPanel("Variables Importance", withSpinner(plotOutput("Cart_der"))))
                             
                           )),
                         
                         tabItem(
                           tabName = "ElasticNet",
                           
                           fluidRow(  
                             
                             box( title = "ELASTIC NET Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "450px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  selectInput("Optimal",
                                              "Choose the type of Value: ",
                                              choices=c(Manual = "Manual", Cross_Validation = "Automatic"),
                                              selected = "Manual"),
                                  conditionalPanel(
                                    condition = "input.Optimal == 'Manual'" ,
                                    sliderInput("cross_ElasticNet_Alpha", "Choose your Alpha :", 0, 1, 1),
                                    sliderInput("cross_ElasticNet_Lambda", "Choose your Lambda :", 0, 1000, 10)),
                                  em("Note : Lambda is in thousandths")),
                             
                             tabBox(width = 9,
                                    tabPanel("CV Optimization",withSpinner(verbatimTextOutput("ElasticNet"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("ElasticNet_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("ElasticNet_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("ElasticNet_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("ElasticNet_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("ElasticNet_3"))))
                           )),
                         
                         tabItem(
                           tabName = "AdaLasso",
                           
                           fluidRow(  
                             
                             box( title = "ADAPTIVE LASSO Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "200px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  sliderInput("cross_AdaLasso", "Number of k-fold:", 3, 10, 3),
                                  p("Please Wait ...", style = "color:grey")),
                             
                             tabBox(width = 9,
                                    tabPanel("CV Optimization",withSpinner(verbatimTextOutput("AdaLasso"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("AdaLasso_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("AdaLasso_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("AdaLasso_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("AdaLasso_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("AdaLasso_3"))))
                           )),
                         
                         
                         
                         
                         
                         
                         tabItem(
                           tabName = "Lasso",
                           
                           fluidRow(  
                             
                             box( title = "LASSO Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "200px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  sliderInput("cross_Lasso", "Number of k-fold:", 3, 10, 3),
                                  p("Please Wait ...", style = "color:grey")),
                             
                             tabBox(width = 9,
                                    tabPanel("CV Optimization",withSpinner(verbatimTextOutput("Lasso"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("Lasso_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("Lasso_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("Lasso_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("Lasso_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("Lasso_3"))))
                           )),
                         
                         
                         tabItem(
                           tabName = "Ridge",
                           
                           fluidRow(  
                             
                             box( title = "RIDGE Controler",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  height = "200px",
                                  width = 3,
                                  # Only show this panel if the method is Ridge
                                  sliderInput("cross_Ridge", "Number of k-fold:", 3, 10, 3),
                                  p("Please Wait ...", style = "color:grey")),
                             
                             tabBox(width = 9,
                                    tabPanel("CV Optimization",withSpinner(verbatimTextOutput("Ridge"))),
                                    tabPanel("Confusion Matrix", withSpinner(plotOutput("Ridge_1"))),
                                    tabPanel("ROC", withSpinner(plotOutput("Ridge_2")),
                                             p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("Ridge_AUC"),
                                             p("The PR is equal to :", style = "color:red"),verbatimTextOutput("Ridge_PR")),
                                    tabPanel("Summary Plot", withSpinner(plotOutput("Ridge_3"))))
                           )),
                         
                         
                         
                         tabItem(
                           tabName = "Feature_Engineering",
                           fluidRow(
                             
                             # CONTROLS
                             box(
                               
                               title = "Statistics for New-Variables",
                               status = "primary",
                               solidHeader = TRUE,
                               
                               # Choose a column
                               selectInput(
                                 "columnChoice2",
                                 "Choose a column:",
                                 choices = colnames(new_variables),
                                 selected = "Age2")
                               
                             ),
                             # PLOT THE THTINGS
                             box( status = "warning",
                                  solidHeader = TRUE,
                                  withSpinner(plotOutput("histPlot2")) ),
                             
                             box(
                               title = "Data Explanatory",
                               status = "primary",
                               solidHeader = TRUE,
                               width = 12,
                               withSpinner(DT::dataTableOutput("mytablous")))
                             
                             
                             
                             
                           )),
                         
                         
                         tabItem(
                           tabName = "Global",
                           
                           box(
                             title = "Data Explanatory",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             withSpinner(DT::dataTableOutput("myTable")),
                             sidebar = boxSidebar(
                               id = "mycardsidebar",
                               width = 25,
                               checkboxInput(
                                 inputId = "N",
                                 label = "Valeurs Manquantes", 
                                 value = TRUE
                               ),
                               checkboxInput(
                                 inputId = "Ntm",
                                 label = "Valeurs Aberrantes", 
                                 value = TRUE
                               ),
                               checkboxInput(
                                 inputId = "Nop",
                                 label = "Re-echantillonnage", 
                                 value = TRUE
                               )
                             )
                           )),
                         
                         tabItem(
                           tabName = "Variable",
                           fluidRow(
                             
                             # CONTROLS
                             box(
                               
                               title = "Statistics per Variable",
                               status = "primary",
                               solidHeader = TRUE,
                               
                               # Choose a column
                               selectInput(
                                 "columnChoice",
                                 "Choose a column:",
                                 choices = colnames(data)[3:12],
                                 selected = "Age"),
                               
                               sliderInput("bins", "Number of breaks:", 1, 20, 2)
                               
                               
                             ),
                             box( status = "warning",
                                  solidHeader = TRUE,
                                  withSpinner(plotOutput("histPlot")) )
                           ),
                           
                           fluidRow(
                             box(title = "Some descriptive statistics: ",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 height = "150px",
                                 selectInput(
                                   "var",
                                   "Choose a variable:",
                                   choices = colnames(data)[2:12],
                                   selected = "Age")),
                             
                             box(title = "Summary of the variable: ",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 height = "150px",
                                 verbatimTextOutput("var")),
                             box(
                               title = "Correlation Plot",
                               status = "primary",
                               solidHeader = TRUE,
                               width = 8,
                               withSpinner(plotOutput("cor"))
                             )
                             
                           )
                           
                           
                         ),
                         
                         tabItem(tabName = "Propos",
                                 h1("Notre Equipe :", style = "color : #FF;text-align:center"),
                                 fluidRow(userBox(
                                   title = userDescription(
                                     title =tags$a(href='https://www.linkedin.com/in/jenny-raharimanana-230265175/',
                                                   icon("linkedin"),
                                                   'Jenny RAHARIMANANA'),
                                     subtitle = "Etudiante Master ESA",
                                     type = 1,
                                     image = "https://media-exp1.licdn.com/dms/image/C4D03AQGRPRYKSINqZQ/profile-displayphoto-shrink_800_800/0/1634230498709?e=1644451200&v=beta&t=XqHdg-d0Gk8UDSAksJ20BRk8i1wEnhWmKBDQP4iZKmw",
                                   )
                                 ),userBox(
                                   title = userDescription(
                                     title =tags$a(href='https://www.linkedin.com/in/kourosh-kazemi/',
                                                   icon("linkedin"),
                                                   'Kourosh KAZEMI'),
                                     subtitle = "Etudiante Master ESA",
                                     type = 1,
                                     image = "https://media-exp1.licdn.com/dms/image/D4E03AQH-TOP_JzxIOw/profile-displayphoto-shrink_800_800/0/1635976513028?e=1644451200&v=beta&t=Uc43h5eqRNZApasNGocZDSPptwHMxYuSYlA-IOW2-MU",
                                   )
                                 ),
                                 )),
                         tabItem( tabName = "Introduction",
                                  fluidPage(
                                    tags$iframe(src = './Introduction.html', 
                                                width = '100%', height = '2750px', 
                                                frameborder = 0, scrolling = 'auto'
                                    )
                                  )    
                         ),
                         
                         tabItem( tabName = "Penalization",
                                  fluidPage(
                                    tags$iframe(src = './Penalization.html', 
                                                width = '100%', height = '1950px', 
                                                frameborder = 0, scrolling = 'auto'
                                    )
                                  )    
                         ),
                         
                         tabItem( tabName = "Aggregation",
                                  fluidPage(
                                    tags$iframe(src = './Tree_Aggregation.html', 
                                                width = '100%', height = '2000px', 
                                                frameborder = 0, scrolling = 'auto'
                                    )
                                  )    
                         ),
                         tabItem( tabName = "Alternative",
                                  fluidPage(
                                    tags$iframe(src = './Alternatives.html', 
                                                width = '100%', height = '1000px', 
                                                frameborder = 0, scrolling = 'auto'
                                    )
                                  )    
                         ),
                         tabItem( tabName = "Conclusion",
                                  fluidPage(
                                    tags$iframe(src = './Conclusion.html', 
                                                width = '100%', height = '1500px', 
                                                frameborder = 0, scrolling = 'auto'
                                    )
                                  )    
                         ),
                         
                         
                         
                         
                         tabItem( tabName = "logreg",
                                  fluidPage(
                                    tags$iframe(src = './LogisticRegression.html', 
                                                width = '100%', height = '900px', 
                                                frameborder = 0, scrolling = 'auto'
                                    ),
                                    tabBox(width=12,
                                           tabPanel("Summary of the Variables",withSpinner(verbatimTextOutput("Logistic"))),
                                           tabPanel("Confusion Matrix",withSpinner(plotOutput("Logistic_1"))),
                                           tabPanel("ROC",withSpinner(plotOutput("Logistic_2")),
                                                    p("The AUC is equal to :", style = "color:red"), verbatimTextOutput("Logistic_AUC"),
                                                    p("The PR is equal to :", style = "color:red"),verbatimTextOutput("Logistic_PR")),
                                           tabPanel("Summary Plot",withSpinner(plotOutput("Logistic_3"))),
                                           tabPanel("Variables Importance",withSpinner(plotOutput("Logistic_der", height= '900px')))
                                    )
                                    
                                  ))
                         
                         
                         
                       )),
                     
                     
                     
                     
                     controlbar = dashboardControlbar(),
                     title = "DashboardPage"
  ),
  server = function(input, output) {
    
    output$myTable = DT::renderDataTable({
      manqs=input$N
      abers=input$Ntm
      nope=input$Nop
      
      if (manqs==TRUE & abers==FALSE & nope==FALSE){
        
        DT::datatable(
          round(mediane, 3),
          rownames = TRUE,
          extensions = 'Buttons',
          options = list(
            autoWidth = FALSE, scrollX = TRUE,
            columnDefs = list(list(
              width = "100px", targets = "_all"
            )),
            dom = 'tpB',
            lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
            pageLength = 15,
            buttons = list(
              list(
                extend = "collection",
                text = 'Show Less',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}"
                )
              ),
              list(
                extend = "collection",
                text = 'Show More',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(30);
                              dt.ajax.reload();}"
                )
                
              )
            )
          )
        )
      }
      
      else if (nope==TRUE & abers==FALSE & manqs==FALSE){
        
        DT::datatable(
          round(sampled, 3),
          rownames = TRUE,
          extensions = 'Buttons',
          options = list(
            autoWidth = FALSE, scrollX = TRUE,
            columnDefs = list(list(
              width = "100px", targets = "_all"
            )),
            dom = 'tpB',
            lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
            pageLength = 15,
            buttons = list(
              list(
                extend = "collection",
                text = 'Show Less',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}"
                )
              ),
              list(
                extend = "collection",
                text = 'Show More',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(30);
                              dt.ajax.reload();}"
                )
                
              )
            )
          )
        )
      }
      
      else if (abers==TRUE & manqs==FALSE & nope==FALSE){
        
        DT::datatable(
          round(winsorize, 3),
          rownames = TRUE,
          extensions = 'Buttons',
          options = list(
            autoWidth = FALSE, scrollX = TRUE,
            columnDefs = list(list(
              width = "100px", targets = "_all"
            )),
            dom = 'tpB',
            lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
            pageLength = 15,
            buttons = list(
              list(
                extend = "collection",
                text = 'Show Less',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}"
                )
              ),
              list(
                extend = "collection",
                text = 'Show More',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(30);
                              dt.ajax.reload();}"
                )
                
              )
            )
          )
        )
      }
      
      
      else if (abers==TRUE & manqs==TRUE & nope==TRUE){
        
        DT::datatable(
          round(sampled, 3),
          rownames = TRUE,
          extensions = 'Buttons',
          options = list(
            autoWidth = FALSE, scrollX = TRUE,
            columnDefs = list(list(
              width = "100px", targets = "_all"
            )),
            dom = 'tpB',
            lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
            pageLength = 15,
            buttons = list(
              list(
                extend = "collection",
                text = 'Show Less',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}"
                )
              ),
              list(
                extend = "collection",
                text = 'Show More',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(30);
                              dt.ajax.reload();}"
                )
                
              )
            )
          )
        )
      }
      
      
      else {
        
        DT::datatable(
          round(data, 3),
          rownames = TRUE,
          extensions = 'Buttons',
          options = list(
            autoWidth = FALSE, scrollX = TRUE,
            columnDefs = list(list(
              width = "100px", targets = "_all"
            )),
            dom = 'tpB',
            lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
            pageLength = 15,
            buttons = list(
              list(
                extend = "collection",
                text = 'Show Less',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(10);
                              dt.ajax.reload();}"
                )
              ),
              list(
                extend = "collection",
                text = 'Show More',
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                              dt.page.len(30);
                              dt.ajax.reload();}"
                )
                
              )
            )
          )
        )
      }
      

    })
    
    output$histPlot <- renderPlot({
      if (input$columnChoice=="Age"){
        hist(Age, breaks=input$bins)
      }
      else if (input$columnChoice=="UnsecuredLines"){
        hist(UnsecuredLines, breaks=input$bins)
      }
      else if (input$columnChoice=="UnderSixtyDaysPast"){
        hist(UnderSixtyDaysPast, breaks=input$bins)
      }
      else if (input$columnChoice=="DebtRatio"){
        hist(DebtRatio, breaks=input$bins)
      }
      else if (input$columnChoice=="MonthlyIncome"){
        hist(MonthlyIncome, breaks=input$bins)
      }
      else if (input$columnChoice=="NinetyDaysLate"){
        hist(NinetyDaysLate, breaks=input$bins)
      }
      else if (input$columnChoice=="RealEstateLoansOrLines"){
        hist(RealEstateLoansOrLines, breaks=input$bins)
      }
      else if (input$columnChoice=="UnderNinetyDaysPastDue"){
        hist(UnderNinetyDaysPastDue, breaks=input$bins)
      }
      else {
        hist(NumberOfDependents, breaks=input$bins)
      }
      
    })
    
    output$histPlot2 <- renderPlot({
      Sys.sleep(0.2)
      if (input$columnChoice2=="Age2"){ hist(new_variables[,1], main = "Frequency per variable", xlab="Age2")}
      else {hist(new_variables[,2], main = "Frequency per variable", xlab="MonthlyIncome2")}
      
    })
    
    output$var <- renderPrint({
      summary(data[[input$var]])
    })
    
    output$all_var <- renderPrint({
      colnames(data[,c(input$all_var)])
      
    })
    
    output$cor <- renderPlot({
      col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
      M = cor(df[,c(-1,-2)])
      corrplot(M, method="color", col=col(200), type="upper", order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE, tl.cex=0.5)
      
      
    })
    
    output$mytablous = DT::renderDataTable({
      DT::datatable(
        train_new,
        rownames = TRUE,
        extensions = 'Buttons',
        options = list(
          autoWidth = FALSE, scrollX = TRUE,
          columnDefs = list(list(
            width = "100px", targets = "_all"
          ))))
    })
    
    
    
    
    
    
    
    
    
    
    
    ######### LogReg ########
    output$Logistic <- renderPrint({
      summary(reg)
    })
    
    output$Logistic_1 <- renderPlot({
      Confusion_Log_Reg
    })
    
    output$Logistic_2 <- renderPlot({
      precrec_obj <- evalmod(scores = as.numeric(P_log), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$Logistic_3 <- renderPlot({
      precrec_obj2 <- evalmod(scores = as.numeric(P_log), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$Logistic_der <- renderPlot({
      Plot_Log_Reg
    })
    
    output$Logistic_AUC <- renderText({
      precrec_obj <- evalmod(scores = as.numeric(P_log), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$Logistic_PR <- renderText({
      precrec_obj <- evalmod(scores = as.numeric(P_log), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    
    #Ridge
    Ridde         <- reactive({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=0, nfolds=input$cross_Ridge, standardize=T)
    })
    
    output$Ridge <- renderPrint({
      print(Ridde())
    })
    
    output$Ridge_1 <- renderPlot({
      pred_ridge = predict(Ridde(), X_test,s=Ridde()$lambda.min, type="class")
      Y_tot = cbind(Y_test,pred_ridge[,1])
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(pred_ridge))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
    })
    
    
    output$Ridge_2 <- renderPlot({
      pred_ridge = predict(Ridde(), X_test,s=Ridde()$lambda.min, type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$Ridge_3 <- renderPlot({
      pred_ridge = predict(Ridde(), X_test,s=Ridde()$lambda.min, type="class")
      precrec_obj2 <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$Ridge_AUC <- renderText({
      pred_ridge = predict(Ridde(), X_test,s=Ridde()$lambda.min, type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$Ridge_PR <- renderText({
      pred_ridge = predict(Ridde(), X_test,s=Ridde()$lambda.min, type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    
    #Lasso
    Las         <- reactive({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=1, nfolds=input$cross_Lasso, standardize=T)
    })
    
    output$Lasso <- renderPrint({
      print(Las())
    })
    
    output$Lasso_1 <- renderPlot({
      pred_ridge = predict(Las(), X_test,s=Las()$lambda.min, type="class")
      Y_tot = cbind(Y_test,pred_ridge[,1])
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(pred_ridge))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
    })
    
    
    output$Lasso_2 <- renderPlot({
      pred_ridge = predict(Las(), X_test,s=Las()$lambda.min, type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$Lasso_3 <- renderPlot({
      pred_ridge = predict(Las(), X_test,s=Las()$lambda.min, type="class")
      precrec_obj2 <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$Lasso_AUC <- renderText({
      pred_ridge = predict(Las(), X_test,s=Las()$lambda.min, type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$Lasso_PR <- renderText({
      pred_ridge = predict(Las(), X_test,s=Las()$lambda.min, type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_ridge), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
          
    
    #AdaLasso
    output$AdaLasso <- renderPrint({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=0, nfolds=input$cross_AdaLasso, standardize=T)
      w3 <- 1/abs(matrix(coef(cv_modele, s=cv_modele$lambda.min)[, 1][2:(ncol(X)+1)]))^1 ## Using gamma = 1
      w3[w3[,1] == Inf] <- 999999999 ## Replacing values estimated as Infinite for 999999999
      cv.lasso = cv.glmnet(X, train_new$Default, family='binomial', alpha=1, type.measure='class', nfolds=input$cross_AdaLasso, penalty.factor=w3,standardize=T)
      print(cv.lasso)
    })
    
    output$AdaLasso_1 <- renderPlot({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=0, nfolds=input$cross_AdaLasso, standardize=T)
      w3 <- 1/abs(matrix(coef(cv_modele, s=cv_modele$lambda.min)[, 1][2:(ncol(X)+1)]))^1 ## Using gamma = 1
      w3[w3[,1] == Inf] <- 999999999
      pred_adalasso = predict(cv_modele, X_test,s=cv_modele$lambda.min, type="class",penalty.factor=w3, standardize=T)
      Y_tot = cbind(Y_test,pred_adalasso[,1])
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(pred_adalasso))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
      
    })
    
    
    output$AdaLasso_2 <- renderPlot({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=0, nfolds=input$cross_AdaLasso, standardize=T)
      w3 <- 1/abs(matrix(coef(cv_modele, s=cv_modele$lambda.min)[, 1][2:(ncol(X)+1)]))^1 ## Using gamma = 1
      w3[w3[,1] == Inf] <- 999999999
      pred_adalasso = predict(cv_modele, X_test,s=cv_modele$lambda.min, type="class",penalty.factor=w3, standardize=T)
      precrec_obj <- evalmod(scores = as.numeric(pred_adalasso), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$AdaLasso_3 <- renderPlot({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=0, nfolds=input$cross_AdaLasso, standardize=T)
      w3 <- 1/abs(matrix(coef(cv_modele, s=cv_modele$lambda.min)[, 1][2:(ncol(X)+1)]))^1 ## Using gamma = 1
      w3[w3[,1] == Inf] <- 999999999
      pred_adalasso = predict(cv_modele, X_test,s=cv_modele$lambda.min, type="class",penalty.factor=w3, standardize=T)
      precrec_obj2 <- evalmod(scores = as.numeric(pred_adalasso), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$AdaLasso_AUC <- renderText({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=0, nfolds=input$cross_AdaLasso, standardize=T)
      w3 <- 1/abs(matrix(coef(cv_modele, s=cv_modele$lambda.min)[, 1][2:(ncol(X)+1)]))^1 ## Using gamma = 1
      w3[w3[,1] == Inf] <- 999999999
      pred_adalasso = predict(cv_modele, X_test,s=cv_modele$lambda.min, type="class",penalty.factor=w3, standardize=T)
      precrec_obj <- evalmod(scores = as.numeric(pred_adalasso), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$AdaLasso_PR <- renderText({
      cv_modele = cv.glmnet(X, train_new$Default,family="binomial",type.measure="class", alpha=0, nfolds=input$cross_AdaLasso, standardize=T)
      w3 <- 1/abs(matrix(coef(cv_modele, s=cv_modele$lambda.min)[, 1][2:(ncol(X)+1)]))^1 ## Using gamma = 1
      w3[w3[,1] == Inf] <- 999999999
      pred_adalasso = predict(cv_modele, X_test,s=cv_modele$lambda.min, type="class",penalty.factor=w3, standardize=T)
      precrec_obj <- evalmod(scores = as.numeric(pred_adalasso), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    #Elastic Net
    Elastic         <- reactive({
      Lambda_ = (input$cross_ElasticNet_Lambda)/10000
      if (input$Optimal == "Automatic"){
        elastic_model = glmnet(X, train_new$Default, family='binomial', alpha=alpha_opt, type.measure='class', lambda=lambda_opt)
        }
      else {
        elastic_model = glmnet(X, train_new$Default, family='binomial', alpha=input$cross_ElasticNet_Alpha, type.measure='class', lambda=Lambda_)
        }  
    })
    
    
    output$ElasticNet <- renderPrint({
      Lambda_ = (input$cross_ElasticNet_Lambda)/10000
      if (input$Optimal == "Automatic"){
        print(Elastic())}
      else {
        print(Elastic())
            }
      
    })
    
    output$ElasticNet_1 <- renderPlot({
      Lambda_ = (input$cross_ElasticNet_Lambda)/10000
      if (input$Optimal == "Automatic"){
        pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
        Y_tot = cbind(Y_test,pred_elastic[,1])
        d_binomial = tibble("target" = as.factor(test_new$Default),
                            "prediction" = as.factor(pred_elastic))
        basic_table = table(d_binomial)
        cfm = as_tibble(basic_table)
        Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
        Plot_Ridge
      }
      else {
        pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
        Y_tot = cbind(Y_test,pred_elastic[,1])
        d_binomial = tibble("target" = as.factor(test_new$Default),
                            "prediction" = as.factor(pred_elastic))
        basic_table = table(d_binomial)
        cfm = as_tibble(basic_table)
        Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
        Plot_Ridge }
    })
    
    
    output$ElasticNet_2 <- renderPlot({
      Lambda_ = (input$cross_ElasticNet_Lambda)/10000
      if (input$Optimal == "Automatic"){
        pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
        precrec_obj <- evalmod(scores = as.numeric(pred_elastic), labels = test_new$Default)
        autoplot(precrec_obj)
      }
      else {
        pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
        precrec_obj <- evalmod(scores = as.numeric(pred_elastic), labels = test_new$Default)
        autoplot(precrec_obj)        
      }
    })
    
    output$ElasticNet_3 <- renderPlot({
      Lambda_ = (input$cross_ElasticNet_Lambda)/10000
      if (input$Optimal == "Automatic"){
        pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
        precrec_obj2 <- evalmod(scores = as.numeric(pred_elastic), labels = test_new$Default, mode="basic")
        autoplot(precrec_obj2) }
      else {
        pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
        precrec_obj2 <- evalmod(scores = as.numeric(pred_elastic), labels = test_new$Default, mode="basic")
        autoplot(precrec_obj2)
      }
    })
    
    output$ElasticNet_AUC <- renderText({
      pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
      precrec_obj <- evalmod(scores = as.numeric(pred_elastic), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$ElasticNet_PR <- renderText({
      pred_elastic = predict(Elastic(), X_test, type="class", standardize=T)
      precrec_obj <- evalmod(scores = as.numeric(pred_elastic), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    #Cart
    output$Cart <- renderPlot({
      arbre_cart = rpart(Default~., data=train_new) 
      rpart.plot(arbre_cart)
    })
    
    output$Cart_1 <- renderPlot({
      Y_tot = cbind(Y_test,pred_elag)
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(pred_elag))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
    })
    
    output$Cart_2 <- renderPlot({
      precrec_obj <- evalmod(scores = as.numeric(pred_elag), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$Cart_3 <- renderPlot({
      precrec_obj2 <- evalmod(scores = as.numeric(pred_elag), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$Cart_der <- renderPlot({
      V = caret::varImp(arbre1.prune)
      
      ggplot2::ggplot(V, aes(x=reorder(rownames(V),Overall), y=Overall)) +
        geom_point( color="blue", size=4, alpha=0.6)+
        geom_segment( aes(x=rownames(V), xend=rownames(V), y=0, yend=Overall), 
                      color='skyblue') +
        xlab('Variable')+
        ylab('Overall Importance')+
        theme_light() +
        coord_flip() 
    })
    
    output$Cart_AUC <- renderText({
      precrec_obj <- evalmod(scores = as.numeric(pred_elag), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$Cart_PR <- renderText({
      precrec_obj <- evalmod(scores = as.numeric(pred_elag), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    #Bagging
    Baggy         <- reactive({
      bag = bagging(formula=Default~., data=train_Bag, coob=T, par=T, nbagg=input$cross_Bagging, control= rpart.control(minsize=2, cp=0))
    })
    
    output$Bagging <- renderPrint({
      print(Baggy())
    })
    
    output$Bagging_1 <- renderPlot({
      pred_bag = predict(Baggy(),newdata=test_new,type="class")
      Y_tot = cbind(Y_test,pred_bag)
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(pred_bag))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
    })
    
    output$Bagging_2 <- renderPlot({
      pred_bag = predict(Baggy(),newdata=test_new,type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_bag), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$Bagging_3 <- renderPlot({
      pred_bag = predict(Baggy(),newdata=test_new,type="class")
      precrec_obj2 <- evalmod(scores = as.numeric(pred_bag), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$Bagging_der <- renderPlot({
      V_bag = caret::varImp(Baggy())
      
      ggplot2::ggplot(V_bag, aes(x=reorder(rownames(V_bag),Overall), y=Overall)) +
        geom_point( color="blue", size=4, alpha=0.6)+
        geom_segment( aes(x=rownames(V_bag), xend=rownames(V_bag), y=0, yend=Overall), 
                      color='skyblue') +
        xlab('Variable')+
        ylab('Overall Importance')+
        theme_light() +
        coord_flip() 
    })
    
    output$Bagging_AUC <- renderText({
      pred_bag = predict(Baggy(),newdata=test_new,type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_bag), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$Bagging_PR <- renderText({
      pred_bag = predict(Baggy(),newdata=test_new,type="class")
      precrec_obj <- evalmod(scores = as.numeric(pred_bag), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })

    
    #Random Forest
    Random_Forest         <- reactive({
      if (input$Optimus == "Automatic"){
        rf.opt=randomForest(as.factor(train_sampled_rf$Default)~.,data=train_sampled_rf, mtry=30, par = T, ntree=200, proximity=F, importance=T)}
      else {
        rf.opt=randomForest(as.factor(train_sampled_rf$Default)~.,data=train_sampled_rf,mtry=input$cross_Random , ntree=input$cross_Random_1, proximity=F, par = T, importance=T)
      }
    })
    
    output$Random <- renderPrint({
      print(Random_Forest())
    })
    

    output$Random_1 <- renderPlot({
      if (input$Optimus == "Automatic"){
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        Y_tot = cbind(Y_test,rf.pred)
        d_binomial = tibble("target" = as.factor(test_new$Default),
                            "prediction" = as.factor(rf.pred))
        basic_table = table(d_binomial)
        cfm = as_tibble(basic_table)
        Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
        Plot_Ridge
      }
      else {
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        Y_tot = cbind(Y_test,rf.pred)
        d_binomial = tibble("target" = as.factor(test_new$Default),
                            "prediction" = as.factor(rf.pred))
        basic_table = table(d_binomial)
        cfm = as_tibble(basic_table)
        Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
        Plot_Ridge }
    })
    
    
    output$Random_2 <- renderPlot({
      if (input$Optimus == "Automatic"){
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default)
        autoplot(precrec_obj)
      }
      else {
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default)
        autoplot(precrec_obj)        
      }
    })
    
    output$Random_3 <- renderPlot({
      if (input$Optimus == "Automatic"){
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj2 <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default, mode="basic")
        autoplot(precrec_obj2) }
      else {
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj2 <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default, mode="basic")
        autoplot(precrec_obj2)
      }
    })

    output$Random_der <- renderPlot({
      if (input$Optimus == "Automatic"){
        varImpPlot(Random_Forest())}
      else {
        varImpPlot(Random_Forest())
      }
    })
    
    output$Random_AUC <- renderText({
      if (input$Optimus == "Automatic"){
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default)
        auc=auc(precrec_obj)
        auc$aucs[1]
      }
      else {
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default)
        auc=auc(precrec_obj)
        auc$aucs[1]        
      }
    })
    
    output$Random_PR <- renderText({
      if (input$Optimus == "Automatic"){
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default)
        PR=auc(precrec_obj)
        PR$aucs[2]
      }
      else {
        rf.pred = predict(Random_Forest(),newdata=test_rf)
        precrec_obj <- evalmod(scores = as.numeric(rf.pred), labels = test_new$Default)
        PR=auc(precrec_obj)
        PR$aucs[2]        
      }
    })
    
    
    
    
    
    
    #AdaBoost
    AdaBoo         <- reactive({
      if (input$Optima == "Automatic"){
        boo= boosting.cv(Default~., test_sampled_Ada, boos=T, mfinal=30,coeflearn="Breiman", v=input$cross_AdaBoost_2, control=rpart.control(maxdepth=5),par=T)
      }
      else {
        boo= boosting(Default~., train_sampled_Ada, boos=T, mfinal=input$cross_AdaBoost,coeflearn="Breiman",control=rpart.control(maxdepth=input$cross_AdaBoost_1),par=T)
      }
    })
    
    
    
    output$AdaBoost <- renderPrint({
      if (input$Optima == "Automatic"){
        print(AdaBoo())
        }
      else {
        print(AdaBoo())
      }
      
    })
    
    
    output$AdaBoost_1 <- renderPlot({
      if (input$Optima == "Automatic"){
        boo=AdaBoo()
        Y_tot = cbind(Y_test,boo$class)
        d_binomial = tibble("target" = as.factor(test_new$Default),
                            "prediction" = as.factor(boo$class))
        basic_table = table(d_binomial)
        cfm = as_tibble(basic_table)
        Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
        Plot_Ridge
      }
      else {
        pred_Ada = predict(AdaBoo(),test_rf[,-1])
        Y_tot = cbind(Y_test,pred_Ada$class)
        d_binomial = tibble("target" = as.factor(test_new$Default),
                            "prediction" = as.factor(pred_Ada$class))
        basic_table = table(d_binomial)
        cfm = as_tibble(basic_table)
        Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
        Plot_Ridge }
    })
    
    
    output$AdaBoost_2 <- renderPlot({
      if (input$Optima == "Automatic"){
        boo=AdaBoo()
        precrec_obj <- evalmod(scores = as.numeric(boo$class), labels = test_new$Default)
        autoplot(precrec_obj)
      }
      else {
        pred_Ada = predict(AdaBoo(),test_rf[,-1])
        precrec_obj <- evalmod(scores = as.numeric(pred_Ada$class), labels = test_new$Default)
        autoplot(precrec_obj)
      }
    })
    
    output$AdaBoost_3 <- renderPlot({
      if (input$Optima == "Automatic"){
        boo=AdaBoo()
        precrec_obj2 <- evalmod(scores = as.numeric(boo$class), labels = test_new$Default, mode="basic")
        autoplot(precrec_obj2) }
      else {
        pred_Ada = predict(AdaBoo(),test_rf[,-1])
        precrec_obj2 <- evalmod(scores = as.numeric(pred_Ada$class), labels = test_new$Default, mode="basic")
        autoplot(precrec_obj2)
      }
    })
    
    output$AdaBoost_der <- renderPlot({
      if (input$Optima == "Manual"){
        importanceplot(AdaBoo())}
    })
    
    output$AdaBoost_AUC <- renderText({
      if (input$Optima == "Automatic"){
        boo=AdaBoo()
        precrec_obj <- evalmod(scores = as.numeric(boo$class), labels = test_new$Default)
        auc=auc(precrec_obj)
        auc$aucs[1]
      }
      else {
        pred_Ada = predict(AdaBoo(),test_rf[,-1])
        precrec_obj <- evalmod(scores = as.numeric(pred_Ada$class), labels = test_new$Default)
        auc=auc(precrec_obj)
        auc$aucs[1]
      }
    })
    
    output$AdaBoost_PR <- renderText({
      if (input$Optima == "Automatic"){
        boo=AdaBoo()
        precrec_obj <- evalmod(scores = as.numeric(boo$class), labels = test_new$Default)
        PR=auc(precrec_obj)
        PR$aucs[2]
      }
      else {
        pred_Ada = predict(AdaBoo(),test_rf[,-1])
        precrec_obj <- evalmod(scores = as.numeric(pred_Ada$class), labels = test_new$Default)
        PR=auc(precrec_obj)
        PR$aucs[2]
      }
    })
    
    
    
    
    
    
    #Gradient Boosting
    GradBoost         <- reactive({
      Shrink=(input$cross_Gb_1/10000)
      model_gbm = gbm(train_sampled_rf$Default~., data=train_sampled_rf, distribution="bernoulli", cv.folds=input$cross_Gb, shrinkage=Shrink, n.trees=input$cross_Gb_2)
    })
    
    output$Gb <- renderPrint({
      print(GradBoost())
    })
    
    
    output$Gb_1 <- renderPlot({
      predict_gbm = predict.gbm(GradBoost(),newdata=test_rf[,-1],type="response", n.trees=10,single.tree=T)
      prediction_gbm_test=ifelse(predict_gbm>0.5,1,0)
      Y_tot=cbind(prediction_gbm_test,test_rf$Default)
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(prediction_gbm_test))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
    })
    
    
    output$Gb_2 <- renderPlot({
      predict_gbm = predict.gbm(GradBoost(),newdata=test_rf[,-1],type="response", n.trees=10,single.tree=T)
      prediction_gbm_test=ifelse(predict_gbm>0.5,1,0)
      precrec_obj <- evalmod(scores = as.numeric(predict_gbm), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$Gb_3 <- renderPlot({
      predict_gbm = predict.gbm(GradBoost(),newdata=test_rf[,-1],type="response", n.trees=10,single.tree=T)
      prediction_gbm_test=ifelse(predict_gbm>0.5,1,0)
      precrec_obj2 <- evalmod(scores = as.numeric(predict_gbm), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$Gb_der <- renderPlot({
      summary.gbm(GradBoost())
    })
    
    
    
    
    output$Gb_AUC <- renderText({
      predict_gbm = predict.gbm(GradBoost(),newdata=test_rf[,-1],type="response", n.trees=10,single.tree=T)
      prediction_gbm_test=ifelse(predict_gbm>0.5,1,0)
      precrec_obj <- evalmod(scores = as.numeric(predict_gbm), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$Gb_PR <- renderText({
      predict_gbm = predict.gbm(GradBoost(),newdata=test_rf[,-1],type="response", n.trees=10,single.tree=T)
      prediction_gbm_test=ifelse(predict_gbm>0.5,1,0)
      precrec_obj <- evalmod(scores = as.numeric(predict_gbm), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    
    
    
    
    
    #XGBoost
    XGBoost         <- reactive({
      xgb.model <- xgb.train(data = xgb.data.train
                             , params = list(objective = "binary:logistic"
                                             , eta = as.numeric(input$eta)
                                             , max.depth = as.numeric(input$max_depth)
                                             , min_child_weight = as.numeric(input$min_child_weight)
                                             , subsample = 0.5
                                             , colsample_bytree = as.numeric(input$colsample_bytree)
                                             , nthread = 3
                                             , scale_pos_weight = as.numeric(input$scale_pos_weight)
                                             , eval_metric = input$eval_metric
                             )
                             , watchlist = list(train = xgb.data.train,test = xgb.data.test1)
                             , nrounds = as.numeric(input$nrounds)
                             , early_stopping_rounds = as.numeric(input$early_stopping_rounds)
                             , print_every_n = 20)
      })
    
    
    output$XG <- renderPrint({
      
      print(XGBoost())
      
    })
    
    output$XG_1 <- renderPlot({
      params = list(booster = "gbtree", objective = "binary:logistic", eta=as.numeric(input$eta), gamma=0, max_depth=as.numeric(input$max_depth), min_child_weight=as.numeric(input$min_child_weight), subsample=1, colsample_bytree=as.numeric(input$colsample_bytree))
      xgb1 = xgb.train (params = params, data = xgb.data.train, nrounds = 500, watchlist = list(val=xgb.data.test,train=xgb.data.train), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")
      xgbpred = predict (xgb1,xgb.data.test)
      xgbpred = ifelse (xgbpred > 0.5,1,0)
      Y_tot=cbind(xgbpred,test_rf$Default)
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(xgbpred))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
    })
    
    output$XG_2 <- renderPlot({
      params = list(booster = "gbtree", objective = "binary:logistic", eta=as.numeric(input$eta), gamma=0, max_depth=as.numeric(input$max_depth), min_child_weight=as.numeric(input$min_child_weight), subsample=1, colsample_bytree=as.numeric(input$colsample_bytree))
      xgb1 = xgb.train (params = params, data = xgb.data.train, nrounds = 500, watchlist = list(val=xgb.data.test,train=xgb.data.train), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")
      xgbpred = predict (xgb1,xgb.data.test)
      precrec_obj <- evalmod(scores = as.numeric(xgbpred), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$XG_3 <- renderPlot({
      params = list(booster = "gbtree", objective = "binary:logistic", eta=as.numeric(input$eta), gamma=0, max_depth=as.numeric(input$max_depth), min_child_weight=as.numeric(input$min_child_weight), subsample=1, colsample_bytree=as.numeric(input$colsample_bytree))
      xgb1 = xgb.train (params = params, data = xgb.data.train, nrounds = 500, watchlist = list(val=xgb.data.test,train=xgb.data.train), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")
      xgbpred = predict (xgb1,xgb.data.test)
      precrec_obj2 <- evalmod(scores = as.numeric(xgbpred), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$XG_der <- renderPlot({
      train_xgb = xgb.data.train
      test_xgb = xgb.data.test
      model <- xgb.dump(XGBoost(), with_stats=TRUE)
      names = dimnames(train_xgb)[[2]]
      importance_matrix <- xgb.importance(names, model=XGBoost())
      xgb.plot.importance(importance_matrix, top_n=7)
    })
    
    output$XG_AUC <- renderText({
      params = list(booster = "gbtree", objective = "binary:logistic", eta=as.numeric(input$eta), gamma=0, max_depth=as.numeric(input$max_depth), min_child_weight=as.numeric(input$min_child_weight), subsample=1, colsample_bytree=as.numeric(input$colsample_bytree))
      xgb1 = xgb.train (params = params, data = xgb.data.train, nrounds = 500, watchlist = list(val=xgb.data.test,train=xgb.data.train), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")
      xgbpred = predict (xgb1,xgb.data.test)
      precrec_obj <- evalmod(scores = as.numeric(xgbpred), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$XG_PR <- renderText({
      params = list(booster = "gbtree", objective = "binary:logistic", eta=as.numeric(input$eta), gamma=0, max_depth=as.numeric(input$max_depth), min_child_weight=as.numeric(input$min_child_weight), subsample=1, colsample_bytree=as.numeric(input$colsample_bytree))
      xgb1 = xgb.train (params = params, data = xgb.data.train, nrounds = 500, watchlist = list(val=xgb.data.test,train=xgb.data.train), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")
      xgbpred = predict (xgb1,xgb.data.test)
      precrec_obj <- evalmod(scores = as.numeric(xgbpred), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    
    
    #Stochastic Boosting
    Stochastic         <- reactive({
      Iteration = (input$cross_SB-15)
      Shrink=(input$cross_SB_1/10)
      Baggy=(input$cross_SB_2/10)
      model_sto_boo = ada(x=train_sampled_rf[,-1], y=train_sampled_rf[,1],loss="logistic", type="real", iter=Iteration,nu=Shrink,bag.frac=Baggy)
    })
    
    output$SB <- renderPrint({
      print(Stochastic())
    })
    
    
    output$SB_1 <- renderPlot({
      pred_sto_boo = predict(Stochastic(),newdata=test_rf)
      Y_tot=cbind(pred_sto_boo,test_rf$Default)
      d_binomial = tibble("target" = as.factor(test_new$Default),
                          "prediction" = as.factor(pred_sto_boo))
      basic_table = table(d_binomial)
      cfm = as_tibble(basic_table)
      Plot_Ridge = plot_confusion_matrix(cfm, target_col = "target", prediction_col = "prediction", counts_col = "n")
      Plot_Ridge
    })
    
    
    output$SB_2 <- renderPlot({
      pred_sto_boo = predict(Stochastic(),newdata=test_rf)
      precrec_obj <- evalmod(scores = as.numeric(pred_sto_boo), labels = test_new$Default)
      autoplot(precrec_obj)
    })
    
    output$SB_3 <- renderPlot({
      pred_sto_boo = predict(Stochastic(),newdata=test_rf)
      precrec_obj2 <- evalmod(scores = as.numeric(pred_sto_boo), labels = test_new$Default, mode="basic")
      autoplot(precrec_obj2)
    })
    
    output$SB_der <- renderPlot({
      varplot(Stochastic())
    })
    
    output$SB_AUC <- renderText({
      pred_sto_boo = predict(Stochastic(),newdata=test_rf)
      precrec_obj <- evalmod(scores = as.numeric(pred_sto_boo), labels = test_new$Default)
      auc=auc(precrec_obj)
      auc$aucs[1]
    })
    
    output$SB_PR <- renderText({
      pred_sto_boo = predict(Stochastic(),newdata=test_rf)
      precrec_obj <- evalmod(scores = as.numeric(pred_sto_boo), labels = test_new$Default)
      PR=auc(precrec_obj)
      PR$aucs[2]
    })
    
    
    
    
    
  })
