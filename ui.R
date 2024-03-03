# Application for mini-frac test interpretation

library(shiny)
library(mathjaxr)
library(openxlsx)
library(shinydashboard)
library(noteMD)
library(plotly)
library(readr)
library(readxl)
library(DT) 
library(rmarkdown)


shinyUI(
  dashboardPage (
    skin = "green",
    dashboardHeader(title = "Mini Frac"),
    
    dashboardSidebar(
      sidebarMenu(
        
        #Select the language
        menuItem("English", tabName = "English",
                 menuSubItem("Input Data", tabName = "InputDataEng"),
                 menuSubItem("Results", tabName = "ResultsEng")),
        menuItem("Русский", tabName = "Русский",
                 menuSubItem("Входные данные", tabName = "InputDataRU"),
                 menuSubItem("Результаты анализа", tabName = "ResultsRu"))
      )),
    dashboardBody(
      
      
      # Font style
      tags$style(
        "p, div, h4, h3, h2, h1, pre {
    # color: #27AE60;
     font-family: 'Montserrat';
     
      font-size: 18px;
      
    }"),
      
      # Math
      tags$style(
        "span {
      # color: #27AE60;
     font-family: 'Montserrat';
     letter-spacing: .1rem;
      font-size: 18px;

    }"),
      
      tags$head(tags$style(HTML('
    
    @font-face {  
  src: url(Montserrat-VariableFont_wght.ttf);
  font-family: "Montserrat";
}
      .main-header .logo {
      font-family: "Montserrat";
      
        
        font-weight: bold;
        font-size: 18px;
      }
    '))),
      
      tabItems(
        
        #################################English##################################
        
        # Input data 
        tabItem(tabName = "InputDataEng",
                fluidRow(  
                  column (5, 
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Permeable (leakoff) thickness, m',"</p>")),
                                numericInput("hp_en", label = NULL , value = 12, min = 0, step = 1),
                                HTML(paste("<p>",'Fracture height, m',"</p>")),
                                numericInput("hf_en", label = NULL, value = 20, min = 0, step = 1),
                                HTML(paste("<p>",'Young modulus, atm',"</p>")),
                                numericInput("E_en", label = NULL, value = 15e+4, min = 0, step = 1),
                                HTML(paste("<p>",'Poisson ratio',"</p>")),
                                numericInput("puas_en", label = NULL, value = 0.18, min = 0, step = 1),
                                HTML(paste("<p>",'Closure pressure, atm',"</p>")),
                                numericInput("pc_en", label = NULL, value = 300, min = 0)),
                            width = NULL,height = 470, collapsible = TRUE, collapsed = FALSE)),
                  
                  column (7, 
                          box(div(style = 'height: 100%; width: 100%',
                                  plotlyOutput("injection_en")),
                              width = NULL, height = 470, collapsible = TRUE, collapsed = FALSE
                          )),
                  
                  column (12, 
                          
                          box( 
                            fluidRow(
                              column (9,
                                      wellPanel(fileInput("file_en", h4("Select Excel file with mini-frac test data")
                                      ))),
                              column (3,
                                      downloadButton('sample_file_en',"Download mini-frac test sample",class="butt" ),br(),
                                      tags$head(tags$style(".butt{background-color:#27AE60;} .butt{color: #000000;}"))),
                              
                              column (12, 
                                      DT::dataTableOutput("contents_en")
                              )),
                            width = NULL))
                  
                  
                )), 
        
        # Results
        tabItem(tabName = "ResultsEng", 
                fluidRow(
                  column (6, 
                          box( div(style = 'overflow-x: scroll',
                                   tableOutput("results_en")),
                               width = NULL,  height = 350, collapsible = TRUE, collapsed = FALSE
                          ), 
                          box(div(style = 'overflow-x: scroll', 
                                  column (6, 
                                          downloadButton('describe_download_en',"Download Excel Report",class="butt" ), br(),
                                          tags$head(tags$style(".butt{background-color:#27AE60;} .butt{color: #000000;}"))),
                                  
                                  column (6, 
                                          downloadButton('describe_download_test_en',"Download Word Report",class="butt" ), br(), 
                                          tags$head(tags$style(".butt{background-color:##27AE60;} .butt{color: #000000;}")))), 
                              width = NULL, collapsible = TRUE, collapsed = FALSE, height = 130),
                          
                          box(div(style = 'height: 100%; width: 100%', 
                                  column(12,
                                         helpText("Note: All entered text will be displayed in the report") ),
                                  column(12,
                                         tags$textarea(
                                           "Enter your comment to the calculations",
                                           id    = 'markdowninput_en',
                                           rows  = 3,
                                           style = 'width:100%;')),
                                  helpText("Preview:"),
                                  htmlOutput('htmlmarkdown_en'),br(),br()), width = NULL, collapsible = TRUE, collapsed = FALSE)
                  ), 
                  
                  column (6, 
                          box(div (style ='overflow-x: scroll',
                                   plotlyOutput("g_func_plot_en")),
                              width = NULL, height = 500, collapsible = TRUE, collapsed = FALSE),
                          
                          box( div (style ='overflow-x: scroll',
                                    tableOutput("g_func_table_en")),
                               width = NULL, collapsible = TRUE, collapsed = FALSE)
                  )
                  
                )
                
        ),
        
        #################################Русский##################################
        
        # Исходные данные
        tabItem(tabName = "InputDataRU",
                fluidRow(  
                  column (5, 
                          box(
                            div(style = 'height: 100%; width: 100%',
                                withMathJax(),
                                HTML(paste("<p>",'Проницаемая мощность (утечек), м',"</p>")),
                                numericInput("hp", label = NULL , value = 12, min = 0, step = 1),
                                HTML(paste("<p>",'Высота трещины, м',"</p>")),
                                numericInput("hf", label = NULL, value = 20, min = 0, step = 1),
                                HTML(paste("<p>",'Модуль Юнга, атм',"</p>")),
                                numericInput("E", label = NULL, value = 15e+4, min = 0, step = 1),
                                HTML(paste("<p>",'Коэффициент Пуассона',"</p>")),
                                numericInput("puas", label = NULL, value = 0.18, min = 0, step = 1),
                                HTML(paste("<p>",'Давление смыкания, атм',"</p>")),
                                numericInput("pc", label = NULL, value = 300, min = 0)),
                            width = NULL,height = 470, collapsible = TRUE, collapsed = FALSE)),
                  
                  
                  column (7, 
                          box(div(style = 'height: 100%; width: 100%',
                                  plotlyOutput("injection")),
                              width = NULL, height = 470, collapsible = TRUE, collapsed = FALSE
                          )),
                  
                  column (12, 
                          
                          box( 
                            fluidRow(
                              column (9,
                                      wellPanel(fileInput("file", h4("Выберите файл Excel с данными по тесту мини-ГРП")
                                      ))),
                              column (3,
                                      downloadButton('sample_file',"Скачать образец теста мини-ГРП",class="butt" ),br(),
                                      tags$head(tags$style(".butt{background-color:#27AE60;} .butt{color: #000000;}"))),
                              
                              column (12, 
                                      DT::dataTableOutput("contents")
                              )),
                            width = NULL))
                  
                  
                )), 
        
        # Результаты
        tabItem(tabName = "ResultsRu", 
                fluidRow(
                  column (6, 
                          box( div(style = 'overflow-x: scroll',
                                   tableOutput("results")),
                               width = NULL,  height = 350, collapsible = TRUE, collapsed = FALSE
                          ), 
                          box(div(style = 'overflow-x: scroll', 
                                  column (6, 
                                          downloadButton('describe_download',"Скачать в формате Excel",class="butt" ), br(),
                                          tags$head(tags$style(".butt{background-color:#27AE60;} .butt{color: #000000;}"))),
                                  
                                  column (6, 
                                          downloadButton('describe_download_test',"Скачать в формате Word",class="butt" ), br(), 
                                          tags$head(tags$style(".butt{background-color:##27AE60;} .butt{color: #000000;}")))), 
                              width = NULL, collapsible = TRUE, collapsed = FALSE, height = 130),
                          
                          box(div(style = 'height: 100%; width: 100%', 
                                  column(12,
                                         helpText("Примечание: Весь введённый текст будет отображён в отчёте")),
                                  column(12,
                                         tags$textarea(
                                           "Введите свои комментарии к расчётам",
                                           id    = 'markdowninput',
                                           rows  = 3,
                                           style = 'width:100%;'
                                         )),
                                  helpText("Предпросмотр:"),
                                  htmlOutput('htmlmarkdown'),br(),br()), width = NULL, collapsible = TRUE, collapsed = FALSE)
                  ), 
                  
                  column (6, 
                          box(div (style ='overflow-x: scroll',
                                   plotlyOutput("g_func_plot")),
                              width = NULL, height = 500, collapsible = TRUE, collapsed = FALSE),
                          
                          box( div (style ='overflow-x: scroll',
                                    tableOutput("g_func_table")),
                               width = NULL, collapsible = TRUE, collapsed = FALSE)
                  )
                  
                )
                
        )
      )))

)
