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

# setwd('/Users/makar/OneDrive/Desktop/UE_Applied_Sciences/Projects/Mini-Frac')

shinyServer(
  
  function(input, output) {

###################################ENGLISH####################################
    
    #Input data table
    Name_input_en <- as.character (
      c("Permeable (leakoff) thickness, m",
        "Fracture height, m",
        "Young modulus, atm",
        "Poisson ratio",
        "Closure pressure, atm"
      ))
    
    Value_input_en <- reactive ({ as.numeric(
      c(input$hp_en,
        input$hf_en,
        input$E_en,
        input$puas_en,
        input$pc_en)
    )
    })
    
    # Formatting input value
    Value1_input_en <- reactive ({
      format(Value_input_en(), scientific = FALSE)
    })
    
    df_input_en <- reactive({
      data.frame(
        Name_input_en, Value1_input_en(), stringsAsFactors = FALSE)
    })
    
    
    #Download an example file with the mini-frac test
    output$sample_file_en <- downloadHandler(
      filename <- function() {
        paste("Mini-frac test sample", "xlsx", sep=".")
      },
      content <- function(file) {
        #temp <- file.path(tempdir(), "report.Rmd")
        file.copy(file.path(getwd(),'minifrac_test_en.xlsx'), file,overwrite = TRUE)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    #Data frame recognition
    df_inject_en <- reactive ({
      tryCatch(
        {
          req(input$file_en)
          df_inject_en <- read_excel(input$file_en$datapath
          )
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          df_inject_en <- read_excel ("minifrac_test_en.xlsx")
        }
      )
    })
    
    #Table with data on mini-frac
    output$contents_en <- DT::renderDataTable({
      df_inject_en()
    })
    
    #Plotting injection schedule
    output$injection_en <- renderPlotly({
      
      df_inject = df_inject_en()
      
      colnames(df_inject)<-c('t','u', 'p', 'V', 'g')
      
      fig <- plot_ly()
      # Add traces
      fig <- fig %>% add_trace(x = ~df_inject$t, y = ~df_inject$u, name = "Bottomhole Injection rate, m3/min", mode = "lines+markers", type = "scatter", 
                               line = list(color = 'rgb(60, 77, 233)'), marker = list(color = 'rgb(60, 77, 233)'))
      
      m <- list(
        l = 70,
        r = 70,
        b = 50,
        t = 50,
        pad = 10
      )
      
      ay <- list(color = "rgb(233, 16, 16)",
                 overlaying = "y",
                 side = "right",
                 title = list(text ="Bottomhole Pressure, atm", standoff = 20))
      
      fig <- fig %>% add_trace(df_inject, x = ~df_inject$t, y = ~df_inject$p, name = "Bottomhole Pressure, atmм", yaxis = "y2", mode = "lines+markers", type = "scatter",
                               line = list(color = 'rgb(233, 16, 16)'), marker = list(color = 'rgb(233, 16, 16)'))
      
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = "Mini-Frac Test", yaxis2 = ay,
        xaxis = list(title="Time, min"),
        yaxis = list(title="Bottomhole Injection rate, m3/min", color ='rgb(60, 77, 233)')
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               margin = m,
               # legend = list(x = 0.01, y = 0.9),
               showlegend = FALSE
        )
      
      fig 
    })
    
    #g-function
    DLgf1 <- function (din) {
      
      a0 = 1.41495
      a1 = 79.4125
      a2 = 632.457
      a3 = 1293.07
      a4 = 763.19
      a5 = 94.0367
      b0 = 1
      b1 = 54.8534
      b2 = 383.11
      b3 = 540.342
      b4 = 167.741
      b5 = 6.49129
      b6 = -0.0765693
      
      if (din < 15) {
        d = din 
      } else {
        d = 15
      }
      
      gg <- (a0 + a1 * d + a2 * d ^ 2 + a3 * d ^ 3 + a4 * d ^ 4 + a5 * d ^ 5) / (b0 + b1 * d + b2 * d ^ 2 + b3 * d ^ 3 + b4 * d ^ 4 + b5 * d ^ 5 + b6 * d ^ 6)
      
      if (din > 15) {
        gg <- (gg - 2 * (din + 1) ^ 0.5) * exp(15 - din) + 2 * (din + 1) ^ 0.5
      }
      gg
    }
    
    #Straight line
    straight <- function (b, m , gg) {
      str <- (b + m * gg)
      str
    } 
    
    #Number of values from the table included in the g-function (where column 5 in the original table is 1)
    ng_en <- reactive ({
      data = df_inject_en()
      ng_en = 0
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 5] == 1 ) {    
            ng_en <- ng_en + 1
          } 
        }
      }
      ng_en
    })
    
    #Time (g-function)
    tt_en <- reactive ({
      data = df_inject_en()
      ng_en = 0
      tt_en <- c()
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 5] == 1 ) {    
            ng_en <- ng_en + 1 
            tt_en[ng_en] = as.numeric(data [i, 1]) * 60 
          } 
        }
      }
      tt_en
    })
    
    #Static pressure (g-function)
    pp_en <- reactive ({
      data = df_inject_en()
      ng_en = 0
      pp_en <- c()
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 5] == 1 ) {    
            ng_en <- ng_en + 1
            pp_en[ng_en] = as.numeric(data [i, 3])
          } 
        }
      }
      pp_en
    })
    
    #Injection time of fracturing fluid
    tni_en <- reactive ({
      data = df_inject_en() 
      tni1_en = 0
      Vi_en=0
      bpm = 0.1589873 / 60
      
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 4] == 1) {
            tni_en <- as.numeric(data [i, 1]) * 60
            qni_en <- as.numeric(data [i, 2]) / 0.1589 * bpm / 2
            Vi_en <- Vi_en + qni_en * (tni_en - tni1_en)
            tni1_en = tni_en
          }
        }
      }
      tni_en
    })
    
    #Bottom-hole injection speed
    qni_en <- reactive ({
      data = df_inject_en() 
      tni1_en = 0
      Vi_en=0
      bpm = 0.1589873 / 60
      
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 4] == 1) {
            tni_en <- as.numeric(data [i, 1]) * 60
            qni_en <- as.numeric(data [i, 2]) / 60 / 2
            Vi_en <- Vi_en + qni_en * (tni_en - tni1_en)
            tni1_en = tni_en
          }
        }
      }
      qni_en
    })
    
    #Pumped volume of frac fluid
    Vi_en <- reactive ({
      data = df_inject_en() 
      tni1_en = 0
      Vi_en=0
      
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 4] == 1) {
            tni_en <- as.numeric(data [i, 1]) * 60
            qni_en <- as.numeric(data [i, 2]) / 60 / 2
            Vi_en <- Vi_en + qni_en * (tni_en - tni1_en)
            tni1_en = tni_en
          }
        }
      }
      Vi_en
    })
    
    #g-function value
    gg_en <- reactive ({
      gg_en <- c()
      for (i in 1 : ng_en())
      {
        dt = (tt_en()[i] - tni_en())/tni_en()  
        gg_en[i] <- DLgf1 (dt) 
        
      }
      gg_en
    })
    
    #Slope coefficient of linear trend
    mm_en <- reactive ({
      ng = ng_en()
      sumxy = 0
      sumx = 0
      sumy = 0
      sumx2 = 0
      
      for (i in 1:ng)
      {
        sumxy = sumxy + gg_en()[i] * pp_en()[i]
        sumx = sumx + gg_en()[i]
        sumy = sumy + pp_en()[i]
        sumx2 = sumx2 + gg_en()[i] ^ 2
      }
      mm_en <-(ng * sumxy - sumx * sumy) / (ng * sumx2 - sumx ^ 2)
      mm_en
    })
    
    #Shift coefficient of linear trend
    bb_en <- reactive ({
      ng = ng_en()
      sumxy = 0
      sumx = 0
      sumy = 0
      sumx2 = 0
      
      for (i in 1:ng)
      {
        sumxy = sumxy + gg_en()[i] * pp_en()[i]
        sumx = sumx + gg_en()[i]
        sumy = sumy + pp_en()[i]
        sumx2 = sumx2 + gg_en()[i] ^ 2
      }
      bb_en <- (sumy - mm_en() * sumx) / ng
      bb_en
    })
    
    str_en <- reactive ({
      str_en <- c()
      for (i in 1 : ng_en())
      {
        str_en[i] <- straight (bb_en(), mm_en() , gg_en()[i])
      }
      str_en
    })
    
    
    df_g_en <- reactive ({
      data.frame(
        gg_en(), pp_en(), str_en(), stringsAsFactors = FALSE)
    })
    
    output$g_func_table_en <- renderTable({
      df=df_g_en()
      colnames(df)<-c('g-function','Shut-in Pressure, atm', 'Linear trend')
      df
    })
    
    # Plotting g-function
    output$g_func_plot_en <- renderPlotly({
      df = df_g_en()
      
      colnames(df)<-c('g','p', 'lin')
      
      fig <- plot_ly()
      # Add traces
      fig <- fig %>% add_trace(x = ~df$g, y = ~df$p, name = "Pressure, atm", mode = "markers", type = "scatter",
                               marker = list(color = '#3131E1', size = 7))
      
      
      fig <- fig %>% add_trace(x = ~df$g, y = ~df$lin, name = "Linear trend", mode = "lines", type = "scatter",
                               line = list(color = '#27AE60'))
      #
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = "g-function plot",
        xaxis = list(title="g-function"),
        yaxis = list(title="Pressure, atm", range = c(0, 800))
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               # legend = list(x = 0.01, y = 0.9),
               showlegend = FALSE
        )
      
      fig
    })
    
    #Plane strain modulus
    Ep_en <- reactive ({
      input$E_en / (1 - input$puas_en^2)
    })
    
    
    #Half-length fracture
    xf_en <- reactive ({
      2 * Ep_en() * Vi_en() / (pi * input$hf_en ^ 2 * (bb_en() - input$pc_en))
    })
    
    #Apparent leakoff coefficient
    CL_en <- reactive ({
      pi * input$hf_en / (4 * sqrt(tni_en()) * Ep_en()) * (-mm_en())
    })
    
    #Fracture width
    w_en <- reactive ({
      Vi_en() / (xf_en() * input$hf_en) - 2.83 * CL_en() * sqrt(tni_en())
    })
    
    #Efficiency of fracturing fluid
    eta_en <- reactive ({
      w_en() * xf_en() * input$hf_en / Vi_en()
    })
    
    DLrp1 <- function(hp, hf) {
      if (hp <= hf){
        rp <- hp / hf
      } else {
        rp <- 1
      }
    }
    
    rp_en <- reactive ({ DLrp1 (input$hp_en, input$hf_en) })
    
    #Leakoff coefficient in permeable layer
    CLr_en <- reactive ({
      (1 / rp_en()) * CL_en()
    })
    
    psi = 6894.757
    
    
    #Output a table with the results of mini-frac analysis
    Name_en = as.character (
      c("Injected volume, m^3",
        "Fracture half length, m",
        "Average width, m",
        "Fluid efficiency, %",
        "Apparent leakoff coefficient, m/min^(1/2)",
        "Leakoff coefficient in permeable layer, m/min^(1/2)")
    )
    
    Value_en = reactive ({ as.numeric(
      c(2*Vi_en(),
        xf_en(),
        w_en(),
        eta_en()*100,
        CL_en()*60^(1/2),
        CLr_en()*60^(1/2))
    )
    })
    
    Value1_en <- reactive ({
      format(Value_en(),digits = 4)
    })
    
    
    df_result_en <- reactive({
      data.frame(
        Name_en, Value1_en(), stringsAsFactors = FALSE)
    })
    
    output$results_en <- renderTable({
      df_result = df_result_en()
      colnames(df_result)<-c('Parameter','Value')
      df_result
    })
    
    #Comment output
    output$htmlmarkdown_en = reactive({
      note_in_html(input$markdowninput_en)
    })
    
    #Download results in Excel format
    #Excel file style
    hs <- createStyle(
      textDecoration = "BOLD", fontColour = "#000000", fontSize = 12,
      # fontName = "Times New Roman",
      fgFill = "#9ACD32"
    )
    
    #Generating a file for downloading
    output$describe_download_en <- downloadHandler(
      filename = function() {
        paste('Mini-frac analysis results', ".xlsx", sep = "")
      },
      
      content = function(file) {
        df <- df_result_en()
        colnames(df)<-c('Parameter','Value')
        dataset <- list("Mini-frac analysis results" = df)
        write.xlsx(dataset, file, halign='center', colWidths = "auto",borders = "all", fontName = "Times New Roman", headerStyle = hs, firstRow=TRUE, firstCol=TRUE)
      }
    )
    
    #Word report
    output$describe_download_test_en = downloadHandler(
      filename<- function(){
        paste("Mini Frac Report", Sys.Date(), '.docx',sep = " ") #File name
      },
      
      content = function(file) {
        
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       src <- normalizePath('Results_Eng.Rmd')
                       
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'Results_Eng.Rmd', overwrite = TRUE)
                       
                       out <- render('Results_Eng.Rmd', word_document())
                       file.rename(out, file)
                     })
      })
    
###################################РУССКИЙ####################################
    
    #Таблица входных данных для отчета Word
    Name_input <- as.character (
      c("Проницаемая мощность, м",
        "Высота трещины, м",
        "Модуль Юнга, атм",
        "Коэффициент Пуассона",
        "Давление смыкания, атм"
      ))
    
    Value_input <- reactive ({ as.numeric(
      c(input$hp,
        input$hf,
        input$E,
        input$puas,
        input$pc)
    )
    })
    
    Value1_input <- reactive ({
      format(Value_input(), scientific = FALSE)
    })
    
    df_input <- reactive({
      data.frame(
        Name_input, Value1_input(), stringsAsFactors = FALSE)
    })
    
    #Скачать пример файла для загрузки теста мини-грп  
    output$sample_file <- downloadHandler(
      filename <- function() {
        paste("Пример теста мини-ГРП", "xlsx", sep=".")
      },
      content <- function(file) {
        
        file.copy(file.path(getwd(), 'minifrac_test.xlsx'), file,overwrite = TRUE)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    #Распознавание data frame
    df_inject <- reactive ({
      tryCatch(
        {
          req(input$file)
          df_inject <- read_excel(input$file$datapath
          )
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          df_inject <- read_excel ("minifrac_test.xlsx")
        }
      )
    })
    
    #Таблица с данными по мини-ГРП  
    output$contents <- DT::renderDataTable({
      df_inject()
    })
    
    #Построение графика закачки 
    output$injection <- renderPlotly({
      
      df_inject = df_inject()
      # df_inject <-read_excel("minifrac_test.xlsx")
      colnames(df_inject)<-c('t','u', 'p', 'V', 'g')
      
      fig <- plot_ly()
      # Add traces
      fig <- fig %>% add_trace(x = ~df_inject$t, y = ~df_inject$u, name = "Скорость закачки на забое, м3/мин", mode = "lines+markers", type = "scatter", 
                               line = list(color = 'rgb(60, 77, 233)'), marker = list(color = 'rgb(60, 77, 233)'))
      
      m <- list(
        l = 70,
        r = 70,
        b = 50,
        t = 50,
        pad = 10
      )
      
      ay <- list(color = "rgb(233, 16, 16)",
                 overlaying = "y",
                 side = "right",
                 title = list(text ="Забойное давление, атм", standoff = 20))
      
      fig <- fig %>% add_trace(df_inject, x = ~df_inject$t, y = ~df_inject$p, name = "Забойное давление, атм", yaxis = "y2", mode = "lines+markers", type = "scatter",
                               line = list(color = 'rgb(233, 16, 16)'), marker = list(color = 'rgb(233, 16, 16)'))
      
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = "Тест мини-ГРП", yaxis2 = ay,
        xaxis = list(title="Время, мин"),
        yaxis = list(title="Скорость закачки на забое, м3/мин", color ='rgb(60, 77, 233)')
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               margin = m,
               # legend = list(x = 0.01, y = 0.9),
               showlegend = FALSE
        )
      fig 
      
    })
    
    
    #g-функция  
    DLgf1 <- function (din) {
      
      a0 = 1.41495
      a1 = 79.4125
      a2 = 632.457
      a3 = 1293.07
      a4 = 763.19
      a5 = 94.0367
      b0 = 1
      b1 = 54.8534
      b2 = 383.11
      b3 = 540.342
      b4 = 167.741
      b5 = 6.49129
      b6 = -0.0765693
      
      if (din < 15) {
        d = din 
      } else {
        d = 15
      }
      
      gg <- (a0 + a1 * d + a2 * d ^ 2 + a3 * d ^ 3 + a4 * d ^ 4 + a5 * d ^ 5) / (b0 + b1 * d + b2 * d ^ 2 + b3 * d ^ 3 + b4 * d ^ 4 + b5 * d ^ 5 + b6 * d ^ 6)
      
      if (din > 15) {
        gg <- (gg - 2 * (din + 1) ^ 0.5) * exp(15 - din) + 2 * (din + 1) ^ 0.5
      }
      gg
    }
    
    #Прямая линия
    straight <- function (b, m , gg) {
      str <- (b + m * gg)
      str
    } 
    
    #Количество значений из таблицы, включенных в g-функцию  (где 5 столбец в исходной таблице равен 1)
    ng <- reactive ({
      data = df_inject()
      ng = 0
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 5] == 1 ) {    
            ng <- ng + 1
          } 
        }
      }
      ng
    })
    
    #Время (g-функция)
    tt <- reactive ({
      data = df_inject()
      ng = 0
      tt <- c()
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 5] == 1 ) {    
            ng <- ng + 1 
            tt[ng] = as.numeric(data [i, 1]) * 60 
          } 
        }
      }
      tt
    })
    
    #Статическое давление (g-функция)
    pp <- reactive ({
      data = df_inject()
      ng = 0
      pp <- c()
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 5] == 1 ) {    
            ng <- ng + 1
            pp[ng] = as.numeric(data [i, 3])
          } 
        }
      }
      pp
    })
    
    #Время закачки жидкости разрыва
    tni <- reactive ({
      data = df_inject() 
      tni1 = 0
      Vi=0
      bpm = 0.1589873 / 60
      
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 4] == 1) {
            tni <- as.numeric(data [i, 1]) * 60
            qni <- as.numeric(data [i, 2]) / 0.1589 * bpm / 2
            Vi <- Vi + qni * (tni - tni1)
            tni1 = tni
          }
        }
      }
      tni
    })
    
    #Скорость закачки на забое
    qni <- reactive ({
      data = df_inject() 
      tni1 = 0
      Vi=0
      bpm = 0.1589873 / 60
      
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 4] == 1) {
            tni <- as.numeric(data [i, 1]) * 60
            qni <- as.numeric(data [i, 2]) / 60 / 2
            Vi <- Vi + qni * (tni - tni1)
            tni1 = tni
          }
        }
      }
      qni
    })
    
    #Закачанный объем жидкости
    Vi <- reactive ({
      data = df_inject() 
      tni1 = 0
      Vi=0
      
      for (i in 1 : nrow(data))
      {
        if (data [i, 1] >=0) {
          
          if (data [i, 4] == 1) {
            tni <- as.numeric(data [i, 1]) * 60
            qni <- as.numeric(data [i, 2]) / 60 / 2
            Vi <- Vi + qni * (tni - tni1)
            tni1 = tni
          }
        }
      }
      Vi
    })
    
    #Значение g-функции
    gg <- reactive ({
      gg <- c()
      for (i in 1 : ng())
      {
        dt = (tt()[i] - tni())/tni()  
        gg[i] <- DLgf1 (dt) 
        
      }
      gg
    })
    
    #Коэффициент наклона линейного тренда
    mm <- reactive ({
      ng = ng()
      sumxy = 0
      sumx = 0
      sumy = 0
      sumx2 = 0
      
      for (i in 1:ng)
      {
        sumxy = sumxy + gg()[i] * pp()[i]
        sumx = sumx + gg()[i]
        sumy = sumy + pp()[i]
        sumx2 = sumx2 + gg()[i] ^ 2
      }
      mm <-(ng * sumxy - sumx * sumy) / (ng * sumx2 - sumx ^ 2)
      mm
    })
    
    #Коэффициент смещения линейного тренда
    bb <- reactive ({
      ng = ng()
      sumxy = 0
      sumx = 0
      sumy = 0
      sumx2 = 0
      
      for (i in 1:ng)
      {
        sumxy = sumxy + gg()[i] * pp()[i]
        sumx = sumx + gg()[i]
        sumy = sumy + pp()[i]
        sumx2 = sumx2 + gg()[i] ^ 2
      }
      bb <- (sumy - mm() * sumx) / ng
      bb
    })
    
    str <- reactive ({
      str <- c()
      for (i in 1 : ng())
      {
        str[i] <- straight (bb(), mm() , gg()[i])
      }
      str
    })
    
    
    df_g <- reactive ({
      data.frame(
        gg(), pp(), str(), stringsAsFactors = FALSE)
    })
    
    output$g_func_table <- renderTable({
      df=df_g()
      colnames(df)<-c('g-функция','Статическое давление, атм', 'Линейный тренд')
      df
    })
    
    #Построение графика g-функции
    output$g_func_plot <- renderPlotly({
      df = df_g()
      # df_inject <-read_excel("minifrac_test.xlsx")
      colnames(df)<-c('g','p', 'lin')
      
      fig <- plot_ly()
      # Add traces
      fig <- fig %>% add_trace(x = ~df$g, y = ~df$p, name = "Давление, атм", mode = "markers", type = "scatter", 
                               marker = list(color = '#3131E1', size = 7))
      
      
      fig <- fig %>% add_trace(x = ~df$g, y = ~df$lin, name = "Линейный тренд", mode = "lines", type = "scatter",
                               line = list(color = '#27AE60'))
      # 
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        title = "График g-функции", 
        xaxis = list(title="g-функция"),
        yaxis = list(title="Давление, атм", range = c(0, 800))
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
               # legend = list(x = 0.01, y = 0.9),
               showlegend = FALSE
        )
      
      fig  
    })
    
    #Модуль плоской деформации
    Ep <- reactive ({
      input$E / (1 - input$puas^2)
    })
    
    #Полудлина трещины 
    xf <- reactive ({
      2 * Ep() * Vi() / (pi * input$hf ^ 2 * (bb() - input$pc))
    })
    
    #Кажущийся коэффициент утечек
    CL <- reactive ({
      pi * input$hf / (4 * sqrt(tni()) * Ep()) * (-mm())
    })
    
    #Ширина трещины 
    w <- reactive ({
      Vi() / (xf() * input$hf) - 2.83 * CL() * sqrt(tni())
    }) 
    
    #Эффективность жидкости разрыва
    eta <- reactive ({
      w() * xf() * input$hf / Vi()
    }) 
    
    # #Коэффициент мгновенных утечек
    # Sp <- reactive ({
    #   Vi()/(xf()*input$hf) - (bb() - input$pc) * input$hf * pi / (4 * Ep())
    # })
    
    DLrp1 <- function(hp, hf) {
      if (hp <= hf){
        rp <- hp / hf
      } else {
        rp <- 1
      }
    }
    
    rp <- reactive ({ DLrp1 (input$hp, input$hf) })
    
    #Коэффициент утечек в проницаемом слое 
    CLr <- reactive ({
      (1 / rp()) * CL()
    })
    
    psi = 6894.757 
    
    
    #Вывод таблицы с результатами анализа мини-ГРП
    Name = as.character (
      c("Закачанный объем жидкости, м^3", 
        "Полудлина трещины, м",
        "Средняя ширина трещины, м",
        "Эффективность жидкости, %",
        "Кажущийся коэффициент утечек, м/мин^(1/2)",
        "Коэффициент утечек в проницаемом слое, м/мин^(1/2)")
    )
    
    Value = reactive ({ as.numeric(
      c(2*Vi(),
        xf(), 
        w(),
        eta()*100,
        CL()*60^(1/2),
        CLr()*60^(1/2)
      ) )
    })
    
    Value1 <- reactive ({
      format(Value(),digits = 4)
    })
    
    
    df_result <- reactive({
      data.frame(
        Name, Value1(), stringsAsFactors = FALSE)
    })
    
    output$results <- renderTable({
      df_result = df_result()
      colnames(df_result)<-c('Параметр','Значение')
      df_result
    })
    
    #Вывод комментария 
    output$htmlmarkdown = reactive({
      note_in_html(input$markdowninput)
    })
    
    #Скачать результаты в формате Excel
    #Стиль файла Эксель
    hs <- createStyle(
      textDecoration = "BOLD", fontColour = "#000000", fontSize = 12,
      # fontName = "Times New Roman", 
      fgFill = "#9ACD32"
    )
    
    #Формирования файла для скачивания
    output$describe_download <- downloadHandler(
      filename = function() {
        paste('Результаты анализа мини-ГРП', ".xlsx", sep = "")
      },
      
      content = function(file) {
        df <- df_result()
        colnames(df)<-c('Параметр','Значение')
        dataset <- list("Результаты анализа мини-ГРП" = df)
        write.xlsx(dataset, file, halign='center', colWidths = "auto",borders = "all", fontName = "Times New Roman", headerStyle = hs, firstRow=TRUE, firstCol=TRUE)
      }
    )
    
    ###Отчет Word
    output$describe_download_test = downloadHandler(
      filename<- function(){
        paste("Отчет по мини-ГРП", Sys.Date(), '.docx',sep = " ") #Название файла
      },
      
      content = function(file) {
        
        withProgress(message = 'Документ загружается',
                     detail = 'Это может занять некоторое время...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('Results_Ru.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'Results_Ru.Rmd', overwrite = TRUE)
                       
                       out <- render('Results_Ru.Rmd', word_document())
                       file.rename(out, file)
                     })
      })
}

)
