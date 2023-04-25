
# load packages needed
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(plotly)
#library(shinyw)
library(leaflet)
library(leaflet.extras)
library(readxl)

############ Connexion postrgreSQL ##############
library(RPostgreSQL)
library(RPostgres)
library(DBI)
library(RODBC)
library(odbc)
library(shinyFiles)
library(openxlsx)
conc<- dbConnect(RPostgres::Postgres(),
                 dbname = "postgres",
                 port = 5432,
                 user = "postgres",
                 password = "090999")
#################################################
df <- read.csv('data/bank_sales.csv', stringsAsFactors = F, header = T)

ui <- dashboardPage(
  
      dashboardHeader(title = 'Ratios réglementaires'),

      dashboardSidebar(
        sidebarMenu(
          menuItem('Ratios de liquidité', tabName = 'dashboard', icon = icon('dashboard')),
          menuItem('Ratios de solvabilité', icon = icon('github'))
        )
      ),
      dashboardBody(
        
        fluidRow(
          column(4,
          #useShinyFiles(),
          pickerInput(
            inputId = "doc_type",
            label = "Documents realatifs au :", 
            choices = c("LCR", "NSFR", "LDR"),
            options = list(
              style = "btn-primary")
          )),
          column(4,
                 titlePanel("Download related documents"),
                   downloadButton("downloadData", "Download documents")
          ),
          column(4,
                 titlePanel("Upload related documents"),
                 sidebarLayout(
                   sidebarPanel(
                     fileInput("file", "Choose Excel file")
                   ),
                   mainPanel(
                     tableOutput("data")
                   )
                 )
                 
          )
          
        ),
        fluidRow(
              #selection date 
          column(4,  airDatepickerInput(inputId = "myDate", label = "Select a Year and Month",
                                      view = "years", minView = "months",
                                      dateFormat = "yyyy-MM-dd" ,clearButton = TRUE,
                                      autoClose = TRUE),
               
               #Display date
               #textOutput("output")
              )
        ),
      
        fluidRow(
          valueBoxOutput('value1'),
          valueBoxOutput('value2'),
          valueBoxOutput('value3')
        ),
  
        fluidRow(
          box(
            title = 'Revenue per Product & Region',
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput('revenueByPrd', height = '300px')),
          box(
            title = 'Total Revenue by Bank',
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput('revenueByBnk', height = '300px'))
        ),
      
        bsModal("modal1", "Détails du Liquidity Coverage Ratio",
              trigger = "value1",
              size = "large",
              textOutput("modal_text1"),
              
              
              fluidRow(
                column(6, plotlyOutput("modal_plot2")),
                column(3, 
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           title ="Entrées de trésorerie",
                           textOutput("Entrees_de_tresorerie"))),
                column(3,
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           title = "Sorties de trésorerie",
                           textOutput("Sorties_de_tresorerie"))),
              )),
      
      
        bsModal("modal3", "Détails du Net Stable Funding Ratio",
              trigger = "value3",
              size = "large",
              textOutput("modal_text3"),
              
              fluidRow(
                column(6, 
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           title ="Financement stable disponible",
                           textOutput("ASF"))),
                column(6,
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           title = "Financement stable requis ",
                           textOutput("RSF"))),
                
                
              )
        ),
        bsModal("modal2", "Détails du Loan-to-Deposit Ratio",
              trigger = "value2",
              size = "large",
              textOutput("modal_text2"),
              
              fluidRow(
                column(6, 
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           title ="Crédits",
                           textOutput("Credits"))),
                column(6,
                       box(width = 12, solidHeader = TRUE, status = "primary",
                           title = "Dépôts",
                           textOutput("Depots"))),
              )
        ),
      
      
        fluidRow(
          titlePanel("LCR Value"),
          mainPanel(
          plotOutput("chart")
          )
        ),
      
      
      
      
     skin = 'blue'
    ))

server <- function(input, output) {
  values<-reactiveValues()
  values$LCR_selectionne <- NULL
  values$LCR_calcule <- NULL
  #########################################################################################################################################  
  # Supposons que vous avez une fonction appelée calcul_LCR qui calcule le LCR pour un mois donné
  # Supposons également que vous avez un fichier CSV appelé "resultats_LCR.csv" qui contient les résultats précédemment calculés
  
  # Définir le mois pour lequel vous voulez calculer le LCR
  output$value1 <-  renderValueBox({valueBox(
      formatC("Selectionner le mois" , format = 'd', big.mark = ',') ,
      paste('Valeur du LCR'),
      icon = icon('stats', lib ='glyphicon'),
      color = 'purple')
  })
  
    
  
   observeEvent(input$myDate,{
     print(input$myDate)
     if(!is.null(input$myDate)){
       values$selecteddate<-input$myDate
       print(values$selecteddate)
     }
   })

  
  # compute KPI values on revenue/sales
  total_rev <- sum(df$Revenue)
  sales_act <- df %>% group_by(Account) %>% summarize(value = sum(Revenue)) %>%
    filter(value==max(value))
  top_prod <- df %>% group_by(Product) %>% summarize(value = sum(Revenue)) %>%
    filter(value==max(value))
  
  # Set work directory
  setwd("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios sur R/LCR")
  # Import LCR
  source('LCR.R')
  # create the KPI valueBox output
  # Import LDR
  source('LDR.R')
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(LDR$Value, format ="d", big.mark =',') %>% paste0(" %")
      ,'Valeur du LDR'
      ,icon = icon("usd",lib ='glyphicon')
      ,color = "green")  
  })
  
  
  # Import NSFR
  source('NSFR.R')
  output$value3 <- renderValueBox({
    valueBox(
      formatC(NSFR$Value, format ="d", big.mark =',') %>% paste0(" %")
      ,paste('Valeur du NSFR')
      ,icon = icon("menu-hamburger",lib ='glyphicon')
      ,color = "orange")   
  })
  
  # create revenue plots by product/region
  output$revenueByPrd <- renderPlot({
    ggplot(data = df, aes(x = Product, y = Revenue, fill = factor(Region))) + 
      geom_bar(position = 'dodge', stat = 'identity') + 
      ylab('Revenue ($USD)') +
      xlab('Product') +
      theme(legend.position = 'bottom', 
            plot.title = element_text(size = 15, face = 'bold')) +
      ggtitle('Revenue by Product') + labs(fill = 'Region')
    })
  output$revenueByBnk <- renderPlot({
    ggplot(data = df, aes(x = Account, y = Revenue)) +
      geom_bar(stat = 'identity', fill = 'gold') + 
      ylab('Revenue ($USD)') +
      xlab('Account') +
      coord_flip() +
      theme(legend.position = 'bottom',
            plot.title = element_text(size = 15, face = 'bold'))
  })
  
  
  output$modal_text1 <- renderText({
    "High Quality Liquid Assets (HQLA)"
  })
  
  output$modal_text2 <- renderText({
    ""
  })
  
  output$modal_text3 <- renderText({
    ""
  })
  
  output$modal_text4 <- renderText({
    "This is popup box 4!"
  })
  
  

  #values$Sorties <- S
  #values$Entrees
  
  
  output$modal_plot2 <- renderPlotly({
    pie_data <- data.frame(
      category = c("Niveau 1", "Niveau 2A", "Niveau 2B"),
      #value = c(LCR$A1, LCR$A2A,LCR$A2B )
      value = c(values$HQLA1, values$HQLA2A,values$HQLA2B )
    )
    plot_ly(pie_data, labels = ~category, values = ~value, type = "pie")
  })
  
  output$Entrees_de_tresorerie <- renderText({
    values$Entrees
  })
  
  output$Sorties_de_tresorerie <- renderText({
    values$Sorties
  })
  
  output$ASF <- renderText({
    NSFR$asf
  })
  
  output$RSF <- renderText({
    NSFR$nsfd
  })
  
  output$Credits <- renderText({
    LDR$credits
  })
  
  output$Depots <- renderText({
    LDR$depots
  })
  #################################################################################################################################
  
  setwd("C:\\Users\\medhc\\OneDrive\\Documents\\GitHub\\bank-revenue-dashboard")
  source("server_uploadData.R")
  server_uploadData(input,output,session)
  source("server_downloadData.R")
  server_downloadData(input,output,session)
  
  #################################################################################################################################
  observe({
    if(!is.null(input$myDate)){
  #HQLA1
      #browser()
  dateSelectionnee <- input$myDate
  #dateSelectionnee <- as.Date("2022-09-01")
  HQLA1 <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Niveau 1 HQLA")
  #View(HQLA1)
  filtered_df <- HQLA1 %>% filter(as.Date(Date) == dateSelectionnee)
  #class(filtered_df$`Total HQLA 1`[1])
  #View(filtered_df$`Total HQLA 1`[1])
  H1 <-filtered_df$`Total HQLA 1`[1]
  #HQLA1 <- as.numeric(H1)
  
  #HQLA2A
  #dateSelectionnee <- as.Date("2022-09-01")
  dateSelectionnee <- input$myDate
  HQLA2A <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Niveau 2A HQLA")
  filtered_df <- HQLA2A %>% filter(as.Date(Date) == dateSelectionnee)
  H2A <- filtered_df$`Total HQLA 2A`[1]
  
  #HQLA2B
  dateSelectionnee <- input$myDate
  HQLA2B <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Niveau 2B HQLA")
  filtered_df <- HQLA2B %>% filter(as.Date(Date) == dateSelectionnee)
  H2B <- filtered_df$`Total HQLA 2B`[1] 

  #Sorties
  dateSelectionnee <- input$myDate
  Sorties <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Sorties de tresorerie")
  filtered_df <- Sorties %>% filter(as.Date(Date) == dateSelectionnee)
  S <- filtered_df$`Total sorties`[1]

  #Entrees
  dateSelectionnee <- input$myDate
  Entrees <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Entrees de tresorerie")
  #View(HQLA1)
  filtered_df <- Entrees %>% filter(as.Date(Date) == dateSelectionnee)
  ##View(filtered_df$`Total entrees`[1])
  E <- filtered_df$`Total entrees`[1]
  #values$LCR_calcule <- c(1)
  values$LCR_calcule<-as.integer(((H1+H2A+H2B)/(S-E))*100)
  
  #########################sauvegarde de la nouvelle valeur du LCR dans la base de données############################################
  #browser()
  value<- values$LCR_calcule
  sql_statement_historiquelcr <- paste0("INSERT INTO historiquelcr (lcr, dt) VALUES (", value, ", '", dateSelectionnee, "')")
  dbSendQuery(conc, sql_statement_historiquelcr)
  
  #browser()
  recuperation_pk <- paste0("SELECT pk FROM historiquelcr WHERE dt = '", dateSelectionnee, "'")
  pk <- dbGetQuery(conc, recuperation_pk)
  pk[1]
  
  sql_statement_proprietes_lcr <- paste0("INSERT INTO proprietes_lcr (pk, hqla1, hqla2a, hqla2b, sorties, entrees) VALUES 
                                         (",pk[1],",", H1,",", H2A,",", H2B,",", S, ",", E,")")
  dbSendQuery(conc, sql_statement_proprietes_lcr)
  values$HQLA1 <- H1
  values$HQLA2A <- H2A
  values$HQLA2B <- H2B
  values$Sorties <- S
  values$Entrees <- E
  #################################################################################################################################
  resultats_LCR_df <- dbGetQuery(conc, "SELECT * FROM historiquelcr")
  observe({
    if(!is.null(values$selecteddate)){
      if(values$selecteddate %in% resultats_LCR_df$dt){
        index <- which(values$selecteddate %in% resultats_LCR_df$dt)
        values$LCR_selectionne<-resultats_LCR_df$lcr[index]
      }
      else if (!is.null(values$LCR_calcule)){
        #browser()
        values$LCR_selectionne<-values$LCR_calcule
      }
      else print("Error detected")
    }
  })
  observeEvent(values$LCR_selectionne,{
    if(!is.null(values$LCR_selectionne)){
      output$value1 <- renderValueBox({
        #browser() 
        valueBox(
          formatC(toString(values$LCR_selectionne) , format = 'd', big.mark = ',') %>% paste0(" %"),
          paste('Valeur du LCR'),
          icon = icon('stats', lib ='glyphicon'),
          color = 'purple'
        )
      })
    }
  })
  #################################################################################################################################
  
  
  
  
  
  #LCR GRAPHE
  LCRV <- LCR$Value
  output$chart <- renderPlot({
      ggplot(data.frame(LCRV), aes(x = 1, y = LCRV)) +
      geom_point(size = 3) +
      geom_text(aes(label = LCRV), hjust = -0.1, vjust = 0.5) +
      ylim(c(0, max(LCR, 100))) +
      labs(x = "", y = "LCR Value") +
      theme_bw()
  })
    }
  })
}

shinyApp(ui = ui, server = server)
