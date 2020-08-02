
# load packages needed
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

df <- read.csv('data/bank_sales.csv', stringsAsFactors = F, header = T)

ui <- dashboardPage(
    dashboardHeader(title = 'Bank Revenue'),
    dashboardSidebar(
      sidebarMenu(
        menuItem('Sales Analysis', tabName = 'dashboard', icon = icon('dashboard')),
        menuItem('GitHub', icon = icon('github'),
                 href = 'https://github.com/bicachu/bank-revenue-dashboard/')
      )
    ),
    dashboardBody(
      fluidRow(
        valueBoxOutput('value1'),
        valueBoxOutput('value2'),
        valueBoxOutput('value3')),
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
        )
      ),
    skin = 'blue'
    )

server <- function(input, output) {
  # compute KPI values on revenue/sales
  total_rev <- sum(df$Revenue)
  sales_act <- df %>% group_by(Account) %>% summarize(value = sum(Revenue)) %>%
    filter(value==max(value))
  top_prod <- df %>% group_by(Product) %>% summarize(value = sum(Revenue)) %>%
    filter(value==max(value))
  
  # create the KPI valueBox output
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales_act$value, format = 'd', big.mark = ','),
      paste('Top Account:', sales_act$Account),
      icon = icon('stats', lib ='glyphicon'),
      color = 'purple')
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(total_rev, format ="d", big.mark =',')
      ,'Total Expected Revenue'
      ,icon = icon("usd",lib ='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(top_prod$value, format ="d", big.mark =',')
      ,paste('Top Product:',top_prod$Product)
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
}

shinyApp(ui = ui, server = server)
