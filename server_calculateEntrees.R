server_calculateEntrees<-function(input,output,session){
  output$data <- renderTable({
    dateSelectionnee <- input$myDate
    Entrees <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Entrees de tresorerie")
    #View(HQLA1)
    filtered_df <- Entrees %>% filter(as.Date(Date) == dateSelectionnee)
    ##View(filtered_df$`Total entrees`[1])
    E <- filtered_df$`Total entrees`[1]
    return(as.numeric(E))
  })
}
