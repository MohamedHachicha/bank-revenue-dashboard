server_calculateSorties<-function(input,output,session){
  output$data <- renderTable({
    dateSelectionnee <- input$myDate
    Sorties <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Sorties de tresorerie")
    #View(Sorties)
    filtered_df <- Sorties %>% filter(as.Date(Date) == dateSelectionnee)
    #View(filtered_df$`Total HQLA 1`[1])
    S <- filtered_df$`Total sorties`[1]
    return(as.numeric(S))
  })
}