server_calculateHQLA2B<-function(input,output,session){
  output$data <- renderTable({
    dateSelectionnee <- input$myDate
    HQLA2B <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Niveau 2B HQLA")
    #View(HQLA2A)
    filtered_df <- HQLA2B %>% filter(as.Date(Date) == dateSelectionnee)
    #View(filtered_df$`Total HQLA 2B`[1])
    H2A <- filtered_df$`Total HQLA 2B`[1]
    return(as.numeric(H2A))
  })
}