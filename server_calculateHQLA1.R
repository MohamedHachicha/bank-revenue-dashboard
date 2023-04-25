server_calculateHQLA1<-function(input,output,session){
  output$data <- renderTable({
    dateSelectionnee <- input$myDate
    #dateSelectionnee <- as.Date("2022-09-01")
    HQLA1 <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Niveau 1 HQLA")
    #View(HQLA1)
    filtered_df <- HQLA1 %>% filter(as.Date(Date) == dateSelectionnee)
    class(filtered_df$`Total HQLA 1`[1])
    #View(filtered_df$`Total HQLA 1`[1])
    H1 <-filtered_df$`Total HQLA 1`[1]
    #class(as.numeric(H1))
    return(as.numeric(H1))
  })
}