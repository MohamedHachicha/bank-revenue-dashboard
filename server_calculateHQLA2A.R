server_calculateHQLA2A<-function(input,output,session){
  output$data <- renderTable({
    # dateSelectionnee <- input$myDate
    dateSelectionnee <- as.Date("2022-09-01")
    HQLA2A <- read_excel("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LCR1.xlsx",sheet = "Niveau 2A HQLA")
    #View(HQLA2A)
    filtered_df <- HQLA2A %>% filter(as.Date(Date) == dateSelectionnee)
    #View(filtered_df$`Total HQLA 2A`[1])
    H2A <- filtered_df$`Total HQLA 2A`[1]
    class(H2A)
    View(filtered_df$`Total HQLA 2A`[1])
    return(as.numeric(H2A))
  })
}