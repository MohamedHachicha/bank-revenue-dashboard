
server_downloadData<-function(input,output,session){
#download data
output$downloadData <- downloadHandler(
  filename = function() {
    if (input$doc_type == "LCR") {
    "LCR.xlsx"
    }
    else if (input$doc_type == "NSFR") {
     "NSFR.xlsx" 
    }
    else if (input$doc_type == "LDR") {
      "LDR.xlsx"
    }
  },
  content = function(file) {
    if (input$doc_type == "LCR") {
      file.copy("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/Template LCR.xlsx", file)
    }
    else if (input$doc_type == "NSFR") {
      file.copy("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/NSFR.xlsx", file)  
    }
    else if (input$doc_type == "LDR") {
      file.copy("C:/Users/medhc/OneDrive - QUANTYLIX/Ratios/LDR.xlsx", file)  
    }
  }
)
}