server_uploadData<-function(input,output,session){
  #Exceldata
  #upload data
  output$data <- renderTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    # define your destination path here:
    dest_path <- "C:/Users/medhc/OneDrive - QUANTYLIX/Excels"
    file.copy(inFile$datapath, file.path(dest_path, inFile$name), overwrite = TRUE)
    data <- read_excel(inFile$datapath, sheet = 1)
    data
  })
}