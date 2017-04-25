library(shiny)
library(httr)
library(jsonlite)
library(shinyjs)

shinyServer(function(input, output, session) {

    ## Reactivity not neccessarily needed as its triggered by the action button
    # binarySet <- reactive({
    #   #loading RDS data
    #   return(readBin(load(input$file1$datapath), what = "raw", n = 50000000))
    # })
    # 
    # 
    # inFileName <- reactive({
    #   #dataset name input
    #   input$name
    # })
    
    observeEvent(input$uploadButton, {
      binarySet = readBin(input$file1$datapath, what = "raw", n = 50000000)

      #upload procedure to firebase
      dbURL <- "https://spatialmap-1b08e.firebaseio.com"
      path <- paste0("/objects/", input$name)
      
      base64Set = toJSON(base64_enc(binarySet), raw = "hex")
      
      #adding content to db
      PUT(paste0(dbURL,path,".json"), body = base64Set)
      cat("works")
    })

})
