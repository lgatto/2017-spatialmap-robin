library(shiny)
library(shinyjs)


shinyUI(fluidPage(
  useShinyjs(),
  

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose a RDS File'),
      textInput("name", "Name"),
      actionButton("uploadButton", "upload")),

    # mainpanel required
    mainPanel(
      
    )
  )
))
