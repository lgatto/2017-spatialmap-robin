source("data.R")

shinyUI(fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectizeInput("markers", "Labels",
                           choices = colnames(pmarkers),
                           multiple = TRUE,
                           selected = colnames(pmarkers)[pmsel]),
            sliderInput("trans", "Transparancy",
                        min = 0,  max = 1, value = 0.5),
            checkboxInput("checkbox", label = "Show labels", value = TRUE),
            br(),
            actionButton("resetButton", "Zoom/reset plot"),
            br(),
            actionButton("clear", "Clear selection"),
            width = 2),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Description", id = "descPanel",
                                 includeMarkdown("DESCRIPTION.md")
                                 ),
                        tabPanel("PCA", id = "pcaPanel",
                                 fluidRow(
                                     column(9, 
                                            plotOutput("pca",
                                                       height = fig.height,
                                                       width = fig.width,
                                                       dblclick = "dblClick",
                                                       brush = brushOpts(
                                                           id = "pcaBrush",
                                                           resetOnNew = TRUE)),
                                            offset = 0),
                                     column(3, 
                                            plotOutput("legend1",
                                                       height = fig.height,
                                                       width = legend.width))
                                 )
                                 ),
                        tabPanel("Profiles", id = "profilesPanel",
                                 fluidRow(
                                     column(8,
                                            plotOutput("profile",
                                                       height = "400px",
                                                       width = "120%"),
                                            offset = 0),
                                         
                                     column(3, 
                                            plotOutput("legend2",
                                                       width = "80%"),
                                            offset = 1)
                                 )
                                 ),
                        ## feature data table is always visible
                        fluidRow(
                            column(12,
                                   column(ncol(fData(object)), 
                                          DT::dataTableOutput("fDataTable"))))
                        )
        )
    )
))
