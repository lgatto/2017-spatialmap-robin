source("data.R")

shinyServer(
    function(input, output, session) {
        ranges <- reactiveValues(x = NULL, y = NULL)
        brushBounds <- reactiveValues(i =  try(pcas[, 1] >= min(pcas[, 1]) & 
                                               pcas[, 1] <= max(pcas[, 1])),
                                      j = try(pcas[, 2] >= min(pcas[, 2]) & 
                                              pcas[, 2] <= max(pcas[, 2])))
        resetLabels <- reactiveValues(logical = FALSE)
        
        ## Get coords for proteins according to selectized marker class(es)
        mrkSel <- reactive({
            lapply(input$markers,
                   function(z) which(pmarkers[, z] == 1))
        })
        
        ## Update colour transparacy according to slider input
        myCols <- reactive({
            scales::alpha(cols,
                          input$trans)[sapply(input$markers, function(z) 
                              which(colnames(pmarkers) == z))]
        })
        
        ## PCA plot
        output$pca <- renderPlot({
            par(mar = c(4, 4, 0, 0))
            par(oma = c(1, 0, 0, 0))
                                        # plot(pcas, 
            
            plot2D(object,
                   col = rep(getUnknowncol(), nrow(object)),
                   pch = 21, cex = 1,
                   xlim = ranges$x,
                   ylim = ranges$y,
                   fcol = newName)
            if (!is.null(input$markers)) {
                for (i in 1:length(input$markers)) 
                    points(pcas[mrkSel()[[i]], ], pch = 16, 
                           cex = 1.4, col = myCols()[i])
            } 
            
            ## highlight point on plot by selecting item in table
            idxDT <<- feats[input$fDataTable_rows_selected]
            
            if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
            
            namesIdxDT <<- names(idxDT)
            if (length(idxDT)) {
                highlightOnPlot(object, namesIdxDT, cex = 1.3)
                if (input$checkbox) 
                    highlightOnPlot(object, namesIdxDT, labels = TRUE, pos = 3)
            }
            resetLabels$logical <- FALSE
        })
        
        
        ## Protein profile
        output$profile <- renderPlot({
            par(mar = c(8, 3, 1, 1))
            par(oma = c(0, 0, 0, 0))
            ylim <- range(profs)
            n <- nrow(profs)
            m <- ncol(profs)
            fracs <- sampleNames(object)
            plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity", 
                 type = "n", xaxt = "n", xlab = "")
            axis(1, at = 1:m, labels = fracs, las = 2)
            title(xlab = "Fractions", line = 5.5)
            matlines(t(profs[feats, ]),
                     col = getUnknowncol(),
                     lty = 1,
                     type = "l")
            if (!is.null(input$markers)) {
                for (i in 1:length(input$markers)) { 
                    matlines(t(profs[mrkSel()[[i]], ]),
                             col = myCols()[i],
                             lty = 1,
                             lwd = 1.5) 
                }
            }
            
            ## If an item is clicked in the table highlight profile
            idxDT <<- feats[input$fDataTable_rows_selected]
            
            namesIdxDT <<- names(idxDT)
            if (length(idxDT)) {
                matlines(t(profs[namesIdxDT, , drop = FALSE]),
                         col = "black",
                         lty = 1,
                         lwd = 2)
            }
        })             
        
        
        ## Feature data table
        output$fDataTable <- DT::renderDataTable({
            
            feats <<- which(brushBounds$i & brushBounds$j)
            
            ## Double clicking to identify protein
            
            if (!is.null(input$dblClick)) {
                dist <- apply(pcas, 1, function(z) sqrt((input$dblClick$x - z[1])^2 
                                                        + (input$dblClick$y - z[2])^2))
                idxPlot <- which(dist == min(dist))
                if (idxPlot %in% idxDT) {                          ## 1--is it already clicked?
                    setsel <- setdiff(names(idxDT), names(idxPlot))  ## Yes, remove it from table
                    idxDT <<- idxDT[setsel]
                } else {                                           ## 2--new click?
                    idxDT <<- c(idxDT, idxPlot)                      ## Yes, highlight it to table
                }
            }

            namesIdxDT <<- names(idxDT)
            toSel <- match(namesIdxDT, featureNames(object)[brushBounds$i & brushBounds$j])
            if (resetLabels$logical) toSel <- numeric()

            ## don't display newName - see https://github.com/ComputationalProteomicsUnit/pRolocGUI/issues/52
            dtdata <- fData(object)[, -grep(newName, fvarLabels(object))]
            dtdata <- dtdata[brushBounds$i & brushBounds$j, ]        
            DT::datatable(data = dtdata,
                          rownames = TRUE,
                          selection = list(mode = 'multiple', selected = toSel))
        })
        
        
        ## When a the reset button is clicked check to see is there is a brush on
        ## the plot, if yes zoom, if not reset the plot.
        observeEvent(input$resetButton, {
            brush <- input$pcaBrush
            if (!is.null(brush)) {
                ranges$x <- c(brush$xmin, brush$xmax)
                ranges$y <- c(brush$ymin, brush$ymax)
                brushBounds$i <- pcas[, 1] >= brush$xmin & pcas[, 1] <= brush$xmax
                brushBounds$j <- pcas[, 2] >= brush$ymin & pcas[, 2] <= brush$ymax
            } else {
                ranges$x <- NULL
                ranges$y <- NULL
                brushBounds$i <- try(pcas[, 1] >= min(pcas[, 1]) 
                                     & pcas[, 1] <= max(pcas[, 1]))
                brushBounds$j <- try(pcas[, 2] >= min(pcas[, 2]) 
                                     & pcas[, 2] <= max(pcas[, 2]))
            }
        })
        
        ## When clear selection is pressed update clear idxDT above and reset selection 
        observeEvent(input$clear, {
            resetLabels$logical <- TRUE
        })
        

        ## Output legend
        output$legend1 <- renderPlot({
            par(mar = c(0, 0, 0, 0))
            par(oma = c(0, 0, 0, 0))
            plot(0, type = "n",
                 xaxt = "n", yaxt = "n",
                 xlab = "", ylab = "",
                 bty = "n")
            if (!is.null(input$markers)) {
                legend("topleft",
                       c(input$markers, "unlabelled"),
                       col = c(myCols(), getUnknowncol()),
                       ncol = 1, bty = "n",
                       pch = c(rep(16, length(myCols())), 21),
                       cex = legend.cex)
            } else {
                legend("topleft",
                       "unlabelled",
                       col = getUnknowncol(),
                       ncol = 1, bty = "n",
                       pch = 21,
                       cex = legend.cex)
            }
        })
        ## Output legend
        output$legend2 <- renderPlot({
            par(mar = c(0, 0, 0, 0))
            par(oma = c(0, 0, 0, 0))
            plot(0, type = "n",
                 xaxt = "n", yaxt = "n",
                 xlab = "", ylab = "",
                 bty = "n")
            if (!is.null(input$markers)) {
                legend("topleft",
                       c(input$markers, "unlabelled"),
                       col = c(myCols(), getUnknowncol()),
                       ncol = 1, bty = "n",
                       pch = c(rep(16, length(myCols())), 21),
                       cex = legend.cex
                       )
            } else {
                legend("topleft",
                       "unlabelled",
                       col = getUnknowncol(),
                       ncol = 1, bty = "n",
                       pch = 21,
                       cex = legend.cex)
            }
        })
    })
