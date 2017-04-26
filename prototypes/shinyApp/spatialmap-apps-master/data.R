## requires DT 0.1.40 from github
## version 0.1 from CRAN fails

library("shiny")
library("MSnbase")
library("pRoloc")
library("DT")

## from devel pRoloc
highlightOnPlot <- function(object, foi, labels, args = list(), ...) {
    if (is.character(foi))
        foi <- FeaturesOfInterest(description = "internally created",
                                  fnames = foi)
    stopifnot(inherits(foi, "FeaturesOfInterest"))
    
    if (!fnamesIn(foi, object)) {
        warning("None of the features of interest are present in the data.")
        return(invisible(NULL))
    }
    if (inherits(object, "MSnSet")) {
        .args <- list(object = object, plot = FALSE)
        args <- c(args, .args)
        .pca <- do.call(plot2D, args = args)
        sel <- featureNames(object) %in% foi(foi)
    } else if (is.matrix(object)) {
        .pca <- object
        sel <- rownames(object) %in% foi(foi)
    } else {
        stop("'object' must be a matrix (as returned by plot2D) or an MSnSet.")
    }
    if (!is.null(args$mirrorX) && args$mirrorX)
        .pca[, 1] <- -.pca[, 1]
    if (!is.null(args$mirrorY) && args$mirrorY)
        .pca[, 2] <- -.pca[, 2]
    if (!missing(labels)) {
        if (is.character(labels)) {
            stopifnot(inherits(object, "MSnSet"))
            labels <- labels[1]
            stopifnot(labels %in% fvarLabels(object))
            labels <- fData(object)[, labels]
        } else if (isTRUE(labels)) {
            if (inherits(object, "MSnSet"))
                labels <- featureNames(object)
            else labels <- rownames(object) ## a matrix
        } else {
            stop("'labels' must be a character or logical of length 1.")
        }
        text(.pca[sel, 1], .pca[sel, 2], labels[sel], ...)
    } else {
        points(.pca[sel, 1], .pca[sel, 2], ...)
    }
}

## from devel pRolocGUI
narrowFeatureData <- function (object, n1 = 6, n2 = 6, fcol = "markers") {
    if (length(fcol) > 1) 
        fcol <- NULL
    if (is.null(fcol)) {
        i <- selectFeatureData(object)
        fData(object) <- fData(object)[, i]
        return(object)
    }
    n <- n1 + n2
    fv <- fvarLabels(object)
    ln <- length(fv)
    if (ln <= n) 
        return(object)
    i <- c(1:n1, (ln - n2 + 1):ln)
    if (any(fcol %in% fv) & !any(fcol %in% fv[i])) 
        i <- c(1:n1, (ln - n2 + 2):ln, match(fcol, fv))
    fData(object) <- fData(object)[, i]
    if (validObject(object)) 
        return(object)
}

pRolocFire <- function(dataset){
  dbURL <- "https://spatialmap-1b08e.firebaseio.com"
  path <- paste0("/objects/", dataset)
  #retrieving data
  data = GET(paste0(dbURL,path,".json"))
  retrievedData = httr::content(data,"text")
  tempPath2 = tempfile()
  writeBin(base64_dec(fromJSON(retrievedData)), tempPath2)
  x <- readRDS(tempPath2)
  assign(toString(as.name("object")), x, envir = .GlobalEnv)
  return(paste0(dataset, " was transfered"))
}

pRolocFire("hyperLOPIT2015")

load("fois.rda")

fcol <- "markers"
mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
object <- mrkVecToMat(object, fcol, mfcol = mName)
fcol <- mName
pmarkers <- fData(object)[, fcol]

newName <- paste0(format(Sys.time(), "%a%b%d%H%M%S%Y"), "markers")
fData(object)[, newName] <- "unknown"

pmsel <- 1:ncol(pmarkers) ## on startup, only display organelles, not fois
foimarkers <- as(fois, "matrix")
pmarkers <- merge(pmarkers, foimarkers,
                  by = 0, all.x = TRUE)
pmarkers[is.na(pmarkers)] <- 0
rownames(pmarkers) <- pmarkers[, "Row.names"]
pmarkers <- pmarkers[featureNames(object), -1]

setLisacol()
setUnknowncol("grey")
cols <- getStockcol()

fData(object) <- fData(object)[, -grep(fcol, fvarLabels(object))]

object <- narrowFeatureData(object)

pcas <- plot2D(object, fcol = NULL, plot = FALSE)
profs <- exprs(object)

feats <- toSel <- 1:nrow(object)
idxDT <- numeric()
namesIdxDT <- character()

fig.height <- "600px"
fig.width <- "100%"
legend.width <- "200%"
legend.cex <- 1