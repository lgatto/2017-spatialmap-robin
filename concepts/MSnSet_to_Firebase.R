#load packages
library(httr)
library(jsonlite)
library(pRolocdata)

#project settings
dbURL <- "https://spatialmap-1b08e.firebaseio.com"
path <- "/objects"

#taking pRolocData MSnSet
data(E14TG2aS1)
tempPath = tempfile()
saveRDS(E14TG2aS1, tempPath)
binarySet = readBin(tempPath, what = "raw", n = 5000000)
base64Set = toJSON(base64_enc(binarySet),raw = "hex")

#adding content
PUT(paste0(dbURL,path,".json"), body = base64Set)

#retrieving data
data = GET(paste0(dbURL,path,".json"))
retrievedData = content(data,"text")
tempPath2 = tempfile()
writeBin(base64_dec(fromJSON(retrievedData)), tempPath2)
readRDS(tempPath2)

# uploading all pRolocDatasets
for (i in 1:length(a)){
  a[i] = head(strsplit(a[i], split = " ")[[1]],1)
}

b = a[nchar(a) > 2]

#uploading all pRolocData MSnSets
for (i in b) {
path <- paste0("/objects/", i)
data(list = i)
tempPath = tempfile()
saveRDS(eval(as.name(i)) , file = tempPath)
binarySet = readBin(tempPath, what = "raw", n = 50000000)
base64Set = toJSON(base64_enc(binarySet), raw = "hex")

#adding content
PUT(paste0(dbURL,path,".json"), body = base64Set)
print(paste0(i, " is ready"))
}

#retrieving datasets from firebase
pRolocFire <- function(dataset){
  dbURL <- "https://spatialmap-1b08e.firebaseio.com"
  path <- paste0("/objects/", dataset)
  #retrieving data
  data = GET(paste0(dbURL,path,".json"))
  retrievedData = content(data,"text")
  tempPath2 = tempfile()
  writeBin(base64_dec(fromJSON(retrievedData)), tempPath2)
  x <- readRDS(tempPath2)
  assign(toString(as.name(dataset)), x, envir = .GlobalEnv)
  return(paste0(dataset, " was transfered"))
}

#testing datasets functionality, b is a list of all MSnBase names
lapply(b, function(x) tryCatch(pRolocFire(x), error = function(e) NULL))

##
firebaseQuality <- function(dName) {
  #pRolocData
  data(list = dName)
  pRolocDataSet = eval(as.name(dName))
  tempRoloc = tempfile()
  saveRDS(pRolocDataSet, tempRoloc)
  #Firebase Data
  dbURL <- "https://spatialmap-1b08e.firebaseio.com"
  path <- paste0("/objects/", dName)
  data = GET(paste0(dbURL,path,".json"))
  retrievedData = content(data,"text")
  tempFire = tempfile()
  writeBin(base64_dec(fromJSON(retrievedData)), tempFire)
  
  #comparing both objects
  return(identical(toString(tools::md5sum(tempRoloc)), toString(tools::md5sum(tempFire))))
}

## testing the whole DB, b is a vector of all MSnBase names
lapply(b, function(x) tryCatch(firebaseQuality(x), error = function(e) NULL))

