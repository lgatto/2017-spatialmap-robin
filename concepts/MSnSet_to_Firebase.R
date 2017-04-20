#load packages
library(httr)
library(jsonlite)
library(pRolocdata)

#project settings
dbURL <- "https://firedata-b0e54.firebaseio.com"
path <- "/MSnBase/6"

#taking pRolocData MSnSet
data(E14TG2aS1)
tempPath = tempfile()
saveRDS(E14TG2aS1, tempPath)
binarySet = readBin(tempPath, what = "raw", n=500000)
base64Set = toJSON(base64_enc(binarySet),raw = "hex")

#adding content
PUT(paste0(dbURL,path,".json"), body = base64Set)

#retrieving data
data = GET(paste0(dbURL,path,".json"))
retrievedData = content(data,"text")
tempPath2 = tempfile()
writeBin(base64_dec(fromJSON(retrievedData)), tempPath2)
readRDS(tempPath2)
