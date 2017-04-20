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

# uploading all pRolocDatasets
for(i in 1:length(a)){
  a[i] = head(strsplit(a[i], split=" ")[[1]],1)
}

b =  a[nchar(a) > 2]

#uploading all pRolocData MSnSets
for(i in b){
path <- paste0("/pRolocData/", i)
data(list = i)
tempPath = tempfile()
saveRDS(eval(as.name(i)) , file = tempPath)
binarySet = readBin(tempPath, what = "raw", n=500000)
base64Set = toJSON(base64_enc(binarySet), raw = "hex")

#adding content
PUT(paste0(dbURL,path,".json"), body = base64Set)

print(paste0(i, " is ready"))
}

pRolocFire <- function(dataset){
  dbURL <- "https://firedata-b0e54.firebaseio.com"
  path <- paste0("/pRolocData/", dataset)
  #retrieving data
  data = GET(paste0(dbURL,path,".json"))
  retrievedData = content(data,"text")
  tempPath2 = tempfile()
  writeBin(base64_dec(fromJSON(retrievedData)), tempPath2)
  readRDS(tempPath2)
}
