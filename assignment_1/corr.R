corr <- function(directory, threshold=0) {
  source("./complete.R")
  
  completes<-complete(directory)
  stationsAboveThreshold<-completes[completes$nobs>threshold,]
  obs <- rawData(directory)
  
  observationsAboveThresholdVector <- obs$ID %in% stationsAboveThreshold$id
  observationsAboveThreshold <- obs[observationsAboveThresholdVector,]
  
  corrList <- c()
  for(id in stationsAboveThreshold$id) {
    currentObservations <- observationsAboveThreshold[observationsAboveThreshold$ID==id,]
    newCor <- cor(currentObservations$sulfate, currentObservations$nitrate)
    corrList <- c(corrList, newCor)
  }
  return(corrList)
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}

checkCorr <- function() {
  cr <- corr("specdata", 150)
  print(head(cr))
  print(summary(cr))
  cr <- corr("specdata", 400)
  print(head(cr))
  print(summary(cr))
  cr <- corr("specdata", 5000)
  print(summary(cr))
  length(cr)
  cr <- corr("specdata")
  print(summary(cr))
  print(length(cr))
}