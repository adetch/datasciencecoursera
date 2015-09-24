complete <- function(directory, id = 1:332) {
  obs <- data.frame(do.call(rbind,lapply(list.files(directory,full.names=TRUE),read.csv)))
  
  idVector <- obs$ID %in% id
  idRows <- obs[idVector,]
  completeVector <- complete.cases(idRows$sulfate, idRows$nitrate)
  
  completeRows <- idRows[completeVector,]
  freq <- data.frame(table(completeRows$ID))
  colnames(freq) <- c("id","nobs")
  freq
}

rawData <- function(directory, id = 1:332) {
  obs <- data.frame(do.call(rbind,lapply(list.files(directory,full.names=TRUE),read.csv)))
  idVector <- obs$ID %in% id
  idRows <- obs[idVector,]
  completeVector <- complete.cases(idRows$sulfate, idRows$nitrate)
  
  idRows[completeVector,]
}

checkComplete <- function() {
  #print(complete("specdata", 1))
  #print(complete("specdata", c(2, 4, 8, 10, 12)))
  print(complete("specdata", 30:25))
  #print(complete("specdata", 3))
}