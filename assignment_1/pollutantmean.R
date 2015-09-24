pollutantmean <- function(directory, pollutant, id = 1:332) {
  obs <- do.call(rbind,lapply(list.files(directory,full.names=TRUE),read.csv))
  
  idVector <- obs$ID %in% id
  idRows <- obs[idVector,]
  mean(idRows[[pollutant]],na.rm = TRUE)
}

checkPollute <- function() {
  print(pollutantmean("specdata", "sulfate", 1:10))
  print(pollutantmean("specdata", "nitrate", 70:72))
  print(pollutantmean("specdata", "nitrate", 23))
}