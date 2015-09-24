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

checkComplete <- function() {
  complete("specdata",c(1:43))
}