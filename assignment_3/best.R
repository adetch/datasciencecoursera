best <- function(state, outcome) {
  outcomeMeasures <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]<-as.numeric(outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
  outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]<-as.numeric(outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
  outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]<-as.numeric(outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])
  
  outcomeMeasures[,"State"]<-as.factor(outcomeMeasures[,"State"])
  states <- levels(outcomeMeasures[,"State"])
  inputMapping <- data.frame(
    input=c(
      "heart attack",
      "heart failure",
      "pneumonia"
    ),
    column=c(
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
      "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )
  )
  if(!state %in% states) {
    stop("invalid state")
  }
  if(!outcome %in% inputMapping[,"input"]) {
    stop("invalid outcome")
  }
  outcomeColumnName <- as.character(inputMapping[inputMapping$input==outcome,][,"column"])
  outcomeMeasures <- outcomeMeasures[outcomeMeasures$State == state,]
  outcomeMeasures <- outcomeMeasures[order(outcomeMeasures[,eval(outcomeColumnName)],outcomeMeasures[,"Hospital.Name"]),]
  minDeathHospital <- outcomeMeasures[1,"Hospital.Name"]
  
  return(minDeathHospital)
}

testWork <- function() {
  print(best("GA","heart attack"))
  print(best("GA","heart failure"))
  print(best("GA","pneumonia"))
}

