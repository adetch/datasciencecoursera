best <- function(state, outcome) {
  outcomeMeasures <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]<-as.numeric(outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
  
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
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Pneumonia"
    )
  )
  if(!state %in% states) {
    warning("invalid state")
  }
  if(!outcome %in% inputMapping[,"input"]) {
    warning("invalid outcome")
  }
  outcomeColumnName <- as.character(inputMapping[inputMapping$input==outcome,][,"column"])
  outcomeMeasures <- outcomeMeasures[outcomeMeasures$State == state,]
  outcomeMeasures <- outcomeMeasures[order(eval(outcomeColumnName),"Hospital.Name"),]
  minDeathHospital <- outcomeMeasures[1,"Hospital.Name"]
  
  return(minDeathHospital)
}

