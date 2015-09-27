rankall <- function(outcome, num="best") {
  
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
  if(!outcome %in% inputMapping[,"input"]) {
    stop("invalid outcome")
  }
  outcomeColumnName <- as.character(inputMapping[inputMapping$input==outcome,][,"column"])
  outcomeMeasures <- outcomeMeasures[order(outcomeMeasures[,eval(outcomeColumnName)],outcomeMeasures[,"Hospital.Name"],na.last=NA),]
  
  nDeathStateHospitals<-data.frame("hospital"=NA,"state"=states)
  rownames(nDeathStateHospitals)<-states
  stateData <- split(outcomeMeasures,outcomeMeasures$State)
  
  
  
  for (eachState in states) {
    if (num=="worst") {
      index <- nHospitals(eachState, stateData)
    } else if (num=="best") {
      index <- 1
    } else if (num > length(outcomeMeasures)) {
      return("NA")
    } else {
      index <- num
    }
    nDeathStateHospitals[eachState,"hospital"] <- nDeathHospital(eachState,stateData,index)
  }

  return(nDeathStateHospitals)
}

nHospitals<-function(state, stateData) {
  nrow(stateData[[eval(state)]])
}
nDeathHospital<-function(state, stateData, num) {
  nthDeathHospital <- tryCatch(
    {
      hospitalRow <- stateData[[eval(state)]][num,]
      hospitalRow$Hospital.Name
    }, 
    error=function(cond) {
      NA
    }
  )
  return(nthDeathHospital)
}