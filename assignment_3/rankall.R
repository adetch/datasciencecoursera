# Take a desired outcome ("heart attack", "heart failure", "pneumonia") and num index
# Return the numth best hospital in each state for the 30 day mortality rate related to the outcome
# num can be numeric, or 'best'/'worst'
rankall <- function(outcome, num="best") {
  # Ingest data
  outcomeMeasures <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  # Map relevant columns as numeric
  outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]<-as.numeric(outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
  outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]<-as.numeric(outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
  outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]<-as.numeric(outcomeMeasures[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])
  # Map state as a factor
  outcomeMeasures[,"State"]<-as.factor(outcomeMeasures[,"State"])
  # Create a character vector of all states in dataset
  states <- levels(outcomeMeasures[,"State"])
  
  # Maps input:output
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
  outcomeColumnName <- as.character(inputMapping[inputMapping$input==outcome,][,"column"])
  
  # Sort by outcomeColumnName, throwing out NAs
  outcomeMeasures <- outcomeMeasures[order(outcomeMeasures[,eval(outcomeColumnName)],outcomeMeasures[,"Hospital.Name"],na.last=NA),]
  
  # Create a blank result set
  nDeathStateHospitals<-data.frame("hospital"=NA,"state"=states)
  rownames(nDeathStateHospitals)<-states
  
  # Split our sorted outcome data by state
  stateData <- split(outcomeMeasures,outcomeMeasures$State)
  
  # Ugly hack. 
  # For each state:
  #   Check the num argument
  #     If worst, set the index to the number of rows for the state in question.
  #     If best, set index to 1
  #     if index is greater than number of hospitals, return NA
  #     else index is as requested
  #   Then populate our result set with the proper hospital name at rank index
  # Include the lookup in the for loop because the 'worst' index can differ per-state
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

# Accepts data frame stateData and returns the number of hospitals for a given state 
# Works because (NAs across all states for the outcome in question) have already been thrown out.
nHospitals<-function(state, stateData) {
  nrow(stateData[[eval(state)]])
}

# Accepts a state argument, list of dataframes by state, num index
# Returns the hospital for the specified state with the numth lowest death rate
# or NA if there the state doesn't have a hospital record at the numth index
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