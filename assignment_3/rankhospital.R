rankhospital <- function(state, outcome, num) {
  
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
  outcomeMeasures <- outcomeMeasures[order(outcomeMeasures[,eval(outcomeColumnName)],outcomeMeasures[,"Hospital.Name"],na.last=NA),]
  
    if (num=="worst") {
      num=nrow(outcomeMeasures)
    } else if (num=="best") {
      num=1
    } else if (num > length(outcomeMeasures)) {
      return("NA")
    }
  
  minDeathHospital <- outcomeMeasures[num,"Hospital.Name"]
  
  return(minDeathHospital)
}

testWork <- function () {
  library('testthat')
  expect_output(rankhospital("TX", "heart failure", 4),"DETAR HOSPITAL NAVARRO")
  expect_output(rankhospital("MD", "heart attack", "worst"), "HARFORD MEMORIAL HOSPITAL")
  expect_output(rankhospital("MN", "heart attack", 5000),"NA")
  expect_error(rankhospital("NAA", "heart attack", 5000))
}