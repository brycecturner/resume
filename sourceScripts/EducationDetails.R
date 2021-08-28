setClass("educationDetails",
         slots = list(university = "character",
                      degree = "character",
                      minor = "character",
                      location = "character",
                      graduationDate = "numeric"))


setMethod("show", "educationDetails",
          function(object){
            cat(object@university)
            cat("\n")
            cat(object@degree)
            cat("\n")
            cat(object@minor)
            cat("\n")
            cat(object@location)
            cat("\n")
            cat(object@graduationDate)
            cat("\n")
            
          })


createEducationDetailFromExcel <- function(dataframe, rowNum){

  usingRow <- dataframe[rowNum,]

  newInstance <- 
    new("educationDetails",
        university = usingRow$SchoolName,
        degree = usingRow$DegreeTitle,
        minor = usingRow$SecondaryTitle,
        location = paste0(usingRow$City, ", ", usingRow$State),
        graduationDate = usingRow$EndDate)
  
  return(newInstance)

}



renderEducationDetailsForResume <- function(educationDetailsObject){
  
  minorDetail <- 
   ifelse(is.na(educationDetailsObject@minor), 
          "", 
          paste(",", educationDetailsObject@minor))
  
  endDateDetail <- 
    ifelse(is.na(educationDetailsObject@graduationDate), 
           "In Progress", 
           educationDetailsObject@graduationDate)
  
  
  outString <- paste0("### ", educationDetailsObject@degree, minorDetail,
                      "\n \n", 
                      educationDetailsObject@university,
                      "\n \n", 
                      educationDetailsObject@location, "\n \n",
                      endDateDetail)
  
  return(outString)
    
}