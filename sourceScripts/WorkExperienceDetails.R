setClass("workExperienceDetails",
         slots = list(experienceId = "numeric",
                      companyName_full = "character",
                      companyName_short = "character",
                      companySubtitle = "character",
                      positionTitle = "character",
                      startDate = "numeric",
                      endDate = "numeric",
                      city = "character",
                      state = "character",
                      experienceBullets = "character"))


renderBulletsForResume <- function(bulletsVector){
  
  bullets <- paste(bulletsVector, collapse = "\n- ")
  
  finalForRender <- paste0(":::consise\n- ", bullets, "\n:::")
  
  return(finalForRender)
}

createWorkBulletsFromExcel <- function(experienceId, fullBulletsDataframe){
  
  usingData <- filter(fullBulletsDataframe, 
                      fullBulletsDataframe$ExperienceId == experienceId)
  
  new_ob <- renderBulletsForResume(usingData$Description)
  
  return(new_ob)
}

createWorkExperienceDetail <- function(rowNum, dataframe, allRenderedBullets){
  
  usingRow <- dataframe[rowNum,]
  
  startDate = paste(usingRow$StartDateMonth, 
                    usingRow$StartDateYear, sep = ", ")
  
  endDate = paste(usingRow$EndDateMonth, 
                  usingRow$EndDateYear, sep = ", ")
  
  
  specificExperienceId <- as.character(usingRow$ExperienceId)
  specificBullet <- unname(allRenderedBullets[specificExperienceId])
  
  
  newInstance <- 
    new("workExperienceDetails",
        experienceId = usingRow$ExperienceId,
        companyName_full = usingRow$CompanyName_full,
        companyName_short = usingRow$CompanyName_short,
        companySubtitle = usingRow$CompanySubtitle,
        positionTitle = usingRow$PositionTitle,
        startDate = usingRow$StartDateYear,
        endDate = usingRow$EndDateYear,
        city = usingRow$City,
        state = usingRow$State,
        experienceBullets = specificBullet)
  
  return(newInstance)
  
}

renderWorkExperienceDetailsForResume <- function(workExerienceDetailObject){
  
  endDateDetail <- workExerienceDetailObject@endDate
  if(is.na(endDateDetail)){
    endDateDetail <- "Present"
  }
  
  outString <- 
    paste0("### ", workExerienceDetailObject@positionTitle,
            "\n \n", 
            workExerienceDetailObject@companyName_full,
            "\n \n", 
            workExerienceDetailObject@city, ", ", workExerienceDetailObject@state, "\n \n",
           endDateDetail, " - ", workExerienceDetailObject@startDate,
            "\n \n", workExerienceDetailObject@experienceBullets)

  return(outString)
  
}
