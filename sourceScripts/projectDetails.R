setClass("projectDetails",
         slots = list(projectId = "numeric",
                      projectName_full = "character",
                      projectName_short = "character", 
                      companyName_short = "character",
                      companyName_full = "character",
                      projectYear = "numeric",
                      projectLink  ="character",
                      projectBullets = "character"))


renderBulletsForResume <- function(bulletsVector){

  bullets <- paste(bulletsVector, collapse = "\n- ")

  finalForRender <- paste0(":::consise\n- ", bullets, "\n:::")

  return(finalForRender)
}

createProjectBulletsFromExcel <- function(projectId, fullBulletsDataframe){
  
  usingData <- filter(fullBulletsDataframe, 
                      fullBulletsDataframe$ProjectId == projectId)
  
  new_ob <- renderBulletsForResume(usingData$Description)
  
  return(new_ob)
}

createProjectExperienceDetail <- function(rowNum, dataframe, allRenderedBullets){
  
  usingRow <- dataframe[rowNum,]
  
  specificExperienceId <- as.character(usingRow$ProjectId)
  specificBullet <- unname(allRenderedBullets[specificExperienceId])
  
  
  newInstance <- 
    new("projectDetails",
        projectId = usingRow$ProjectId,
        projectName_full = usingRow$ProjectName_full,
        projectName_short = usingRow$ProjectName_short,
        companyName_full = usingRow$ProjectCompanyName_full,
        companyName_short = usingRow$ProjectCompanyName_short,
        projectYear = usingRow$ProjectYear,
        projectLink = usingRow$ProjectLink,
        projectBullets = specificBullet)
  
  return(newInstance)
  
}

renderProjectExperienceDetailsForResume <- function(projectDetailObject){
  
  projectYearDetail <- projectDetailObject@projectYear
  if(is.na(projectYearDetail)){
    projectYearDetail <- "Ongoing"
  }
  

  if(is.na(projectDetailObject@projectLink)){
    projectLink <- "N/A"
  } else{
    projectLink <- 
      paste0("<i class=\"fas fa-globe\"></i> ",
             "[Published Project Link](",
             projectDetailObject@projectLink,
             ")")
  }
  
  outString <- 
    paste0("### ", projectDetailObject@projectName_full,
           "\n \n", 
           projectDetailObject@companyName_full,
           "\n \n", 
           projectLink,
           "\n \n",  #Removing geographic location
           projectYearDetail,"\n \n", 
           projectDetailObject@projectBullets)
  
  return(outString)
  
}


# projectBullets <- read_excel("00_resumeDetails.xlsx", sheet = "ProjectsBullets")
# projectDetails <- read_excel("00_resumeDetails.xlsx", sheet = "ProjectsDetails")
# 
# allProjectIds <- unique(projectBullets$ProjectId)
# 
# testList <- projectBullets$Description[projectBullets$ProjectId == 1]
# 
# 
# 
# renderBulletsForResume(testList)
# allRenderedBullets <- 
# createProjectBulletsFromExcel(projectId = 1, fullBulletsDataframe = projectBullets)
# 
# names(allRenderedBullets) <- c("1", "2", "3")
# 
# testObs <- 
# createProjectExperienceDetail(rowNum = 1,
#                               dataframe = projectDetails,
#                               allRenderedBullets)


