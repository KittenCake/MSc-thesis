library(tidyverse)
library(ggplot2)
library(proto) 
library(gsubfn)
library(readr)
library(anytime)

#Universit??t Z??rich EBPI
#Pascal Misala, 2019
#Version: 2.0


# Auswertungsskript zur ersten Aufl??sung des Datensatzes (??bersicht) - WeCoach Intervention 


######## Imports + set up ########

#first all team survey files + session_variables, plus the external variable-exclusion file must be uploaded

#external variable-exclusion is an external .csv file that includes a list of all variables that are to be excluded (not needed) in the data conversion. Adjust this file if different variables are needed.
#The file is generated through a separate R-file (though could be integraed in this one)

#set path to working directory with all files (survey files, session_variables, external variable exclusion) in it - resulting .csv conversions are saved in this directory
setwd("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/Live Datensatz/Updated Live Datensatz")

#list all data files in a big list of data.frames in the current folder (make sure only the session_variables and survey team files are in it)
listAllFiles <- list.files(pattern="*.csv")

#set path to exclusion list (external)
toExcludeVariables <- read_csv("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/toExcludeVariables.csv")

toExcludeVariables <- rbind(toExcludeVariables, "workspace")

#set path to ID's identifier, teamNumber with FKID - (preset File)
fkidIdentifierTeam <- read_delim("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/IDs.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)



#Loop through directory loading all the csv files into a list of dataFrames (allDataFrames) for later use (actual upload of the lists) -> checks via listAllFiles
allDataFrames <- NULL

dataPath <- "~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/Live Datensatz/Updated Live Datensatz/"

for(i in 1:length(listAllFiles)){
  
  tempDataFrames <-  read_delim(paste(dataPath,listAllFiles[i],sep= ""),
                                 ";", escape_double = FALSE, trim_ws = TRUE, na = "NA")
  
  allDataFrames <- append(allDataFrames,list(tempDataFrames))
  
}

rm(tempDataFrames)

#find position of session variable frame in list of all dataFrames (in case upload of files is not ordered) - could probably be solved better with key value pairing

#loading all Data of the session variables frame to a new data frame (trialdata)

trialdata <- data.frame(allDataFrames[match("session_variables.csv",listAllFiles)])


######## Big converting starts here (starting with the session variables file) ########


# order ID's (in session variables) ascending 
trialdata <- trialdata[order(trialdata$FKID),] 


# Idea -- Convert variables to row index -- and leader id to column index -- fill up with corresponding values. 
# (chronologische Reihenfolge der Variablen evtl finden? - besser nachvollziehbar...)

# List of unique trials and subjects - creating a list of unique variables for the later creation of the end data frame  // ---> FKID (double) and undefined 

variableList <-  "FKID"
variableList <- append(variableList, unique(trialdata$Variable))

#List of all variables in the current session variables file
variableListDataFrame <-  data.frame(variableList)

#exclude unwanted variables here.. with  "setdiff(list.a, list.b)" - filters the current set of variables (all in session variables) with the external exclude variables list

toExcludeVariablesList <- toExcludeVariables$x


#a list with all wanted variable names in this conversion - Further variables can be added here (new data or variables)
wantedVariableList <-  "FKID"

####This is just my specific selection of variables that I needed in my work - **adjust as needed**

wantedVariableList <- append(wantedVariableList, grep("SESSION|LOGIN|JOURNEY|Journey", variableList, value=TRUE))

#add index of chosen monitor variables 
wantedVariableList <- append(wantedVariableList, variableList[match(c("$MONITORfullN","$MONITORfullVN","$MONITORfullPCT","$MONITORfullCounter","$O_PAYCHECK_V"), variableList)])

#remove variables with "MEDIAN" and "xmlns:rule" and "eInvitation" in them
wantedVariableList <- wantedVariableList[lapply(wantedVariableList,function(x) length(grep("MEDIAN|<rule:result xmlns:rule=|_eInvitation|_eReminder",x,value=FALSE))) == 0]

####

#Nr of unique subejcts in dataset
subjectList <- unique(trialdata$FKID)

#creating a dataFrame with unique variable names (wanted Variable list) as columns / name them -- fill out 
sessionVariables <- data.frame(matrix(nrow = length(subjectList), ncol= length(wantedVariableList) + 1,byrow=TRUE))
colnames(sessionVariables) <- wantedVariableList


#helper variables
fkidIndex <- 1
firstFid <-  as.numeric(trialdata[1,1])


#handling session variables file nrow(trialdata)
#loop through input data.frame (session variables) and insert values in the new one. -- takes unreasonable amount of time though (some hours)... (plenty of room for improvement here)

for(i in 1:nrow(trialdata)){
  
  if(trialdata[i,1] == "NA"| is.na(trialdata[i,1])){
    
    #wait for actual value
  }

  else{
  
    if(is.na(sessionVariables$FKID[fkidIndex]) == TRUE){
    sessionVariables$FKID[fkidIndex] <- firstFid
   }
  
    if(trialdata[i,1] != firstFid && fkidIndex <= (length(subjectList) -1)){
    
      fkidIndex <- fkidIndex + 1
      firstFid <- as.numeric(trialdata[i,1])
   }
   
      #for loop to check variable name  --> retrieving columnname => colnames(sessionVariables[2])
          for(j in 2:ncol(sessionVariables)){
      
            if(trialdata$Variable[i] == colnames(sessionVariables[j]) && j <= (length(wantedVariableList) - 1))
           {
              #add variable values to corresponding fkid and variable
                 sessionVariables[fkidIndex,j] <- trialdata[i,3]
             
             }
          }
   }
  
  }


#in case you want to drop empty rows --> where all lines are Na's -- Excludes all leaders or team members that do not have any values
rowsToDrop <- NULL
for(y in 1:nrow(sessionVariables)){
  
  if(sum(is.na(sessionVariables[y, ])) >= length(wantedVariableList)){
    rowsToDrop <- c(rowsToDrop, y)
  }
}

#remove all lines with only NA's
sessionVariables <- sessionVariables[-rowsToDrop, ]

# in case you want to drop empty columns --> where a column does not consits of any data
colsToDrop <- NULL
for(y in 1:ncol(sessionVariables)){
  
  if(sum(is.na(sessionVariables[, y])) == nrow(sessionVariables)){
    colsToDrop <- c(rowsToDrop, y)
  }
}

#remove all columns with only NA's
sessionVariables <- sessionVariables[,-colsToDrop]

#write resulting converted session variable file to current working directory as .csv
write.csv(sessionVariables, file = "ConvertedSessionVariables.csv", row.names = FALSE)



################################################################################################################################################

# handle team survey files  // in the code changed aliases - SessionID = Survey ID, FKID = TeamSurveyNr, TeamMember = PersonID

#idea loop through all team survey files in working directory to create data frame of all variables -- in a next loop fill out all the data by looping through them

#get this from the list (list with all DataFrames) -- Create a list of all survey team files first 
surveyTeamList <- allDataFrames
surveyTeamList[match("session_variables.csv",listAllFiles)] <- NULL


# list of team survey in one file (needed to create table) -- on this level exclude not needed variable via the exclusion list (similar to above)

#nr of rows 
rowNrNames <- NULL

#nr of variables
variableListTeam <- NULL

#create a list with all variable names in all team survey files --
for(i in 1:length(surveyTeamList)){
  
  rowNrNames <- append(rowNrNames, colnames(as.data.frame(surveyTeamList[i])[-1]))
  variableListTeam <- append(variableListTeam, unique(as.data.frame(surveyTeamList[i])$X1))
  
}


#clean out SessionID aus variableListTeam
variableListTeam <- unique(variableListTeam)
variableListTeam <- variableListTeam[!grepl("^[A-Z]|^[0-9]", variableListTeam)]
variableListTeam <- variableListTeam[!grepl("init",unlist(variableListTeam))]
variableListTeam <- variableListTeam[ variableListTeam != "$_pin" ]

#add variables - TeamSurveyNr, SurveyID, PersonID, isTeamLeader, timeLog
variableListTeam <-  prepend(variableListTeam,"isTeamLeader")
variableListTeam <-  prepend(variableListTeam, "PersonID")
variableListTeam <-  prepend(variableListTeam,"Timestamp Sc_125_FK")
variableListTeam <-  prepend(variableListTeam,"SurveyID")
variableListTeam <-  prepend(variableListTeam, "TeamSurveyNr")



#Filter the list of variables with the exclusion List // - here are manually some variables excluded - they are being excluded by their index in the list (adjust as needed)
toExcludeVariablesList <- toExcludeVariablesList[-c(122,123,124,125,126,388)]

# lazy and easy way to just strike $Q_S199_02 - 5 variables from list
toExcludeVariablesList <- toExcludeVariablesList[-match("$Q_S199_01",toExcludeVariablesList)]
toExcludeVariablesList <- toExcludeVariablesList[-match("$Q_S199_02",toExcludeVariablesList)]
toExcludeVariablesList <- toExcludeVariablesList[-match("$Q_S199_03",toExcludeVariablesList)]
toExcludeVariablesList <- toExcludeVariablesList[-match("$Q_S199_04",toExcludeVariablesList)]
toExcludeVariablesList <- toExcludeVariablesList[-match("$Q_S199_05",toExcludeVariablesList)]

# Remove all variables from the list (with all current variables in data) which are in the exclude file
wantedVariableListTeam <- setdiff(variableListTeam, toExcludeVariablesList)

#creating an empty dataFrame with unique variable names as columns - with the wantedVariableListTeam as columns names  // *50 is an imaginary Nr. -just to make sure there is enough room for pos. bigger datasets in the future (scale up if bigger) -*general room for improvement here
sessionVariablesTeams <- data.frame(matrix(nrow = (length(rowNrNames)*50), ncol= length(wantedVariableListTeam),byrow=TRUE))
colnames(sessionVariablesTeams) <- wantedVariableListTeam


#helper variables
currentSessionId <- "placeholder"
sessionCounter <- 0
currentFKID <- NULL
currentTeamMember <- NULL
currentTeamNr <- "placeholder"

#set for loop x loops through all survey files in the relevant surveyTeamList - (*takes about 10 mins with 340 team files*)

for(x in 1:length(surveyTeamList)){

  currentDataFrame <- as.data.frame(surveyTeamList[x])
  currentTeamNr <- as.numeric(gsub("[^0-9-]", "", listAllFiles[x + 1]))
  currentFKID <- colnames(currentDataFrame[2])
  

#set for loop j loops through all columns (which are the team memebers) in one team survey file
for(j in 2:ncol(currentDataFrame)){
  
  # if its not a timestamp column -> do everything below
  if(grepl("_", colnames(currentDataFrame[j])) == FALSE){
   
   
  # add conditionals to handle if further teamMembers filled out something --> each team memeber needs its own session
  currentTeamMember <- colnames(currentDataFrame[j])
  
#set for loop i loops through all rows (variable names) in one team survey file
    for(i in 1:nrow(currentDataFrame)){
    
      #check for new session id, if so alter helper variables and add on new row  //currentTeamNr == as.numeric(gsub("[^0-9-]", "", listAllFiles[x + 1]))
      if(grepl("^[A-Z]|^[0-9]", currentDataFrame$X1[i]) ==  TRUE){
    
        #set new sessionVariable
        currentSessionId <- currentDataFrame$X1[i]
        sessionCounter <- sessionCounter + 1
        sessionVariablesTeams$SurveyID[sessionCounter] <- currentSessionId
        sessionVariablesTeams$TeamSurveyNr[sessionCounter] <- as.numeric(gsub("[^0-9-]", "", listAllFiles[x + 1]))
        currentTeamNr <- listAllFiles[x + 1]
    
        #handle teammembers
        sessionVariablesTeams$PersonID[sessionCounter] <- currentTeamMember
        }
  
    #add and find normal variable fits
    if(is.element(currentDataFrame$X1[i], wantedVariableListTeam) == TRUE){
      
      sessionVariablesTeams[[match(currentDataFrame$X1[i], wantedVariableListTeam)]][[sessionCounter]] <- currentDataFrame[[j]][i]
      
      if(currentDataFrame[i,1] == "$C_S125_FK_IC" || currentDataFrame[i,1] == "$Q_S199"){
        
        sessionVariablesTeams$`Timestamp Sc_125_FK`[sessionCounter] <- currentDataFrame[[j+1]][i]
      }
      
      # sessionVariablesTeams$timeLog <- currentDataFrame[[j + 1]][i]
     }
       
        #evaluate if team leader or not from file ID
        #returns nr of the file --> idea compare this number and the index (FKID) of the current file with the reference if current FKID matches filename (in the reference) fill isLeader = yes
        #gsub("[^0-9-]", "", listAllFiles[2])
        
      # Team- Nummer in reference file of the currentFKID of the looped file - This number must be the same as the nth filename(counted through loop position) in listAllFiles - (if not the currentFKId is not the TeamLeader of the current TeamNr)
      # Problem here can return NA (if FKID is not in reference file) ) = is solved
        referenceTeamNumber <- fkidIdentifierTeam$`Team-Nummer`[match(as.numeric(gsub("[^0-9-]", "",currentFKID)), fkidIdentifierTeam$FKID)]
      
         
        if(is.na(referenceTeamNumber) == FALSE){
        
      if(referenceTeamNumber == as.numeric(gsub("[^0-9-]", "", listAllFiles[x + 1])) && currentFKID == currentTeamMember){
    
          sessionVariablesTeams$isTeamLeader[sessionCounter] <- "Yes"
          
          }else {
            
            if(as.numeric(gsub("[^0-9-]", "", colnames(currentDataFrame[j]))) == fkidIdentifierTeam$FKID[match(as.numeric(gsub("[^0-9-]", "", listAllFiles[x + 1])), fkidIdentifierTeam$`Team-Nummer`)]){
              
              sessionVariablesTeams$isTeamLeader[sessionCounter] <- "Yes"
              
            } else {sessionVariablesTeams$isTeamLeader[sessionCounter] <- "No"}
          }
        #no match - team leader is not in reference file
      }else {
          sessionVariablesTeams$isTeamLeader[sessionCounter] <- "no reference"
        }  
      }
    }
  }
}


#switch all "-" to NA's -- to later remove any rows with only NA's in them 
sessionVariablesTeams[sessionVariablesTeams == "-"] <- NA

#Create a list with all the row index nr's that only have Na's in it (after the first 3 columns)

rowsToDrop <- NULL
for(y in 1:nrow(sessionVariablesTeams)){
  
  if(sum(is.na(sessionVariablesTeams[y, ])) >= length(wantedVariableListTeam)){
    rowsToDrop <- c(rowsToDrop, y)
    }
  }

#remove all lines with only NA's
sessionVariablesTeams <- sessionVariablesTeams[-rowsToDrop, ]

#write converted sessionVariablesTeams in current working directory
write.csv(sessionVariablesTeams, file = "ConvertedTeamSurveyVariables.csv", row.names = FALSE)

#SessionVariables relevante Variablen 
write.csv(colnames(sessionVariables), file = "relevantVariablesSession_Variables.csv", row.names = FALSE)

#SessionVariablesTeams relevante Variablen 
write.csv(colnames(sessionVariablesTeams), file = "relevantVariablesSessionTeam_Variables.csv", row.names = FALSE)