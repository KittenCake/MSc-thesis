library(tidyverse)
library(ggplot2)
library(proto) 
library(gsubfn)
library(readr)
library(anytime)
library(lubridate)
library(psy)
library(psych)
library(dplyr)
library(DescTools)
library(lsr)
library(fmsb)
library(mctest)

#Universität Zürich EBPI
#Pascal Misala, 2019
#Version: 1.5

###import converted cleaned datatables

#set path to working directory
setwd("~/Dropbox/Master/Masterarbeit/Datensatz Auswertung/Live Datensatz/Updated Live Datensatz")


#1 import
sessionVariablesAnalysis <- read_csv("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/Live Datensatz/Updated Live Datensatz/ConvertedSessionVariables.csv")


surveyTeamAnalysis <- read_csv("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/Live Datensatz/Updated Live Datensatz/ConvertedTeamSurveyVariables.csv")

#exclude the ones with no reference in "isTeamLeader"
surveyTeamAnalysis <- subset(surveyTeamAnalysis, surveyTeamAnalysis$isTeamLeader != "no reference")

#2 filter participants out that have "NA" in $O_SESSION_COUNTER - they have 0 completed sessions (exclude from anaylsis) / filter out participants who have $O_PAYCHECK_V == 0 / = free trial version
sessionVariablesAnalysis <- subset(sessionVariablesAnalysis, sessionVariablesAnalysis$`$O_SESSION_COUNTER` != "NA")


#3 assign/group remaining participants to their preassigned group - experimental/control -- kann ergänzt werden
interventionGrp <- c(190,909,609,792,638,1154,791,428,1090,1089,1037,1182,1103,1225,1402,1424,1554,1547,1526,1488,1565)
controlGrp <- c(726,789,790,791,1102,1091,1175,416,1099,1248,1241,1239,1247,1181,1228,1259,1332,1514,1525,1562,1563)


# Konzeptuelle Black Box für alle User welche Session Counter mehr wie 1 haben --> Da nur jeweils der letzte Eintrag vorhanden ist im Datensatz

#Interest/ Compliance  --> set of data points needs to set - data considering at certain point..
################################################################################################################################################


#indicator variable list / as data frame
interestIndicators <- read_csv("~/Dropbox/Master/Masterarbeit/Datensatz Auswertung/Indikatoren/interestCompliance/interestIndicators.csv", col_names = FALSE)

complianceIndicators <- read_csv("~/Dropbox/Master/Masterarbeit/Datensatz Auswertung/Indikatoren/interestCompliance/complianceIndicators.csv", col_names = FALSE)

# as list
interestIndicatorsList <- "FKID"
interestIndicatorsList <- append(interestIndicatorsList,interestIndicators$X1)

complianceIndicatorsList <- "FKID"
complianceIndicatorsList <-  append(complianceIndicatorsList, complianceIndicators$X1)


# there are certain interest/compliance variables missing in the actual dataset -- a list of them here:
missingInterest <- "$O_SESSION_C5_RESULTS_GO"
missingCompliance <- c("$O_SESSION_A4_TOSCA_FILLIN","$O_SESSION_A5_ILEAD_FILL","$O_SESSION_B2_PRINCIPLES","$O_SESSION_B5_SWE_FILL","$O_SESSION_C4_REACTIONSELF","$O_SESSION_C5_ES_INFOS","$O_SESSION_D3_SWE_FILL")

#remove the missing variables form the list in order to create a data frame to work with
interestIndicatorsList <- interestIndicatorsList[! interestIndicatorsList %in% missingInterest]
complianceIndicatorsList <- complianceIndicatorsList[! complianceIndicatorsList %in% missingCompliance]


#logic to create a new dataFrame with a list of columns from another dataframe

#DataFrame with all interest variables 
interestDataFrame <- select(sessionVariablesAnalysis, interestIndicatorsList)

#Delete FKID that did not answer any interest variables
rowsToDrop <- NULL
for(y in 1:nrow(interestDataFrame)){
  
  if(sum(is.na(interestDataFrame[y, ])) >= 54){
    rowsToDrop <- c(rowsToDrop, y)
  }
}

interestDataFrame <- interestDataFrame[-rowsToDrop, ]


listOfEligibleSubjects <- unique(eligibleSubjects$PersonID)

eligibleSubjectsInterest <- subset(interestDataFrame,interestDataFrame$FKID %in% listOfEligibleSubjects)



#testing - drop all that filled out less then 40% = 32, 80% = 18, 50% = 27 of the interest variables
rowsToDrop <- NULL
for(y in 1:nrow(interestDataFrame)){
  
  if(sum(is.na(interestDataFrame[y, ])) >= 27){
    rowsToDrop <- c(rowsToDrop, y)
  }
}

# Interest where 40, 50, 80% of indicators are available
interestDataFrame40 <- interestDataFrame[-rowsToDrop, ]
interestDataFrame80 <- interestDataFrame[-rowsToDrop, ]
interestDataFrame50 <- interestDataFrame[-rowsToDrop, ]


#INTEREST
interestPlotEligibleSubjec <- data.frame(matrix(nrow = nrow(eligibleSubjectsInterest), ncol= 3,byrow=TRUE))
colnames(interestPlotEligibleSubjec) <- c("FKID","Nr","interestInPercent")

#calculate overall interest variable per participant relative to the number of chance to show it. --> Rounded 
for(v in 1:nrow(eligibleSubjectsInterest)){

  interestPlotEligibleSubjec$FKID[v] <- eligibleSubjectsInterest$FKID[v]
  interestPlotEligibleSubjec$Nr[v] <- v
  interestPlotEligibleSubjec$interestInPercent[v] <- round(100/(sum(!is.na(eligibleSubjectsInterest[v, ])) -1) * sum(str_count(eligibleSubjectsInterest[v,2:55], "2")))
}

#interest Plot
interestGraphEligibleSubjects <- ggplot(interestPlotEligibleSubjec, aes(interestPlotEligibleSubjec$interestInPercent, interestPlotEligibleSubjec$Nr)) + geom_point(fill="blue",
                                                                                                      colour="darkblue", size=1) + xlim(0,100) + labs(title = "Overall Interest N=84" , x = "interest in percent (100%)", y = "participant Nr")
#COMPLIANCE

#DataFrame with all compliance variables
complianceDataFrame <- select(sessionVariablesAnalysis, complianceIndicatorsList)


# drop col $O_SESSION_A4_TOSCA5 no values
complianceDataFrame <- complianceDataFrame[,-6]


eligibleSubjectsCompliance <- subset(complianceDataFrame,complianceDataFrame$FKID %in% listOfEligibleSubjects)


#get rid of empty rows
rowsToDrop <- NULL
for(y in 1:nrow(complianceDataFrame)){
  
  if(sum(is.na(complianceDataFrame[y, ])) >= 35){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
complianceDataFrame <- complianceDataFrame[-rowsToDrop, ]
complianceDataFrame50 <- complianceDataFrame[-rowsToDrop, ]
complianceDataFrame40 <- complianceDataFrame[-rowsToDrop, ]
complianceDataFrame70 <- complianceDataFrame[-rowsToDrop, ]

#inverse negative items that indicate compliance - people that did not fill out certain things (and get 2 for yes) - need to be inverted to 1 (not compliant)
invertComplianceItems <- c("$O_SESSION_D5_HOL1","$O_SESSION_D3_SWE_FILL","$O_SESSION_C2_TEAMINFO3","$O_SESSION_C2_TEAMINFO2","$O_SESSION_C2_TEAMINFO1","$O_SESSION_B5_SWE_FILL","$O_SESSION_B5_GOALS","$O_SESSION_B2_HOL1","$O_SESSION_B1_FILL_IN1","$O_SESSION_A5_ILEAD_FILL","$O_SESSION_A4_TOSCA5","$O_SESSION_A4_TOSCA_FILL","$O_SESSION_A4_TOSCA_FILLIN","$O_SESSION_A3_USE_FILL1","$O_SESSION_A1_SWE_FILL")

for(c in 1:ncol(eligibleSubjectsCompliance)){
  
    if(colnames(eligibleSubjectsCompliance[c]) %in% invertComplianceItems == TRUE){
    
        for(b in 1:nrow(eligibleSubjectsCompliance)){
    
           if(eligibleSubjectsCompliance[b,c] == 1 && !is.na(eligibleSubjectsCompliance[b,c])){
        
             eligibleSubjectsCompliance[b,c] <- 2 }
    
          else if(eligibleSubjectsCompliance[b,c] == 2 && !is.na(eligibleSubjectsCompliance[b,c])){
       
            eligibleSubjectsCompliance[b,c] <- 1}
          }
    }
}

# compliance where 40, 50, 80% of indicators are available -- 100% = 36, 70% = 25 (no data), 50% = 18, 40% = 14

#COMPLIANCE
compliancePlotEligibleSubjects <- data.frame(matrix(nrow = nrow(eligibleSubjectsCompliance), ncol= 3,byrow=TRUE))
colnames(compliancePlotEligibleSubjects) <- c("FKID","Nr","complianceInPercent")

#calculate overall compliance variable per participant relative to the number of chance to show it. --> Rounded 
for(v in 1:nrow(eligibleSubjectsCompliance)){
  
  compliancePlotEligibleSubjects$FKID[v] <- eligibleSubjectsCompliance$FKID[v]
  compliancePlotEligibleSubjects$Nr[v] <- v
  compliancePlotEligibleSubjects$complianceInPercent[v] <- round(100/(sum(!is.na(eligibleSubjectsCompliance[v, ])) -1) * sum(str_count(eligibleSubjectsCompliance[v,2:37], "2")))
}

#compliance Plot
complianceGraphEligibleSubjects <- ggplot(compliancePlotEligibleSubjects, aes(compliancePlotEligibleSubjects$complianceInPercent, compliancePlotEligibleSubjects$Nr)) + geom_point(fill="red",
                                            colour="red", size=1) + xlim(0,100) + labs(title = "Overall Compliance N=83" , x = "compliance in percent (100%)", y = "participant Nr")


#System Data - Nr. logins - General Activity in/during Sessions
################################################################################################################################################

#list of the here relevant variables  
NrloginSessionData <- read_csv("~/Dropbox/Master/Masterarbeit/Datensatz Auswertung/Indikatoren/SystemActivity/systemActivityIndicators.csv", 
                               col_names = FALSE)

# as list
NrloginSessionDataList <- "FKID"
NrloginSessionDataList <- append(NrloginSessionDataList,NrloginSessionData$X1)

#temporal variables || _FREQ|_MEDIAN|_LASTSTART|_LASTEND"
temporalVarSessionDataList <- append(NrloginSessionDataList, grep("_FREQ|_MEDIAN",colnames(sessionVariablesAnalysis),value=TRUE))

#DataFrame with all here relevant variables (without temporal)
NrloginSessionData <- select(sessionVariablesAnalysis, NrloginSessionDataList)

#without monitorfull (for now)
temporalVarSessionDataList <- temporalVarSessionDataList[-c(match(c( "$MONITORfullN","$MONITORfullVN","$MONITORfullPCT","$MONITORfullCounter", "$O_Journey2","$O_Journey1"), temporalVarSessionDataList))]

temporalVarDataFrame <- select(sessionVariablesAnalysis, temporalVarSessionDataList)

eligibleSubjectsTempVarSessData <- subset(temporalVarDataFrame,temporalVarDataFrame$FKID %in% listOfEligibleSubjects)


eligibleSubjectsLoginFreq <-  barplot(eligibleSubjectsTempVarSessData$`$O_LOGIN_FREQ`, main="Login Frequency N =84", ylab="Anzahl Logins in den Coach",xlab="Team-Leader", ylim = c(0,50))

eligibleSubjectsSessionCount <-  barplot(eligibleSubjectsTempVarSessData$`$O_SESSION_COUNTER`, main="Durchgefuehrte Sessions N =84", ylab="Bereits durchgefuehrte Sessions",xlab="Team-Leader", ylim = c(0,40))
 
# Sessions vs. Login Freq.
ggplot(eligibleSubjectsTempVarSessData, aes(y=eligibleSubjectsTempVarSessData$FKID, x=eligibleSubjectsTempVarSessData$`$O_LOGIN_FREQ`)) + geom_point()

ggplot(eligibleSubjectsTempVarSessData, aes(y=eligibleSubjectsTempVarSessData$FKID, x=eligibleSubjectsTempVarSessData$`$O_SESSION_COUNTER`)) + geom_point()

#temporal graph - show on x axis time in days relative from start of the first session of given person - this sets "date zero" from this date on cont days to next nearest date and session
 # --> take only pairs of start and end - that are within a reasonable time frame like maybe 24hours - take only ids where pairs /we want to know usage here

#LASTSTART/LASTEND

lastStartEnd <- "FKID"
lastStartEnd <- append(lastStartEnd, grep("_MEDIAN|_LASTSTART|_LASTEND",colnames(sessionVariablesAnalysis),value=TRUE))

lastStartEnd <- sort(lastStartEnd)

lastStartEndData <- select(sessionVariablesAnalysis, lastStartEnd)

eligibleSubjectsLastStartEndData <- subset(lastStartEndData ,lastStartEndData$FKID %in% listOfEligibleSubjects)


#get rid of empty rows
rowsToDrop <- NULL
for(y in 1:nrow(lastStartEndData)){
  
  if(sum(is.na(lastStartEndData[y, ])) >= 40){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
lastStartEndData <- lastStartEndData[-rowsToDrop, ]

lastStartEndData80 <- lastStartEndData[-rowsToDrop, ]
lastStartEndData90 <- lastStartEndData[-rowsToDrop, ]

#getting the difference between two dates as rounded numeric in probably days
# as.numeric(round(lastStartEndData$`$O_SESSION_A2_LASTSTART`[9] - lastStartEndData$`$O_SESSION_A1_LASTSTART`[9]))
# x - days relative to start, y - sessions a1-d5

# need a data frame with col. A1-5 to D1-5 that represents pairs of start & end with the difference value in it - evaluate first date value and set that as Zero - further Values in A1-x are added upon that (those are the chronicle steps) that can be plotted per participant 

eligibleSubjectsCompletedSessionsPlot <- data.frame(matrix(nrow = nrow(eligibleSubjectsLastStartEndData)*20, ncol= 4,byrow=TRUE))
colnames(eligibleSubjectsCompletedSessionsPlot) <- c("session","timeDifferenceToZero", "timeSpent","FKID")

eligibleSubjectsOnlyStartedSessionsPlot <- data.frame(matrix(nrow = nrow(eligibleSubjectsLastStartEndData)*20, ncol= 3,byrow=TRUE))
colnames(eligibleSubjectsOnlyStartedSessionsPlot) <- c("session","timeDifferenceToZero","FKID")

smallIterator <- 1

for(i in 1:nrow(eligibleSubjectsLastStartEndData)){   # all time data is in hours now
  
  listOfCurrentDates <- NULL
  currentFirstEntry <- NULL
  
  #go through all columns on that row
  for(j in 1:ncol(eligibleSubjectsLastStartEndData) -1){
    
    listOfCurrentDates <- append(listOfCurrentDates,as.integer(eligibleSubjectsLastStartEndData[i,j]))
    
  }
  #do something with the data of that row before next turns up
  #all entries that are not Na - and the earliest date on this row
  currentFirstEntry <- min(anytime(listOfCurrentDates[which(!is.na(listOfCurrentDates))], asUTC = TRUE))
  
  currentlistOfDatesIndex <- which(!is.na(listOfCurrentDates))
  
  
    for(l in 1:length(currentlistOfDatesIndex)){
      
     
          if((currentlistOfDatesIndex[l] %% 2) == 0 ){       # is even -- do try to - 1 to look if theres a pair if its not NA you only have to check for even
              
              if(is.na((listOfCurrentDates[currentlistOfDatesIndex[l]-1]))){  # just start
                
                eligibleSubjectsOnlyStartedSessionsPlot$session[smallIterator] <- colnames(eligibleSubjectsLastStartEndData[currentlistOfDatesIndex[l]])
                eligibleSubjectsOnlyStartedSessionsPlot$FKID[smallIterator] <- eligibleSubjectsLastStartEndData$FKID[i]
                eligibleSubjectsOnlyStartedSessionsPlot$timeDifferenceToZero[smallIterator] <- difftime(anytime(listOfCurrentDates[currentlistOfDatesIndex[l]], asUTC = TRUE), currentFirstEntry, units = "days") # add just start Timestamp
                smallIterator <-  smallIterator + 1
              }
            
                  else{   # if -1 is not = NA its a pair
                
            
                    if(difftime(anytime(listOfCurrentDates[currentlistOfDatesIndex[l]-1], asUTC = TRUE), anytime(listOfCurrentDates[currentlistOfDatesIndex[l]], asUTC = TRUE), units = "hours") < 0){
                      
                      eligibleSubjectsOnlyStartedSessionsPlot$timeDifferenceToZero[smallIterator]  <- difftime(anytime(listOfCurrentDates[currentlistOfDatesIndex[l]-1], asUTC = TRUE), anytime(listOfCurrentDates[currentlistOfDatesIndex[l]], asUTC = TRUE), units = "days")
                      eligibleSubjectsOnlyStartedSessionsPlot$session[smallIterator] <- colnames(eligibleSubjectsLastStartEndData[currentlistOfDatesIndex[l]])
                      eligibleSubjectsOnlyStartedSessionsPlot$FKID[smallIterator] <- eligibleSubjectsLastStartEndData$FKID[i]
                    
                      }
                    else{eligibleSubjectsCompletedSessionsPlot$timeSpent[smallIterator] <- difftime(anytime(listOfCurrentDates[currentlistOfDatesIndex[l]-1], asUTC = TRUE), anytime(listOfCurrentDates[currentlistOfDatesIndex[l]], asUTC = TRUE), units = "hours")
                    eligibleSubjectsCompletedSessionsPlot$timeDifferenceToZero[smallIterator] <-  difftime(anytime(listOfCurrentDates[currentlistOfDatesIndex[l]], asUTC = TRUE),currentFirstEntry, units = "days") # add just start Timestamp
                    eligibleSubjectsCompletedSessionsPlot$session[smallIterator] <- colnames(eligibleSubjectsLastStartEndData[currentlistOfDatesIndex[l]])
                    eligibleSubjectsCompletedSessionsPlot$FKID[smallIterator] <- eligibleSubjectsLastStartEndData$FKID[i]}
                    
                    smallIterator <-  smallIterator + 1
                }
            
              }
      
           else{   # if modulo is odd 
               # do nothing
            }
       }
  
}

##### compltededSessionPlot --> - hours are no pairs - participant did at one point finish the session but started a new one which they never finished... /those values need to be deleted

completedSessionsPlot <- na.omit(completedSessionsPlot)
onlyStartedSessionsPlot <- na.omit(onlyStartedSessionsPlot)

eligibleSubjectsCompletedSessionsPlot <- na.omit(eligibleSubjectsCompletedSessionsPlot)
eligibleSubjectsOnlyStartedSessionsPlot <- na.omit(eligibleSubjectsOnlyStartedSessionsPlot)

#turn all negatives to pos...
onlyStartedSessionsPlot$timeDifferenceToZero <- abs(onlyStartedSessionsPlot$timeDifferenceToZero)

eligibleSubjectsOnlyStartedSessionsPlot$timeDifferenceToZero <- abs(eligibleSubjectsOnlyStartedSessionsPlot$timeDifferenceToZero)

for(z in 1:nrow(eligibleSubjectsCompletedSessionsPlot)){
  eligibleSubjectsCompletedSessionsPlot$session[z] <- str_sub(eligibleSubjectsCompletedSessionsPlot$session[z], 12,13)
}
for(z in 1:nrow(eligibleSubjectsOnlyStartedSessionsPlot)){
  eligibleSubjectsOnlyStartedSessionsPlot$session[z] <- str_sub(eligibleSubjectsOnlyStartedSessionsPlot$session[z], 12,13)
}

###Plotting

completedSessionsEligibleSubjects <-  ggplot(eligibleSubjectsCompletedSessionsPlotFilteredHours, aes(eligibleSubjectsCompletedSessionsPlotFilteredHours$timeDifferenceToZero, eligibleSubjectsCompletedSessionsPlotFilteredHours$session)) + geom_point(fill="red",
                                                                                                                                            colour="red", size=1) + labs(title = "Start of all completed sessions relative to first started session N=82" , x = "Days", y = "Session")

onlyStartedSessionsEligibleSubjects <-  ggplot(eligibleSubjectsOnlyStartedSessionsPlot, aes(eligibleSubjectsOnlyStartedSessionsPlot$timeDifferenceToZero, eligibleSubjectsOnlyStartedSessionsPlot$session)) + geom_point(fill="red",
                                                                                                                                                      colour="blue", size=1) + labs(title = "Start of only started sessions but not finished, relative to first started session  N=60" , x = "Days", y = "Session")

### Aggregate time Functions for correlation matrice

#aggregated hours spent in sessions by all 84 participants per session
aggTimeSpentOnSessions = aggregate(eligibleSubjectsCompletedSessionsPlotFilteredHours [,3],by = list(eligibleSubjectsCompletedSessionsPlotFilteredHours $session), FUN = sum)

#remove sessions that were longer than 2hours
eligibleSubjectsCompletedSessionsPlotFilteredHours <- eligibleSubjectsCompletedSessionsPlot[eligibleSubjectsCompletedSessionsPlot$timeSpent<=2,]

# Aggregated SpendTime + TimeDifference by FKID --> With the filtered Table (<2 hours)  --> Watch out = TimeDifference to Zero is in Days
aggTimeSpentDiffbyFKID <- aggregate(eligibleSubjectsCompletedSessionsPlotFilteredHours[,c(2,3)],by = list(eligibleSubjectsCompletedSessionsPlotFilteredHours$FKID), FUN = sum)

ggplot(aggTimeSpentDiffbyFKID, aes(as.factor(aggTimeSpentDiffbyFKID$Group.1), aggTimeSpentDiffbyFKID$timeSpent)) + geom_point(fill="red",colour="blue", size=1) + labs(title = "Overall usage of Wecoach in hours per FKID N=82" , x = "FKID", y = "Hours")
                                                                                                                

# change timeDifference to zero from days to hours
aggTimeSpentDiffbyFKID$timeDifferenceToZero <- aggTimeSpentDiffbyFKID$timeDifferenceToZero*24

logFreqSessCount <- eligibleSubjectsTempVarSessData[1:3]

#merge data frames - compliancePlotEligibleSubjects, aggTimeSpentDiffbyFKID, logFreqSessCount, interestPlotEligibleSubjec  by.x = "id", by.y = "ID"

correlationDataFrameEligSub <- merge(interestPlotEligibleSubjec[,-2], compliancePlotEligibleSubjects[,-2], by = "FKID")
correlationDataFrameEligSub <- merge(correlationDataFrameEligSub, logFreqSessCount, by = "FKID")

#finished correlation matric - use cov with na.exclude
correlationDataFrameEligSub <- merge(correlationDataFrameEligSub, aggTimeSpentDiffbyFKID , by.x = "FKID", by.y = "Group.1",all = TRUE)

correlationMatriceEligSub <- round(cor(correlationDataFrameEligSub[-1], use = "complete.obs"),3)

overallHoursSpent <- ggplot(data=aggTimeSpentOnSessions, aes(x=aggTimeSpentOnSessions$Group.1, y=aggTimeSpentOnSessions$x)) + geom_bar(stat="identity", color="steelblue", fill="steelblue") +labs(title="Insgesamt Stunden verbracht in unterschiedlichen Sitzungen N=82", x ="Sessions", y = "Stunden")

numberOfWorkedSessionsEligSub <- as.data.frame(count(eligibleSubjectsCompletedSessionsPlotFilteredHours, eligibleSubjectsCompletedSessionsPlotFilteredHours$session))
numberOfSessionsWPlotEligSub <- ggplot(data=numberOfWorkedSessionsEligSub, aes(x=numberOfWorkedSessionsEligSub$`eligibleSubjectsCompletedSessionsPlotFilteredHours$session`, y=numberOfWorkedSessionsEligSub$n)) + geom_bar(stat="identity", color="steelblue", fill="steelblue") +labs(title="Anzahl Sessions bearbeitet insgesamt N=82", x ="Sessions", y = "Nr.")

correlationDataFrameEligSub$firstGesLeader <- surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$C_S130_FK_IC`),]$`$C_S130_FK_IC`[match(tester$FKID, surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$C_S130_FK_IC`),]$PersonID)]
correlationDataFrameEligSub$firstGesTeam <- surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$C_S131_FK_IC`),]$`$C_S131_FK_IC`[match(tester$FKID, surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$C_S131_FK_IC`),]$PersonID)]
correlationDataFrameEligSub$acceptInfo <- surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$O_S188_a_FK_IC`),]$`$O_S188_a_FK_IC`[match(tester$FKID, surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$O_S188_a_FK_IC`),]$PersonID)]
correlationDataFrameEligSub$acceptSyst <- surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$O_S188_b_FK_IC`),]$`$O_S188_b_FK_IC`[match(tester$FKID, surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$O_S188_b_FK_IC`),]$PersonID)]
correlationDataFrameEligSub$leadStyle <-  surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$C_S105_FK_IC`),]$`$C_S105_FK_IC`[match(tester$FKID, surveyTeamAnalysis[!is.na(surveyTeamAnalysis$`$C_S105_FK_IC`),]$PersonID)]



###########################################################################################################################################################################################################################################################################################################################################################################################################################################################
########## Extrahierung Self-Efficacy Indikator     // For Delta Values - Chronbach Alpha would be nice ---> For that you need the Values from single Items == $C_S125_01_FK_IC - $C_S125_05_FK_IC  / Total 5 Items
######
#### Defining a dataset that you want to use as population 0. > For all subsquent calculations one uses that. 4 Criteria:      1. only $O_PAYCHECK_V != 0 (no free users)     2. Everyone with $O_SESSION_COUNTER >= 2 (need at least have done 2 sessions)      3. Everyone that has Datapoint on Self-Efficacy -> $C_S125_FK_IC  (to actually see difference in outcome they need a value to compare to)   4. no Tester (excluded via an external List)
###########################################################################################################################################################################################################################################################################################################################################################################################################################################################

surveyTeamAnalysis$PersonID <-  as.integer(gsub("^.{0,1}", "", surveyTeamAnalysis$PersonID))

# list of FKID that match the criteria (see above) for the Dataset

eligibleListOfFKID <- NULL 


#1 $O_SESSION_COUNTER
eligibleListOfFKID <- sessionVariablesAnalysis$FKID[sessionVariablesAnalysis$`$O_SESSION_COUNTER` >= 2]
eligibleSubjects <- subset(surveyTeamAnalysis, surveyTeamAnalysis$PersonID %in% eligibleListOfFKID)

#2 Self-Efficacy -> $C_S125_FK_IC  - delete all that have NA in $C_S125_FK_IC
  
eligibleSubjects <- eligibleSubjects[!is.na(eligibleSubjects$`$C_S125_FK_IC`), ]

#3 $O_PAYCHECK_V != 0   (extract it directly from trialdata- file)
payCheckInfo <- subset(trialdata, Variable == "$O_PAYCHECK_V" & Value != 0)

listOfPayCheckZero <- payCheckInfo$FKID
#keep only those with 1 or 2 in $O_PAYCHECK_V
eligibleSubjects <- subset(eligibleSubjects, eligibleSubjects$PersonID %in% listOfPayCheckZero)

#4 no Tester  - remove all FKID's that are listed as testers 
# import
wecoach_TesterInnen <- read_csv("~/Dropbox/Master/Masterarbeit/Datensatz Auswertung/ReferenzVariables/wecoach_TesterInnen.csv", 
                                col_names = FALSE)
listOfTesters <- wecoach_TesterInnen$X1

#remove
eligibleSubjects <- subset(eligibleSubjects,!eligibleSubjects$PersonID %in% listOfTesters)

# List of FKID that satisfy the given 4 standards (SessionCounter,Self-Efficacy, Paycheck, none Tester) -- N = 84

#relevant Variables from Subjects
eligibleSubjectsSWEfilter <-  eligibleSubjects[,c(1:5,7:12,50:56)]

########## There needs to be a frame with all self-efficacy values, intensity Values (interest/compliance)  // for the control grp there needs to be a QS199 Value
####################################################################################################################################################################

####################################################################################################################################################################
#All relevant Information for the analysis in one dataframe - of all possible eligibleSubjects

finalAnalysisFrame <- data.frame(matrix(nrow = 1500, ncol= 13,byrow=TRUE))
colnames(finalAnalysisFrame) <- c("FKID","isTeamLeader","completionIntensity","firstPhaseCompliance", "firstPhaseInterest","meanIntensityFirstPhase","secondPhaseCompliance", "secondPhaseInterest","meanIntensitySecondPhase",
                                  "SWE Phase 1","timespan Phase 1 in days", "SWE Phase 2","timespan Phase 2 in days")

##creating a dataFrame to handle course of SWE and cronbach alpha in single phases... 
finalAnalysisCronbachAlphaSWE <- data.frame(matrix(nrow = 200, ncol= 14,byrow=TRUE))
colnames(finalAnalysisCronbachAlphaSWE) <- c("FKID","Timestamp SWE","Ph1 S125 1", "Ph1 S125 2","Ph1 S125 3","Ph1 S125 4", "Ph1 S125 5","Ph1 S125",
                                  "Ph2 S125 1","Ph2 S125 2", "Ph2 S125 3","Ph2 S125 4","Ph2 S125 5","Ph2 S125")

# 2 intervention phases -- A1-B5 => Leadership Empowerement -- C1-D5 => Team Development -- phase one most important

#first second/Compliance
firstPhaseCompliance <- complianceIndicatorsList[1:24]
firstPhaseCompliance <- firstPhaseCompliance[-5] #"$O_SESSION_A4_TOSCA5" Empty-Value

secondPhaseCompliance <- complianceIndicatorsList[1]
secondPhaseCompliance <- append(secondPhaseCompliance, complianceIndicatorsList[25:length(complianceIndicatorsList)])

#### Generating firstPhase Compliance Values - DataFrame with all firstcompliance variables - ATTENTION here all IDS

firstPhaseComplianceDataFrame <- select(sessionVariablesAnalysis, firstPhaseCompliance)

#get rid of empty rows
rowsToDrop <- NULL
for(y in 1:nrow(firstPhaseComplianceDataFrame)){
  
  if(sum(is.na(firstPhaseComplianceDataFrame[y, ])) >= 22){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
firstPhaseComplianceDataFrame  <- firstPhaseComplianceDataFrame [-rowsToDrop, ]

#inverseNegative Items
for(c in 1:ncol(firstPhaseComplianceDataFrame)){
  
  if(colnames(firstPhaseComplianceDataFrame[c]) %in% invertComplianceItems == TRUE){
    
    for(b in 1:nrow(firstPhaseComplianceDataFrame)){
      
      if(firstPhaseComplianceDataFrame[b,c] == 1 && !is.na(firstPhaseComplianceDataFrame[b,c])){
        
        firstPhaseComplianceDataFrame[b,c] <- 2 }
      
      else if(firstPhaseComplianceDataFrame[b,c] == 2 && !is.na(firstPhaseComplianceDataFrame[b,c])){
        
        firstPhaseComplianceDataFrame[b,c] <- 1}
    }
  }
}

#calculate Nr shown compliance in percent -- general
for(v in 1:nrow(firstPhaseComplianceDataFrame)){
  
  if(!is.na(match(firstPhaseComplianceDataFrame$FKID[v], unique(eligibleSubjects$PersonID)))){
    
  finalAnalysisFrame$FKID[v] <- firstPhaseComplianceDataFrame$FKID[v]
  finalAnalysisFrame$firstPhaseCompliance[v] <- round(100/(sum(!is.na(firstPhaseComplianceDataFrame[v, ])) -1) * sum(str_count(firstPhaseComplianceDataFrame[v,2:23], "2")))
  }
}

#### Generating secondPhase Compliance Values 

secondPhaseComplianceDataFrame <- select(sessionVariablesAnalysis, secondPhaseCompliance)

#get rid of empty rows
rowsToDrop <- NULL
for(y in 1:nrow(secondPhaseComplianceDataFrame)){
  
  if(sum(is.na(secondPhaseComplianceDataFrame[y, ])) >= 22){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
secondPhaseComplianceDataFrame  <- secondPhaseComplianceDataFrame[-rowsToDrop, ]

#inverseNegative Items
for(c in 1:ncol(secondPhaseComplianceDataFrame)){
  
  if(colnames(secondPhaseComplianceDataFrame[c]) %in% invertComplianceItems == TRUE){
    
    for(b in 1:nrow(secondPhaseComplianceDataFrame)){
      
      if(secondPhaseComplianceDataFrame[b,c] == 1 && !is.na(secondPhaseComplianceDataFrame[b,c])){
        
        secondPhaseComplianceDataFrame[b,c] <- 2 }
      
      else if(secondPhaseComplianceDataFrame[b,c] == 2 && !is.na(secondPhaseComplianceDataFrame[b,c])){
        
        secondPhaseComplianceDataFrame[b,c] <- 1}
    }
  }
}

#calculate Nr shown compliance in percent -- general
for(v in 1:nrow(secondPhaseComplianceDataFrame)){
  
  if(!is.na(match(secondPhaseComplianceDataFrame$FKID[v], unique(eligibleSubjects$PersonID)))){
    

    finalAnalysisFrame$secondPhaseCompliance[match(secondPhaseComplianceDataFrame$FKID[v], finalAnalysisFrame$FKID)] <- round(100/(sum(!is.na(secondPhaseComplianceDataFrame[v, ])) -1) * sum(str_count(secondPhaseComplianceDataFrame[v,2:15], "2")))
  }
}

#first/second Interest
firstPhaseInterest <- interestIndicatorsList[1:28]
secondPhaseInterest <- interestIndicatorsList[1]
secondPhaseInterest <-append(secondPhaseInterest, interestIndicatorsList[28:length(interestIndicatorsList)])

####################################################################################################################################################################

#First phase interest
firstPhaseInterestDataFrame <- select(sessionVariablesAnalysis, firstPhaseInterest)

#Delete FKID that did not answer any interest variables
rowsToDrop <- NULL
for(y in 1:nrow(firstPhaseInterestDataFrame)){
  
  if(sum(is.na(firstPhaseInterestDataFrame[y, ])) >= 27){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
firstPhaseInterestDataFrame <- firstPhaseInterestDataFrame[-rowsToDrop, ]


#calculate overall interest variable per participant relative to the number of chance to show it. --> Rounded 
for(v in 1:nrow(firstPhaseInterestDataFrame)){
  
  if(!is.na(match(firstPhaseInterestDataFrame$FKID[v], unique(eligibleSubjects$PersonID)))){
    
  
  finalAnalysisFrame$firstPhaseInterest[match(firstPhaseInterestDataFrame$FKID[v], finalAnalysisFrame$FKID)] <- round(100/(sum(!is.na(firstPhaseInterestDataFrame[v, ])) -1) * sum(str_count(firstPhaseInterestDataFrame[v,2:28], "2")))
}
}

#Second phase interest

secondPhaseInterestDataFrame <- select(sessionVariablesAnalysis, secondPhaseInterest)

#Delete FKID that did not answer any interest variables
rowsToDrop <- NULL
for(y in 1:nrow(secondPhaseInterestDataFrame)){
  
  if(sum(is.na(secondPhaseInterestDataFrame[y, ])) >= 27){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
secondPhaseInterestDataFrame <- secondPhaseInterestDataFrame[-rowsToDrop, ]


#calculate overall interest variable per participant relative to the number of chance to show it. --> Rounded 
for(v in 1:nrow(secondPhaseInterestDataFrame)){
  
  if(!is.na(match(secondPhaseInterestDataFrame$FKID[v], unique(eligibleSubjects$PersonID)))){
    
    
    finalAnalysisFrame$secondPhaseInterest[match(secondPhaseInterestDataFrame$FKID[v], finalAnalysisFrame$FKID)] <- round(100/(sum(!is.na(secondPhaseInterestDataFrame[v, ])) -1) * sum(str_count(secondPhaseInterestDataFrame[v,2:28], "2")))
  }
}

#relevant Variables from Subjects
eligibleSubjectsSWEfilter <-  eligibleSubjects[,c(1:5,7:12,50:56)]

# for the sake of simplicity just remove all none leaders from the list eligibleSubjectsSWEfilter
eligibleSubjectsSWEfilter <- eligibleSubjectsSWEfilter[eligibleSubjectsSWEfilter$isTeamLeader == "Yes",]

# convert time variables to actual readable
rowsToDrop <- NULL
for(a in 1:nrow(eligibleSubjectsSWEfilter)){
  eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[a] <-  paste(dmy(str_sub(eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[a], 1,10)), str_sub(eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[a], -5), sep=" ")
  
  #remove extreme values in items of swe - -1 
  if(eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[a] == 0.0){
     
     rowsToDrop <- c(rowsToDrop, a)}}

eligibleSubjectsSWEfilter <- eligibleSubjectsSWEfilter[-rowsToDrop, ]


rowsToDrop <- NULL
for(y in 1:nrow(finalAnalysisFrame)){
  
  if(sum(is.na(finalAnalysisFrame[y, ])) >= 13){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
finalAnalysisFrame <- finalAnalysisFrame[-rowsToDrop, ]


firstId <- "placeholder" 
isFirst <- TRUE
for(g in 1:nrow(eligibleSubjectsSWEfilter)){
  
  if(eligibleSubjectsSWEfilter$PersonID[g] != firstId){
    
      isFirst <- TRUE
      firstId <- eligibleSubjectsSWEfilter$PersonID[g]
  }
  
  if(sum(eligibleSubjectsSWEfilter$PersonID == eligibleSubjectsSWEfilter$PersonID[g]) == 2 && isFirst == TRUE){
    
      
          finalAnalysisFrame$`SWE Phase 1`[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <- eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g + 1] - eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g]
          finalAnalysisFrame$`timespan Phase 1 in days`[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <- difftime(eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g + 1], eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g], units = "days")
          finalAnalysisFrame$isTeamLeader[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <-  eligibleSubjectsSWEfilter$isTeamLeader[g]
          isFirst <- FALSE
          
          #handling of 1 or 2 phase swe and cronbach alpha variables calc. datafrmae
          finalAnalysisCronbachAlphaSWE$FKID[g] <- eligibleSubjectsSWEfilter$PersonID[g]
          finalAnalysisCronbachAlphaSWE$`Timestamp SWE`[g] <- eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g]
          finalAnalysisCronbachAlphaSWE$`Ph1 S125 1`[g] <- eligibleSubjectsSWEfilter$`$C_S125_01_FK_IC`[g]
          finalAnalysisCronbachAlphaSWE$`Ph1 S125 2`[g] <- eligibleSubjectsSWEfilter$`$C_S125_02_FK_IC`[g]
          finalAnalysisCronbachAlphaSWE$`Ph1 S125 3`[g] <- eligibleSubjectsSWEfilter$`$C_S125_03_FK_IC`[g]
          finalAnalysisCronbachAlphaSWE$`Ph1 S125 4`[g] <- eligibleSubjectsSWEfilter$`$C_S125_04_FK_IC`[g]
          finalAnalysisCronbachAlphaSWE$`Ph1 S125 5`[g] <- eligibleSubjectsSWEfilter$`$C_S125_05_FK_IC`[g]
          finalAnalysisCronbachAlphaSWE$`Ph1 S125`[g] <- eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g]
          
          
          finalAnalysisCronbachAlphaSWE$`Ph2 S125 1`[g] <- eligibleSubjectsSWEfilter$`$C_S125_01_FK_IC`[g+1]
          finalAnalysisCronbachAlphaSWE$`Ph2 S125 2`[g] <- eligibleSubjectsSWEfilter$`$C_S125_02_FK_IC`[g+1]
          finalAnalysisCronbachAlphaSWE$`Ph2 S125 3`[g] <- eligibleSubjectsSWEfilter$`$C_S125_03_FK_IC`[g+1]
          finalAnalysisCronbachAlphaSWE$`Ph2 S125 4`[g] <- eligibleSubjectsSWEfilter$`$C_S125_04_FK_IC`[g+1]
          finalAnalysisCronbachAlphaSWE$`Ph2 S125 5`[g] <- eligibleSubjectsSWEfilter$`$C_S125_05_FK_IC`[g+1]
          finalAnalysisCronbachAlphaSWE$`Ph2 S125`[g] <- eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g+1]
  }
  
  else if(sum(eligibleSubjectsSWEfilter$PersonID == eligibleSubjectsSWEfilter$PersonID[g]) == 3 && isFirst == TRUE && eligibleSubjectsSWEfilter$isTeamLeader[g] == "Yes"){
      
    
      finalAnalysisFrame$`SWE Phase 1`[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <- eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g + 1] - eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g]
      finalAnalysisFrame$`timespan Phase 1 in days`[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <- difftime(eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g + 1], eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g], units = "days")
      finalAnalysisFrame$`SWE Phase 2`[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <- eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g + 2] - eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g + 1]
      finalAnalysisFrame$`timespan Phase 2 in days`[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <- difftime(eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g + 2], eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g + 1], units = "days")
      finalAnalysisFrame$isTeamLeader[match(eligibleSubjectsSWEfilter$PersonID[g], finalAnalysisFrame$FKID)] <-  eligibleSubjectsSWEfilter$isTeamLeader[g]
      isFirst <- FALSE
      
      #handling of 1 or 2 phase swe and cronbach alpha variables calc. datafrmae
      finalAnalysisCronbachAlphaSWE$FKID[g] <- eligibleSubjectsSWEfilter$PersonID[g]
      finalAnalysisCronbachAlphaSWE$`Timestamp SWE`[g] <- eligibleSubjectsSWEfilter$`Timestamp Sc_125_FK`[g]
      finalAnalysisCronbachAlphaSWE$`Ph1 S125 1`[g] <- eligibleSubjectsSWEfilter$`$C_S125_01_FK_IC`[g]
      finalAnalysisCronbachAlphaSWE$`Ph1 S125 2`[g] <- eligibleSubjectsSWEfilter$`$C_S125_02_FK_IC`[g]
      finalAnalysisCronbachAlphaSWE$`Ph1 S125 3`[g] <- eligibleSubjectsSWEfilter$`$C_S125_03_FK_IC`[g]
      finalAnalysisCronbachAlphaSWE$`Ph1 S125 4`[g] <- eligibleSubjectsSWEfilter$`$C_S125_04_FK_IC`[g]
      finalAnalysisCronbachAlphaSWE$`Ph1 S125 5`[g] <- eligibleSubjectsSWEfilter$`$C_S125_05_FK_IC`[g]
      finalAnalysisCronbachAlphaSWE$`Ph1 S125`[g] <- eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g]
      
      
      finalAnalysisCronbachAlphaSWE$`Ph2 S125 1`[g] <- eligibleSubjectsSWEfilter$`$C_S125_01_FK_IC`[g+1]
      finalAnalysisCronbachAlphaSWE$`Ph2 S125 2`[g] <- eligibleSubjectsSWEfilter$`$C_S125_02_FK_IC`[g+1]
      finalAnalysisCronbachAlphaSWE$`Ph2 S125 3`[g] <- eligibleSubjectsSWEfilter$`$C_S125_03_FK_IC`[g+1]
      finalAnalysisCronbachAlphaSWE$`Ph2 S125 4`[g] <- eligibleSubjectsSWEfilter$`$C_S125_04_FK_IC`[g+1]
      finalAnalysisCronbachAlphaSWE$`Ph2 S125 5`[g] <- eligibleSubjectsSWEfilter$`$C_S125_05_FK_IC`[g+1]
      finalAnalysisCronbachAlphaSWE$`Ph2 S125`[g] <- eligibleSubjectsSWEfilter$`$C_S125_FK_IC`[g+1]
    
  }
}


# calculate completion continously on interest and compliance - indicating at the same time general completion of phases - the higher completion rate the more session made
# completion of interest and compliance taken together - mean value on both --- 54 possible interest variables - 36 possible compliance variables

    # Mean completion rate : compliance + #interest / 2  -- out of the tables interestDataFrame and complianceDataFrame - mean of first second compliance/interest rate
    for(y in 1:nrow(finalAnalysisFrame)){
      
      finalAnalysisFrame$completionIntensity[y] <- ((100/36 *(sum(!is.na(complianceDataFrame[match(finalAnalysisFrame$FKID[y], complianceDataFrame$FKID), ]))-1)) +  (100/54 *(sum(!is.na(interestDataFrame[match(finalAnalysisFrame$FKID[y], interestDataFrame$FKID), ]))-1))) /2
      finalAnalysisFrame$meanIntensityFirstPhase[y] <- (finalAnalysisFrame$firstPhaseCompliance[y] + finalAnalysisFrame$firstPhaseInterest[y]) /2
      finalAnalysisFrame$meanIntensitySecondPhase[y] <- (finalAnalysisFrame$secondPhaseCompliance[y] + finalAnalysisFrame$secondPhaseInterest[y]) /2
      
       }
    

#delte all rows with too many na's
rowsToDrop <- NULL
for(s in 1:nrow(finalAnalysisFrame)){
  
  if(is.na(finalAnalysisFrame$`SWE Phase 1`[s])){
    rowsToDrop <- c(rowsToDrop, s)
  }
}
finalAnalysisFrameCompleteSelfEfficacy <- finalAnalysisFrame[-rowsToDrop, ]



#creating some models: mutiple linear regression
# working with complete correlationDataFrameEligSub for first question N = 33
correlationDataFrameEligSubNaOm <- na.omit(correlationDataFrameEligSub[,c(1:12)])


adhaerenzModel <- lm(timeSpent ~ firstGesLeader + firstGesTeam + leadStyle + interestInPercent + complianceInPercent  , data = correlationDataFrameEligSubNaOm)
acceptModel <-  lm(acceptMean ~ firstGesLeader + firstGesTeam + leadStyle + interestInPercent + complianceInPercent, data = correlationDataFrameEligSubNaOm)


# working with complete correlationDataFrameEligSub for second question N = 15
correlationDataFrameEligSubNaOmSWE <- na.omit(correlationDataFrameEligSub)

DeltaSWEadhaerenzModel <- lm(DeltaSWE ~ Ph1.S125 + timeSpent + acceptMean, data = correlationDataFrameEligSubNaOmSWE)
DeltaSWEContext <-  lm(DeltaSWE ~ Ph1.S125 + firstGesLeader + firstGesTeam + leadStyle + interestInPercent + complianceInPercent, data = correlationDataFrameEligSubNaOmSWE)

############### --> Extra data frame for control group FKID: 726,789,790,791,1102,1091,1175,416,1099,1248,1241,1239,1247,1181,1228,1259,1332,1514,1525,1562,1563

#crude Frame with relevant variables
crudeControlGroupFrame <- sessionVariablesTeams

controlGroupFrame <- crudeControlGroupFrame[,c(1:5,7:11,48)]
controlGroupFrame <- controlGroupFrame[controlGroupFrame$isTeamLeader == "Yes",]
controlGroupFrame$PersonID <-  as.integer(gsub("^.{0,1}", "", controlGroupFrame$PersonID))

rowsToDrop <- NULL
for(m in 1:nrow(controlGroupFrame)){
  
  if(sum(is.na(controlGroupFrame[m, ])) > 0){
    rowsToDrop <- c(rowsToDrop, m)
    
  }
}
controlGroupFrame <- controlGroupFrame[-rowsToDrop, ]

#adjust time format
rowsToDrop <- NULL
for(a in 1:nrow(controlGroupFrame)){
  controlGroupFrame$`Timestamp Sc_125_FK`[a] <-  paste(dmy(str_sub(controlGroupFrame$`Timestamp Sc_125_FK`[a], 1,10)), str_sub(controlGroupFrame$`Timestamp Sc_125_FK`[a], -5), sep=" ")
}

controlGroupFrame <- as.integer(controlGroupFrame[,c(1:6,6:11)])

finalAnalysisFrameControlGrp <- data.frame(matrix(nrow = 1500, ncol= 6,byrow=TRUE))
colnames(finalAnalysisFrameControlGrp) <- c("FKID","isTeamLeader","∆ SWE Phase 1","timespan Phase 1 in days", "∆ SWE Phase 2","timespan Phase 2 in days")

### do the same for control group as for the experimental group
firstId <- "placeholder" 
isFirst <- TRUE
for(g in 1:52){
  
  if(controlGroupFrame$PersonID[g] != firstId){
    
    isFirst <- TRUE
    firstId <- controlGroupFrame$PersonID[g]
  }
  
  if(sum(controlGroupFrame$PersonID == controlGroupFrame$PersonID[g]) == 2 && isFirst == TRUE){
    
    finalAnalysisFrameControlGrp$FKID[g] <-  controlGroupFrame$PersonID[g]
    finalAnalysisFrameControlGrp$`∆ SWE Phase 1`[g] <- as.integer(controlGroupFrame$`$Q_S199`[g + 1]) - as.integer(controlGroupFrame$`$Q_S199`[g])
    finalAnalysisFrameControlGrp$`timespan Phase 1 in days`[g] <- difftime(controlGroupFrame$`Timestamp Sc_125_FK`[g + 1], controlGroupFrame$`Timestamp Sc_125_FK`[g], units = "days")
    finalAnalysisFrameControlGrp$isTeamLeader[g] <-  controlGroupFrame$isTeamLeader[g]
    isFirst <- FALSE
  }
  
  else if(sum(controlGroupFrame$PersonID == controlGroupFrame$PersonID[g]) == 3 && isFirst == TRUE && controlGroupFrame$isTeamLeader[g] == "Yes"){
    
    finalAnalysisFrameControlGrp$FKID[g] <-  controlGroupFrame$PersonID[g]
    finalAnalysisFrameControlGrp$`∆ SWE Phase 1`[g] <- as.integer(controlGroupFrame$`$Q_S199`[g + 1]) - as.integer(controlGroupFrame$`$Q_S199`[g])
    finalAnalysisFrameControlGrp$`timespan Phase 1 in days`[g] <- difftime(controlGroupFrame$`Timestamp Sc_125_FK`[g + 1], controlGroupFrame$`Timestamp Sc_125_FK`[g], units = "days")
    finalAnalysisFrameControlGrp$`∆ SWE Phase 2`[g] <- as.integer(controlGroupFrame$`$Q_S199`[g + 2]) - as.integer(controlGroupFrame$`$Q_S199`[g + 1])
    finalAnalysisFrameControlGrp$`timespan Phase 2 in days`[g] <- difftime(controlGroupFrame$`Timestamp Sc_125_FK`[g + 2], controlGroupFrame$`Timestamp Sc_125_FK`[g + 1], units = "days")
    finalAnalysisFrameControlGrp$isTeamLeader[g] <-  controlGroupFrame$isTeamLeader[g]
    isFirst <- FALSE
    
  }
}


#delete na rows once more
rowsToDrop <- NULL
for(y in 1:nrow(finalAnalysisFrameControlGrp)){
  
  if(sum(is.na(finalAnalysisFrameControlGrp[y, ])) >= 6){
    rowsToDrop <- c(rowsToDrop, y)
  }
}
finalAnalysisFrameControlGrp <- finalAnalysisFrameControlGrp[-rowsToDrop, ]


####################################################################################################################################################################################################
 # Results part calculations -> Final   // Relevant matrix here : correlationDataFrameEligSub   // work with winsorized variables finalcorrelationFrameWinsorized
####################################################################################################################################################################################################
finalcorrelationFrameWinsorizeCompleteSave <- finalcorrelationFrameWinsorized 


# Cronbach Alpha from sessionVariablesTeamsSave

#create List of all Relevant Items.

ItemList <- NULL
ItemList <- append(ItemList, "PersonID")

#GesLead 1-6
ItemList <- append(ItemList, c("$C_S130_01_FK_IC","$C_S130_02_FK_IC","$C_S130_03_FK_IC","$C_S130_04_FK_IC", "$C_S130_05_FK_IC","$C_S130_06_FK_IC"))
#GesSelf 1-6
ItemList <- append(ItemList,c("$C_S131_01_FK_IC","$C_S131_02_FK_IC","$C_S131_03_FK_IC","$C_S131_04_FK_IC", "$C_S131_05_FK_IC","$C_S131_06_FK_IC"))
#AccInfo 1-5
ItemList <- append(ItemList,c("$O_S188_01_FK_IC","$O_S188_02_FK_IC","$O_S188_03_FK_IC","$O_S188_04_FK_IC","$O_S188_05_FK_IC"))
#AccSyst 1-5
ItemList <- append(ItemList,c("$O_S188_06_FK_IC","$O_S188_07_FK_IC","$O_S188_08_FK_IC","$O_S188_09_FK_IC","$O_S188_10_FK_IC"))
#LeadStyle 1-8
ItemList <- append(ItemList,c("$C_S105_a4_FK_IC","$C_S105_b1_FK_IC","$C_S105_c1_FK_IC","$C_S105_d1_FK_IC","$C_S105_a5_FK_IC","$C_S105_b3_FK_IC","$C_S105_c3_FK_IC","$C_S105_d2_FK_IC"))

#take cronbach self-efficacy out of finalAnalysisCronbachAlphaSWE T0
cronbachTable <- sessionVariablesTeams[colnames(sessionVariablesTeams) %in% ItemList]
cronbachTable$PersonID <-  as.integer(gsub("^.{0,1}", "", cronbachTable$PersonID))

#Information from eligible FKID
cronbachTable <- cronbachTable[cronbachTable$PersonID %in% as.list(finalcorrelationFrameWinsorized$FKID),]

#cronbach alphas tables...

GesLeadAlpha <- as.data.frame(lapply(GesLeadAlpha, as.numeric))
glAlpha <- cronbach(GesLeadAlpha)

GesSelfAlpha  <- as.data.frame(lapply(GesSelfAlpha, as.numeric))
gsAlpha <- cronbach(GesSelfAlpha)

AccInfoAlpha <- as.data.frame(lapply(AccInfoAlpha, as.numeric))
accIAlpha <- cronbach(AccInfoAlpha)

AccSystAlpha <- as.data.frame(lapply(AccSystAlpha, as.numeric))
accSAlpha <- cronbach(AccSystAlpha)

LeadStyleAlpha <- as.data.frame(lapply(LeadStyleAlpha, as.numeric))
leadAlpha <- cronbach(LeadStyleAlpha)

#Cronbach Alpha T1: 
selfEfALpha <- cronbach(finalAnalysisCronbachAlphaSWE[,3:7])

# final Correlation matrix (1/2)
prozessFaktorenOhneSelfEfficacy <- finalcorrelationFrameWinsorized[,c(8,9,12,10,11,4,5,7,2,3)]
prozessFaktorenOhneSelfEfficacy <- na.omit(prozessFaktorenOhneSelfEfficacy)

corr.test(prozessFaktorenOhneSelfEfficacy)

# final Correlation matrix (2/2)
prozessFaktorenMitSelfEfficacy <- finalcorrelationFrameWinsorized[,c(10,11,14,13,4,5,7)]
prozessFaktorenMitSelfEfficacy <- na.omit(prozessFaktorenMitSelfEfficacy)

corr.test(prozessFaktorenMitSelfEfficacy)


# Mittelwert, Standartabweichung, Schiefe, Kurtosis --> use kurtosi() and skew()


# Analyse Verteilung der Sitzungen im Verlauf use: eligibleSubjectsCompletedSessionsPlotFilteredHours n =82

CompletedSessionPlotGraphFinal <- eligibleSubjectsCompletedSessionsPlotFilteredHours

CompletedSessionPlotGraph60Days <- eligibleSubjectsCompletedSessionsPlotFilteredHours[eligibleSubjectsCompletedSessionsPlotFilteredHours$timeDifferenceToZero <= 60,]

#correction of startpoint with two ID's
FKID1595 <- data.frame(session= "A1",timeDifferenceToZero= 0.00,timeSpent=mean(CompletedSessionPlotGraph60Days$timeSpent), FKID=1595)
FKID1090 <- data.frame(session= "A1",timeDifferenceToZero= 0.00,timeSpent=mean(CompletedSessionPlotGraph60Days$timeSpent), FKID=1090)
CompletedSessionPlotGraph60Days <- rbind(CompletedSessionPlotGraph60Days,FKID1595)
CompletedSessionPlotGraph60Days <- rbind(CompletedSessionPlotGraph60Days,FKID1090)

#only sessions A1-B5 the firs 2 month
CompletedSessionPlotGraph60DaysAB5 <- CompletedSessionPlotGraph60Days[!CompletedSessionPlotGraph60Days$session %in% c("C1","C2","C3","C4","C5","D1","D2","D3","D4","D5"),]



finalCompletedGraph <-  ggplot(CompletedSessionPlotGraph60DaysAB5, aes(CompletedSessionPlotGraph60DaysAB5$timeDifferenceToZero, CompletedSessionPlotGraph60DaysAB5$session)) + xlim(0,60) +
  geom_point(colour="red1", size=0.8) + labs(x = "Tage", y = "Sitzungs-Nr") +  geom_line(aes(group = FKID), lty = 5, colour = "black", size=0.2)


StartedSessionPlotGraph60Days <- eligibleSubjectsOnlyStartedSessionsPlot[eligibleSubjectsOnlyStartedSessionsPlot$timeDifferenceToZero <= 60,]

StartedSessionPlotGraph60DaysAB5 <- StartedSessionPlotGraph60Days [!StartedSessionPlotGraph60Days$session %in% c("C1","C2","C3","C4","C5","D1","D2","D3","D4","D5"),]


finalStartedGraph <-  ggplot(StartedSessionPlotGraph60DaysAB5, aes(StartedSessionPlotGraph60DaysAB5$timeDifferenceToZero, StartedSessionPlotGraph60DaysAB5$session)) + xlim(0,60) +
  geom_point(colour="dodgerblue4", size=1) + labs(x = "Tage", y = "Sitzungs-Nr") + geom_line(aes(group = FKID), lty = 5, colour = "black", size=0.2)



##########Clusteranalysen der diversen Studienvariablen  - access different clusters through finalcorrelationFrameWinsorized[adherenceClusters$cluster ==1,]

# Clusternanalyse objektive Prozessfaktor: Adhärenz lösung : tiefe, mittel, hohe Adhärenz

adherenceClusters <- kmeans(finalcorrelationFrameWinsorized[,c(4,5,7)] , 3, nstart = 50, iter.max = 15)


# Clusternanalyse objektive Prozessfaktor: Interest, Fügsamkeit lösung : tiefes, mitteleres, hohes Interess / Fügsamkeit

interestClusters <-  kmeans(finalcorrelationFrameWinsorized[,2] , 3, nstart = 50, iter.max = 15)
complianceCluster <- kmeans(finalcorrelationFrameWinsorized[,3] , 3, nstart = 50, iter.max = 15)

# Clusternanalyse subjektive Prozessfaktor: Systemakzeptanz lösung : tief, hoch - Aufgrund der Cluster grössen (Mittel hätte nur 6 ID's)

akzeptanzSystemInfo <- kmeans(na.omit(finalcorrelationFrameWinsorized[,c(10,11)]) , 3, nstart = 50, iter.max = 15)


# Clusternanalyse Selbstwirksamkeit: lösung : tief+, mittel++, hoch- / Selbstwirksamkeit

selbstwirksamkeitT0 <- kmeans(na.omit(finalcorrelationFrameWinsorized[,c(14,13)]) , 3, nstart = 50, iter.max = 15)
helpTable2 <- na.omit(finalcorrelationFrameWinsorized[,c(14,13)])


#elbow- Method
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- na.omit(finalcorrelationFrameWinsorized[,3])
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Anzahl K-Clusterloesungen",
     ylab="Summe der quadrierten Abweichungen")



#Fisher's exact test tells you only about the probability of your or "more extreme"
#data given that the null hypothesis holds. It does not tell you about where in the table your data deviate from independence.
#Pearson residuals are one way of expressing that. Try chisq.test(x)$residuals to obtain them. 
#Larger absolute values correspond to greater deviance from what would be expected under the null
#hypothesis, positive values indicate "more than expected", negative values "less than expected".
#They can be visualized by mosaicplot(x, shade=T)
#If you square them and sum them up, you get Pearson's Chi-squared statistic. 
#They partition this statistic, making visible how much each table entry contributes to it.

########### Abhängigkeitsanalysen: Kontingenztabellen für Chi square Tests

# Interesse * Adhärenz
sum(finalcorrelationFrameWinsorized[adherenceClusters$cluster ==3,]$FKID %in% finalcorrelationFrameWinsorized[interestClusters$cluster ==1,]$FKID)

contigencyInterestAdherence <- c(13,12,2,9)

dim(contigencyInterestAdherence) <- c(2,2)

contigencyInterestAdherenceChi <- c(13,21,12,7,10,4,2,6,9)

dim(contigencyInterestAdherenceChi) <- c(3,3)

## Zu kleine Sample size fischer exact test machen fisher.test(contigencyInterestAdherence[2:4]) --> Tibble tabelle in data.frame umwandeln und erste Spalte auslassen - Adhärenz - Interest sind unabhängig H0 kann nicht verworfen werden
fischerAdherenceInterest <- fisher.test(contigencyInterestAdherence)

# Fügsamkeit * Adhärenz
sum(finalcorrelationFrameWinsorized[adherenceClusters$cluster ==3,]$FKID %in% finalcorrelationFrameWinsorized[complianceCluster$cluster ==2,]$FKID)


contigencyComplianceAdherence <- c(9,28,1,0)
dim(contigencyComplianceAdherence) <- c(2,2)

contigencyComplianceAdherenceChi <- c(9,9,28,2,10,9,1,16,0)
dim(contigencyComplianceAdherenceChi) <- c(3,3)

ChiAdherenceCompliance <- chisq.test(contigencyComplianceAdherenceChi)


# multinomialen logistischen Regressionsanalyse  -- FKID (mit ihren Ges und Führungsvalues) müssen zugeteilt werden zu L M H Adhärenz 

#logit Data frame

logitgesLead <- na.omit(finalcorrelationFrameWinsorized[c(1,8,9,12)])

logitgesLead[, "gesMean"] <- (logitgesLead$firstGesLeader + logitgesLead$firstGesTeam)/2
logitgesLead <- logitgesLead[,-c(2,3)]

logitgesLead[,"categoryAdherence"] <- NA
logitgesLead[,"categorySystemacceptance"] <- NA

# Next step add categories to each fkid
logitgesLead[logitgesLead$FKID %in% logitgesLead[akzeptanzSystemInfo$cluster ==3,]$FKID,]$categorySystemacceptance <- 3

#convert factor variables to factor

logitgesLead$categoryAdherence <- factor(logitgesLead$categoryAdherence)
logitgesLead$categorySystemacceptance <- factor(logitgesLead$categorySystemacceptance)

# referenz Kategorie = Normal adherence = 1 -- tief - most normative 
logitgesLead$outAdherence <- relevel(logitgesLead$categoryAdherence, ref = "1")

# Develop multinomial logistic regression model 
library(nnet)

gesLeadAdherenceModel <- multinom(outAdherence ~ gesMean + leadStyle, data = logitgesLead)

summary(gesLeadAdherenceModel)

#2- tailed z Test - Adherence 

zAdherence <- summary(gesLeadAdherenceModel)$coefficients/summary(gesLeadAdherenceModel)$standard.errors

pAdherence <- (1- pnorm(abs(zAdherence),0 , 1))*2

# referenz Kategorie = Normal acceptance = 2 -- mittel - most normative 
logitgesLead$outAcceptance <- relevel(logitgesLead$categorySystemacceptance, ref = "2")

# Develop multinomial logistic regression model
gesLeadAcceptanceModel <- multinom(outAcceptance ~ gesMean + leadStyle, data = logitgesLead)

summary(gesLeadAcceptanceModel)

#2- tailed z Test - Acceptance

zAcceptance <- summary(gesLeadAcceptanceModel)$coefficients/summary(gesLeadAcceptanceModel)$standard.errors

pAcceptance <- (1- pnorm(abs(zAcceptance),0 , 1))*2


## Chi square test Acceptance and adherence 

# Systemakzeptanz * Adhärenz
naOmitAccAdhaerenz <- na.omit(finalcorrelationFrameWinsorized[,c(1,4,5,7,10,11)])

sum(naOmitAccAdhaerenz[adherenceClusters$cluster ==2,]$FKID %in% naOmitAccAdhaerenz[akzeptanzSystemInfo$cluster ==1,]$FKID)

contigencyAcceptanceAdherence <- c(13,9,4,7,7,5)

dim(contigencyAcceptanceAdherence) <- c(2,3)


chisquareAccAdh <-  chisq.test(contigencyAcceptanceAdherence)

## Chi square test Acceptance and change in self efficacy 

# Systemakzeptanz * Selbstwirksamkeit

naOmitAccSelfEfficacy <- na.omit(finalcorrelationFrameWinsorized[,c(1,10,11,13,14)])

sum(naOmitAccSelfEfficacy[selbstwirksamkeitT0$cluster ==1,]$FKID %in% naOmitAccSelfEfficacy[akzeptanzSystemInfo$cluster ==1,]$FKID)

contigencyAcceptanceSelf <- c(5,0,2,9)

dim(contigencyAcceptanceSelf) <- c(2,2)

chiTestAccSElf <- chisq.test(contigencyAcceptanceSelf)

## Chi square test Acceptance and change in self efficacy 

# Adhärenz * Selbstwirksamkeit

naOmitAdherenceSelfEfficacy <- na.omit(finalcorrelationFrameWinsorized[,c(1,4,5,7,13,14)])

sum(naOmitAdherenceSelfEfficacy[selbstwirksamkeitT0$cluster ==1,]$FKID %in% naOmitAdherenceSelfEfficacy[adherenceClusters$cluster ==3,]$FKID)

contigencyAdherenceanceSelf <- c(4,4,2,6)

dim(contigencyAdherenceanceSelf ) <- c(2,2)

naOmitAdherenceSelfEfficacy[selbstwirksamkeitT0$cluster ==3,]$FKID
