library(tidyverse)
library(ggplot2)
library(proto) 
library(gsubfn)
library(readr)
library(anytime)

#Universität Zürich EBPI
#Pascal Misala, 2019
#Version: 1.0

#This script serves to easily filter wanted variables for later conversion

#import original file

dataPath <- "~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/Live Datensatz/Updated Live Datensatz/session_variables.csv"


trialdata <- read_delim(dataPath, 
                        ";", escape_double = FALSE, trim_ws = TRUE, na = "NA")

# order ID's ascending 
trialdata <- trialdata[order(trialdata$FKID),] 

variableList <-  "FKID"
variableList <- append(variableList, unique(trialdata$Variable))

#creating a dataFrame with unique variable names as columns / name them -- fill out in future loop (through WeCoachDataScript)
sessionVariables <- data.frame(matrix(nrow = 1, ncol= length(variableList) + 1,byrow=TRUE))

colnames(sessionVariables) <- variableList


#filtering/excluding unwanted variable types

#Plan: import up to date list of all variables, create lists and exclude all variables not wanted - merge those lists and intersect it with the list of variables from the actual datapoints (session variables)


# Export this process to another file maybe --> filter file is external anyways and doesnt need to be computed

#What follows here is the build up of reference list of all existing variables - The rough constellation of the wanted variables can be chosen here

#B- Variables

Bvariables <- read_csv("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/ReferenzVariables/Coach Variables - B_Variables.csv", na = "NA")

# Exclude list B-Variables (these are getting excluded)

Bexclude <- unique(Bvariables$X1)

#Q- Variables

QVariables <-  read_delim("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/ReferenzVariables/Coach Variables - Q_Variables.csv", 
                          ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

# Delete all rows with scale,factor,index,subscale -- Logic => d<-d[!(d$A=="B" & d$E==0),]  - Remove those variables from the list that needs to stay

QVariables <- QVariables[!(QVariables$Type == "Scale" | QVariables$Type == "Subscale" | QVariables$Type == "Index" ), ]
QVariables <- QVariables[!(QVariables$Type == "Factor"), ]

# Exclude list Q-Variables

Qexclude <- unique(QVariables$Variable)

#C- Variables

CVariables <-  read_delim("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/ReferenzVariables/Coach Variables - C_Variables.csv", 
                          ";", escape_double = FALSE, na = "NA",trim_ws = TRUE)

CVariables <- CVariables[!(CVariables$Type == "Scale"), ]
CVariables <- CVariables[!(CVariables$Type == "needed"), ]
CVariables <- CVariables[!(CVariables$Type == "Value"), ]
CVariables <- CVariables[!(CVariables$Type == "Subscale"), ]

# Exclude list C-Variables

Cexclude <- unique(CVariables$Variable)


#O- Variables  // special case $O_SESSION_[XY]_FREQ Variables... item, Values$text

OVariables <-  read_delim("~/Dropbox/Master/FS20/Masterarbeit/Datensatz Auswertung/ReferenzVariables/Coach Variables - O_Variables.csv", 
                          ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

OVariablesA <- OVariables[(OVariables$`Type / Indicator` == "Item"), ]
OVariablesB <- OVariables[(OVariables$Values =="Text"), ]

OVariables <- rbind(OVariablesA, OVariablesB)

# Exclude list O-Variables

Oexclude <- unique(OVariables$Variable)

# Next steps take care of $MFE_JD_4_1Prio/Who/When variables .. that for sessons variables first -- then team variables

toExcludePrio <- grep(c("Prio"), names(sessionVariables), value=TRUE)
toExcludeWhen <- grep(c("When"), names(sessionVariables), value=TRUE)
toExcludeWho <- grep(c("Who"), names(sessionVariables), value=TRUE)


#rbind all lists -- with all unique Variables that can be exluded from the dataset

toExcludeVariables <- c(Bexclude,Cexclude,Qexclude,Oexclude, toExcludePrio,toExcludeWhen,toExcludeWho)

toExcludeVariablesDataFrame <- as.data.frame(toExcludeVariables)

#write an external reusable file - in which one can add or leave-out future variables
write.csv(toExcludeVariables, file = "toExcludeVariables.csv")
