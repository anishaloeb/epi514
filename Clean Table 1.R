library("tidyverse")
library("haven")
library("boot")
library("survey")

# Read in data
BRFSS18v3 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS18v3.csv")
BRFSS18v1 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS18v1.csv")
BRFSS18v2 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS18v2.csv")
BRFSS2018 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS2018.csv")

# Correcting LLCPTWT
colnames(BRFSS18v1)[colnames(BRFSS18v1) == "X_LCPWTV1"] <- "X_LLCPWT"
colnames(BRFSS18v2)[colnames(BRFSS18v2) == "X_LCPWTV2"] <- "X_LLCPWT"
colnames(BRFSS18v3)[colnames(BRFSS18v3) == "X_LCPWTV3"] <- "X_LLCPWT"

# Creating version names
BRFSS2018$VERSION <- 0
BRFSS18v1$VERSION <- 1
BRFSS18v2$VERSION <- 2
BRFSS18v3$VERSION <- 3

# Append data
BRFSS2018_append <- bind_rows(BRFSS18v1, BRFSS18v2)
BRFSS2018_append  <- bind_rows(BRFSS2018_append, BRFSS2018)
BRFSS2018_append  <- bind_rows(BRFSS2018_append , BRFSS18v3)

# Set states to keep
BRFSS2018_append <- BRFSS2018_append[BRFSS2018_append$X_STATE %in% 
                                       c(13, 34, 36, 39, 41),]
# Keep only females
BRFSS2018_append <- BRFSS2018_append %>% filter(BRFSS2018_append$SEX1 == 2)

# Keep 21-75 years old
BRFSS2018_append <- BRFSS2018_append %>% filter(BRFSS2018_append$X_AGE80 >= 25 
                                                & BRFSS2018_append$X_AGE80 <= 75)

# Set vars to keep

keepVars <- c("X_AGE65YR", "X_STATE", "X_PSU", "MENTHLTH", "HLTHPLN1", "CHECKUP1",
              "MARITAL", "QSTVER", "CELLFON4", "MEDCOST", "EMPLOY1", "HLTHCVR1", 
              "X_EDUCAG", "X_LLCPWT", "X_STSTR", "X_INCOMG", "FLUSHOT6", "GENHLTH",
              "POORHLTH", "PHYSHLTH", "HADPAP2", "LASTPAP2", "HPVTEST", "HADMAM", 
              "HOWLONG", "CAREGIV1", "CRGVLNG1", "LSTCOVRG", "DRVISITS", "CRGVHRS1",
              "X_CHLDCNT", "X_AGEG5YR", "X_AGE80", "X_AGE_G", "X_BMI5CAT", "X_RFPAP34",
              "X_MAM5022", "SEX1")

BRFSS2018_append <- BRFSS2018_append[, keepVars]
write.csv(BRFSS2018_append, "Desktop/BRFSS2018_appendv2.csv", row.names = FALSE)

# Ben Bryer
# EPI514 Table 1
# 5/2/22

#load data
rm(list = ls())
setwd("C:/Users/bryer/OneDrive/Documents/UW/Spring22/EPI514")

library("tidyverse")
library("haven")
library("boot")
library("survey")

BRFSS2018_append <- read.csv("BRFSS2018_appendv2.csv", stringsAsFactors=T)
View(BRFSS2018_append)

# Sep by Exp Status
# Remove Missing for caregiving
BRFSS2018_append$CAREGIV1[BRFSS2018_append$CAREGIV1 == 9] <- NA
BRFSS2018_append$CAREGIV1[BRFSS2018_append$CAREGIV1 == 7] <- NA
BRFSS2018_append$CAREGIV1[BRFSS2018_append$CAREGIV1 == 8] <- NA
BRFSS2018_append$CAREGIV1[BRFSS2018_append$CAREGIV1 == ""] <- NA

#Remove missing for variables
BRFSS2018_append$X_EDUCAG[BRFSS2018_append$X_EDUCAG == 9] <- NA
BRFSS2018_append$MARITAL[BRFSS2018_append$MARITAL == ""] <- NA
BRFSS2018_append$MARITAL[BRFSS2018_append$MARITAL == 9] <- NA
BRFSS2018_append$GENHLTH[BRFSS2018_append$GENHLTH == 7] <- NA
BRFSS2018_append$GENHLTH[BRFSS2018_append$GENHLTH == 9] <- NA
BRFSS2018_append$GENHLTH[BRFSS2018_append$GENHLTH == ""] <- NA


#create variable for caregivers who have given care for more than 30 days

#Try out new CareCat1
BRFSS2018_append$CareCat<- NA
BRFSS2018_append$CareCat[BRFSS2018_append$CAREGIV1 ==2] <- 0
BRFSS2018_append$CareCat[BRFSS2018_append$CAREGIV1 ==1 & BRFSS2018_append$CRGVLNG1 == 1] <- 0
BRFSS2018_append$CareCare[BRFSS2018_append$CAREGIV1 ==1 & BRFSS2018_append$CRGVLNG1 > 1] <- 1
table(BRFSS2018_append$CareCat, useNA = "always") # raw



options(survey.lonely.psu = "adjust")

#create weighted data frame
bd <- svydesign(data = BRFSS2018_append, id = ~X_PSU, strata = ~X_STSTR,
                weight = ~X_LLCPWT, nest = TRUE)

###Education 
summary(BRFSS2018_append$X_EDUCAG)

table(BRFSS2018_append$X_EDUCAG, BRFSS2018_append$CareCat, useNA = "always") # raw
prop.table(svytable(~X_EDUCAG + CareCat, bd, exclude='null', na.action=na.pass), margin = 2) *100



### Marital
summary(BRFSS2018_append$MARITAL)

table(BRFSS2018_append$MARITAL, BRFSS2018_append$CareCat, useNA = "always") # raw
prop.table(svytable(~MARITAL + CareCat, bd, exclude='null', na.action=na.pass), margin = 2) *100

###GenHlth
summary(BRFSS2018_append$GENHLTH)

table(BRFSS2018_append$GENHLTH, BRFSS2018_append$CareCat, useNA = "always") # raw
prop.table(svytable(~GENHLTH + CareCat, bd, exclude='null', na.action=na.pass), margin = 2) *100





