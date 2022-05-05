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


# Sep by Exp Status
# Remove Missing
# BRFSS2018_append$CAREGIV1[BRFSS2018_append$CAREGIV1 == 9] <- NA


### Subset into 2 dataframes?? - Decided to keep as one dataframe below commented code
# Caregive 
#Caregive18 <- BRFSS2018_append %>% filter(BRFSS2018_append$CAREGIV1 == 1) # are you caregiver
#Caregive18 <- Caregive18 %>% filter(Caregive18$CRGVLNG1 > 1) # over 30 days

#options(survey.lonely.psu = "adjust")
#bdCaregive <- svydesign(data = Caregive18, id = ~X_PSU, strata = ~X_STSTR,
                      #  weight = ~X_LLCPWT, nest = TRUE)

#svymean(~X_AGE80, bdCaregive)
#svytable(~X_AGEG5YR,bdCaregive)

# NonCaregive Subset
#NonCaregive <- BRFSS2018_append %>% filter(BRFSS2018_append$CAREGIV1 > 1)
#CaregiveU30 <- BRFSS2018_append %>% filter(BRFSS2018_append$CRGVLNG1 == 1)
#NonCaregive18 <- bind_rows(NonCaregive,CaregiveU30)

#bdNonCaregive <- svydesign(data = NonCaregive18, id = ~X_PSU, strata = ~X_STSTR,
                         #  weight = ~X_LLCPWT, nest = TRUE)

#svymean(~X_AGE80, bdNonCaregive)
#svytable(~X_AGEG5YR,bdNonCaregive)

###Possible variable list?
# Education (did not graduate high school, graduated high school, attended college or technical school, graduated from college or technical school)
# Employment (currently employed for wages, self-employed, out of work for at least 1 year, out of work for less than 1 year, a homemaker, a student, retired, or unable to work)
# Income/SES ($0-$14,999, $15,000-$24,999, $25,000-$34,999, $35,000-$49,999, $50,0000)
# marital status?
# children in household?
# State political affiliation?
# insurance status y/n
# Type of insurance (employer, individual buy in, Medicare, Medicaid, military, AI/AN services, or some other source)
# last checkup (within the past year, within the past two years, within the past five years, or five or more years ago).
# Up-to-date checkup (see their primary care provider (PCP) every 3 years and those over 50 years see their PCP every year)
# Influence vaccine in past 12 months
# Self-rated general health (Excellent/Very good, good, fair, poor)
# days in poor mental or physical health?
#health behaviors?
# Smoking status?
#BMI?
#Physical activity?
#sleep?
# alcohol consumption
#daily servings of fruits of vegetables?
#caregiving variables?
#relationship to recipient?
#duration?
#hours per week?

# Sep by Exp Status
# Remove Missing
BRFSS2018_append$CAREGIV1[BRFSS2018_append$CAREGIV1 == 9] <- NA
BRFSS2018_append$X_EDUCAG[BRFSS2018_append$X_EDUCAG == 9] <- NA
BRFSS2018_append$FLUSHOT6[BRFSS2018_append$FLUSHOT6 == 7] <- NA
BRFSS2018_append$FLUSHOT6[BRFSS2018_append$FLUSHOT6 == 9] <- NA
BRFSS2018_append$FLUSHOT6[BRFSS2018_append$FLUSHOT6 == ""] <- NA
BRFSS2018_append$MARITAL[BRFSS2018_append$MARITAL == ""] <- NA
BRFSS2018_append$MARITAL[BRFSS2018_append$MARITAL == 9] <- NA
BRFSS2018_append$GENHLTH[BRFSS2018_append$GENHLTH == 7] <- NA
BRFSS2018_append$GENHLTH[BRFSS2018_append$GENHLTH == 9] <- NA
BRFSS2018_append$GENHLTH[BRFSS2018_append$GENHLTH == ""] <- NA

BRFSS2018_append$X_AGE65YR[BRFSS2018_append$X_AGE65YR == 3] <- NA
BRFSS2018_append$EMPLOY1[BRFSS2018_append$EMPLOY1 == 9] <- NA

BRFSS2018_append$CHECKUP1[BRFSS2018_append$CHECKUP1 == 7] <- NA
BRFSS2018_append$CHECKUP1[BRFSS2018_append$CHECKUP1 == 9] <- NA

BRFSS2018_append$HLTHPLN1[BRFSS2018_append$HLTHPLN1 == 7] <- NA
BRFSS2018_append$HLTHPLN1[BRFSS2018_append$HLTHPLN1 == 9] <- NA

BRFSS2018_append$MENTHLTH[BRFSS2018_append$MENTHLTH == 88] <- 0
BRFSS2018_append$MENTHLTH[BRFSS2018_append$MENTHLTH == 77] <- NA
BRFSS2018_append$MENTHLTH[BRFSS2018_append$MENTHLTH == 99] <- NA

#create variable for caregivers who have given care for more than 30 days
BRFSS2018_append$CareCat <- 0
BRFSS2018_append$CareCat[BRFSS2018_append$CAREGIV1 ==1 & BRFSS2018_append$CRGVLNG1 > 1] <- 1

options(survey.lonely.psu = "adjust")

#create weighted data frame
bd <- svydesign(data = BRFSS2018_append, id = ~X_PSU, strata = ~X_STSTR,
                weight = ~X_LLCPWT, nest = TRUE)

# example of prop.table
#prop.table(svytable(~X_AGE65YR + CareCat, design = bd), margin = 2)

# example of function mean
#svyby(~X_AGE80, by = ~CareCat, design = bd, FUN = svymean, na.rm = TRUE)

###Education 
summary(BRFSS2018_append$X_EDUCAG)

#Table for Caregivers
svytable(~X_EDUCAG + CareCat,bd, exclude='null', na.action=na.pass)
prop.table(svytable(~X_EDUCAG + CareCat, bd, exclude='null', na.action=na.pass), margin = 2) *100


### Flu Shot
# 1=yes, 2=no
summary(BRFSS2018_append$FLUSHOT6)

svytable(~FLUSHOT6 + CareCat,bd, exclude='null', na.action=na.pass)
prop.table(svytable(~FLUSHOT6 + CareCat, bd, exclude='null', na.action=na.pass), margin = 2) *100


### Marital
summary(BRFSS2018_append$MARITAL)

svytable(~MARITAL + CareCat,bd, exclude='null', na.action=na.pass)
prop.table(svytable(~MARITAL + CareCat, bd, exclude='null', na.action=na.pass), margin = 2) *100

###GenHlth
summary(BRFSS2018_append$GENHLTH)

svytable(~GENHLTH + CareCat,bd, exclude='null', na.action=na.pass)
prop.table(svytable(~GENHLTH + CareCat, bd, exclude='null', na.action=na.pass), margin = 2) *100

# Mean age
svyby(~X_AGE80, by = ~CareCat, design = bd, FUN = svymean, na.rm = TRUE)

# Age 
svytable(~X_AGE65YR + CareCat, design = bd, exclude = 'null', na.action=na.pass)

# Employment
svytable(~EMPLOY1 + CareCat, design = bd, exclude = 'null', na.action=na.pass)

# Health Checkup
svytable(~CHECKUP1 + CareCat, design = bd, exclude = 'null', na.action=na.pass)

# Health Insurance
svytable(~HLTHPLN1 + CareCat, design = bd,  exclude = 'null', na.action=na.pass)

# Poor Mental Health Days
svyby(~MENTHLTH, by = ~CareCat, design = bd, FUN = svymean, na.rm = TRUE)

#income 

svytable(~X_INCOMG + CareCat,bd, exclude='null', na.action=na.pass)

prop.table(svytable(~X_INCOMG + CareCat, bd, exclude='null', 
                    na.action=na.pass), margin = 2)*100


##dr visits in last 12 months (DRVISITS)

svyby(~DRVISITS, by = ~CareCat, design = bd, FUN = svymean, na.rm = TRUE)


