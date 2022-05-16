BRFSS2018_append <- read.csv("Desktop/BRFSS2018_append_CareCat&Missing.csv") #missing and CareCat already there

BRFSS2018_append$FLUSHOT6[BRFSS2018_append$FLUSHOT6 == 7] <- NA
BRFSS2018_append$FLUSHOT6[BRFSS2018_append$FLUSHOT6 == 9] <- NA

BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 1] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 2] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 3] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 4] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 8] <- 2

BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 0] <- 2
BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 1] <- 1

# Prevalence between Caregivers and Non Caregivers

prop.table(table(BRFSS2018_append$X_RFPAP34,BRFSS2018_append$CareCatFac, deparse.level = 2), margin = 2)

prop.table(table(BRFSS2018_append$X_MAM5022,BRFSS2018_append$CareCatFac, deparse.level = 2), margin = 2)

prop.table(table(BRFSS2018_append$FLUSHOT6,BRFSS2018_append$CareCatFac, deparse.level = 2), margin = 2)

prop.table(table(BRFSS2018_append$CheckupCat,BRFSS2018_append$CareCatFac, deparse.level = 2), margin = 2)

# Unadjusted Associations

(rrTab <- table(BRFSS2018_append$X_RFPAP34, BRFSS2018_append$CareCatFac, deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$X_MAM5022, BRFSS2018_append$CareCatFac, deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

BRFSS2018_append$ageCat <- factor(BRFSS2018_append$X_AGEG5YR,
                                  labels = c("25-29",
                                             "30-34",
                                             "35-39",
                                             "40-44",
                                             "45-49",
                                             "50-54",
                                             "55-59",
                                             "60-64",
                                             "65-69",
                                             "70-74")
)

# Age Adjusted Pap 
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, CareCatFac, ageCat)))

(epi.2by2(strat1[1:2, 1:2, 1:9])) # excluding 70-74

# Age Adjusted Mammogram
(strat2 <- with(BRFSS2018_append,
                table(X_MAM5022, CareCatFac, ageCat)))

(epi.2by2(strat2[1:2, 1:2, 6:10])) # excluding age cats < 50

# Age adjusted Flushot

(strat3 <- with(BRFSS2018_append,
                table(FLUSHOT6, CareCatFac, ageCat)))
(epi.2by2(strat3))

# Age adjusted Checkup

(strat4 <- with(BRFSS2018_append,
                table(CheckupCat, CareCatFac, ageCat)))
(epi.2by2(strat4))

# Education adjusted Pap
BRFSS2018_append$eduCat <- factor(BRFSS2018_append$X_EDUCAG,
                                  labels = c("Did not graduate High School",
                                             "Graduated High School",
                                             "Attended College or Technical School",
                                             "Graduated from College or Technical School"))
(strat1 <- with(BRFSS2018_append,
               table(X_RFPAP34, CareCatFac, eduCat)))
(epi.2by2(strat1))

# Education adjusted Mammogram
(strat2 <- with(BRFSS2018_append,
                table(X_MAM5022, CareCatFac, eduCat)))
(epi.2by2(strat2))

# Education adjusted Flushot
(strat3 <- with(BRFSS2018_append,
                table(FLUSHOT6, CareCatFac, eduCat)))
(epi.2by2(strat3))

# Education adjusted Checkup
(strat4 <- with(BRFSS2018_append,
                table(CheckupCat, CareCatFac, eduCat)))
(epi.2by2(strat4))

### Effect modification
# Insurance
  # Pap
(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, X_RFPAP34)))
(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, X_RFPAP34)))
(epi.2by2(strat2))

  # Mammogram
(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, X_MAM5022)))
(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, X_MAM5022)))
(epi.2by2(strat2))

  # Health Checkup
(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, CheckupCat)))
(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, CheckupCat)))
(epi.2by2(strat2))

# Flu shot
(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, FLUSHOT6)))
(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, FLUSHOT6)))
(epi.2by2(strat2))

### Employment Status
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 1] <- 1
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 2] <- 1
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 3] <- 2
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 4] <- 2
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 5] <- 2
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 6] <- 2
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 7] <- 2
BRFSS2018_append$EmployCat[BRFSS2018_append$EMPLOY1 == 8] <- 2 

  # Education as confounder

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(X_RFPAP34, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(X_RFPAP34, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(X_MAM5022, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(X_MAM5022, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(FLUSHOT6, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(FLUSHOT6, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat4 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(CheckupCat, CareCatFac, eduCat)))
(epi.2by2(strat4))

(strat4 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(CheckupCat, CareCatFac, eduCat)))
(epi.2by2(strat4))

  # Income as confounder

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(X_RFPAP34, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(X_RFPAP34, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(X_MAM5022, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(X_MAM5022, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(FLUSHOT6, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(FLUSHOT6, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(CheckupCat, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(CheckupCat, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))


# Dose Response Hours per week
BRFSS2018_append$CRGVHRS1[BRFSS2018_append$CRGVHRS1 == 7] <- NA
BRFSS2018_append$CRGVHRS1[BRFSS2018_append$CRGVHRS1 == 9] <- NA

BRFSS2018_append <- BRFSS2018_append %>% mutate(
  hour1 = case_when(
    CareCatFac == "1" & CRGVHRS1 == "1" ~ 1,
    CareCatFac == "2" ~ 2),
  hour1 = factor(hour1, labels = c("HourCat1", "NonCaregiver")),
  hour2 = case_when(
    CareCatFac == "1" & CRGVHRS1 == "2" ~ 1,
    CareCatFac == "2" ~ 2),
  hour2 = factor(hour2, labels = c("HourCat2", "NonCaregiver")),
  hour3 = case_when(
    CareCatFac == "1" & CRGVHRS1 == "3" ~ 1,
    CareCatFac == "2" ~ 2),
  hour3 = factor(hour3, labels = c("HourCat3", "NonCaregiver")),
  hour4 = case_when(
    CareCatFac == "1" & CRGVHRS1 == "4" ~ 1,
    CareCatFac == "2" ~ 2),
  hour4 = factor(hour4, labels = c("HourCat4", "NonCaregiver"))
  )
  
# less than 8 hours
  # Pap
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour1)))
(epi.2by2(strat1))

  # Mamm
(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour1)))
(epi.2by2(strat1))

  # Flu
(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour1)))
(epi.2by2(strat1))

  # Checkup
(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour1)))
(epi.2by2(strat1))

# 9-19 hours
  # Pap
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, hour2)))
(epi.2by2(strat1))

  # Mamm
(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour2)))
(epi.2by2(strat1))

  # Flu
(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour2)))
(epi.2by2(strat1))

  # Checkup
(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour2)))
(epi.2by2(strat1))

# 20-39 hours
  # Pap
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, hour3)))
(epi.2by2(strat1))

  # Mamm
(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour3)))
(epi.2by2(strat1))

  # Flu
(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour3)))
(epi.2by2(strat1))
 
  # Checkup
(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour3)))
(epi.2by2(strat1))

# over 40
  # Pap
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, hour4)))
(epi.2by2(strat1))

  # Mamm
(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour4)))
(epi.2by2(strat1))

  # Flu 
(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour4)))
(epi.2by2(strat1))

  # Checkup
(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour4)))
(epi.2by2(strat1))

#### Confounders on strata-specific estimates




