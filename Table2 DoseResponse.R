BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 0] <- 2
BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 1] <- 1

BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 1] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 2] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 3] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 4] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 8] <- 2

BRFSS2018_append$CRGVHRS1[BRFSS2018_append$CRGVHRS1 == 7] <- NA
BRFSS2018_append$CRGVHRS1[BRFSS2018_append$CRGVHRS1 == 9] <- NA

## Create hour stratum
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
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour1)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour1)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour1)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour1)))
(epi.2by2(strat1))

  # 9-19 hours
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, hour2)))
(epi.2by2(strat1))


(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour2)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour2)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour2)))
(epi.2by2(strat1))

  # 20-39 hours
  
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, hour3)))
(epi.2by2(strat1))


(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour3)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour3)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour3)))
(epi.2by2(strat1))

  # over 40
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, hour4)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour4)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour4)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour4)))
(epi.2by2(strat1))


## testing for confounding
BRFSS2018_append$eduCat <- factor(BRFSS2018_append$X_EDUCAG,
                                  labels = c("Did not graduate High School",
                                             "Graduated High School",
                                             "Attended College or Technical School",
                                             "Graduated from College or Technical School"))

BRFSS2018_append$IncomeCat <- factor(BRFSS2018_append$X_INCOMG,
                                     levels = 1:5,
                                     labels = c("less15",
                                                "15toless24",
                                                "24toless35",
                                                "35toless50",
                                                "50+"))
                                                
BRFSS2018_append$ageCatNEW <- factor(BRFSS2018_append$ageCatNEW,
                                     levels = 1:5,
                                     labels = c("25-34",
                                                "35-44",
                                                "45-54",
                                                "55-64",
                                                "65-74"))
                                                
 
  ## less than 8 hours - Education
 
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour1, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour1, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour1, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour1, eduCat)))
(epi.2by2(strat1))

  ## 9-19 education

(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour2, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour2, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour2, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour2, eduCat)))
(epi.2by2(strat1))

    ## 20-39 education
    
 (strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour3, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour3, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour3, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour3, eduCat)))
(epi.2by2(strat1))

    ## 40+ education
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour4, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour4, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour4, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour4, eduCat)))
(epi.2by2(strat1))


 ### Income
 ## less than 8 hours - Income
 
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour1, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour1, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour1, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour1, IncomeCat)))
(epi.2by2(strat1))

  ## 9-19 Income

(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour2, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour2, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour2, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour2, IncomeCat)))
(epi.2by2(strat1))

    ## 20-39 Income
    
 (strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour3, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour3, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour3, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour3, IncomeCat)))
(epi.2by2(strat1))

    ## 40+ Income
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour4, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour4, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour4, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour4, IncomeCat)))
(epi.2by2(strat1))


### Age (10 year)
 ## less than 8 hours - Age
 
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour1, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour1, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour1, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour1, ageCatNEW)))
(epi.2by2(strat1))

  ## 9-19 Age

(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour2, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour2, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour2, IncomeCat)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour2, ageCatNEW)))
(epi.2by2(strat1))

    ## 20-39 Age
    
 (strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour3, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour3, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour3, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour3, ageCatNEW)))
(epi.2by2(strat1))

    ## 40+ Age
(strat1 <- with(BRFSS2018_append,
                       table(X_RFPAP34, hour4, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(X_MAM5022, hour4, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(FLUSHOT6, hour4, ageCatNEW)))
(epi.2by2(strat1))

(strat1 <- with(BRFSS2018_append,
                table(CheckupCat, hour4, ageCatNEW)))
(epi.2by2(strat1))

