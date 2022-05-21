## Insurance

(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, X_RFPAP34)))

(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, X_RFPAP34)))
(epi.2by2(strat2))

(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, X_MAM5022)))

(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, X_MAM5022)))
(epi.2by2(strat2))

(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, CheckupCat)))

(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, CheckupCat)))
(epi.2by2(strat2))

(strat1 <- with(subset(BRFSS2018_append, HLTHPLN1 == 1),
                table(CareCatFac, FLUSHOT6)))

(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append, HLTHPLN1 == 2),
                table(CareCatFac, FLUSHOT6)))
(epi.2by2(strat2))


  ## Testing confounding

## Age (new cats)
  ## Mammogram Age - excluded 65+
BRFSS2018_append$MamAge[BRFSS2018_append$X_AGEG5YR == 7] <- 1
BRFSS2018_append$MamAge[BRFSS2018_append$X_AGEG5YR == 8] <- 2
BRFSS2018_append$MamAge[BRFSS2018_append$X_AGEG5YR == 9] <- 3

  ## Pap Age - using this flu shot and checkup as well
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 2] <- 1
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 3] <- 1
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 4] <- 1
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 5] <- 1
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 6] <- 1
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 7] <- 2
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 8] <- 2
BRFSS2018_append$PapAge[BRFSS2018_append$X_AGEG5YR == 9] <- 2

BRFSS2018_append$IncomeCat <- factor(BRFSS2018_append$X_INCOMG,
                                     levels = 1:5,
                                     labels = c("less15",
                                                "15toless24",
                                                "24toless35",
                                                "35toless50",
                                                "50+"))
                                                
BRFSS2018_append$eduCat <- factor(BRFSS2018_append$X_EDUCAG,
                                  labels = c("Did not graduate High School",
                                             "Graduated High School",
                                             "Attended College or Technical School",
                                             "Graduated from College or Technical School"))
 
  ## Education

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_RFPAP34, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_RFPAP34, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_MAM5022, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_MAM5022, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(FLUSHOT6, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(FLUSHOT6, CareCatFac, eduCat)))
(epi.2by2(strat1))

(strat4 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(CheckupCat, CareCatFac, eduCat)))
(epi.2by2(strat4))

(strat4 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(CheckupCat, CareCatFac, eduCat)))
(epi.2by2(strat4))

  ## Income
(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_RFPAP34, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_RFPAP34, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_MAM5022, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_MAM5022, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(FLUSHOT6, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(FLUSHOT6, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(CheckupCat, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(CheckupCat, CareCatFac, X_INCOMG)))
(epi.2by2(strat1))

  ## Age 
(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_RFPAP34, CareCatFac, PapAge)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_RFPAP34, CareCatFac, PapAge)))
(epi.2by2(strat1))

(strat2 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_MAM5022, CareCatFac, MamAge)))
(epi.2by2(strat2))

(strat2 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_MAM5022, CareCatFac, MamAge)))
(epi.2by2(strat2))

(strat3 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(FLUSHOT6, CareCatFac, PapAge)))
(epi.2by2(strat3))

(strat3 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(FLUSHOT6, CareCatFac, PapAge)))
(epi.2by2(strat3))

(strat4 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(CheckupCat, CareCatFac, PapAge)))
(epi.2by2(strat4))

(strat4 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(CheckupCat, CareCatFac, PapAge)))
(epi.2by2(strat4))

    ## Combined Variables
BRFSS2018_append$MamAgeEdu <- case_when(!is.na(BRFSS2018_append$eduCat) &
                                          !is.na(BRFSS2018_append$MamAge) ~
                                          paste0(BRFSS2018_append$eduCat, "_", BRFSS2018_append$MamAge))

(strat2 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_MAM5022, CareCatFac, MamAgeEdu)))
(epi.2by2(strat2))

(strat2 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_MAM5022, CareCatFac, MamAgeEdu)))
(epi.2by2(strat2))

BRFSS2018_append$PapAgeIncome <- case_when(!is.na(BRFSS2018_append$PapAge) &
                                             !is.na(BRFSS2018_append$IncomeCat) ~
                                             paste0(BRFSS2018_append$PapAge, "_", BRFSS2018_append$IncomeCat))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(X_RFPAP34, CareCatFac, PapAgeIncome)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(X_RFPAP34, CareCatFac, PapAgeIncome)))
(epi.2by2(strat1))


BRFSS2018_append$PapAgeEdu <- case_when(!is.na(BRFSS2018_append$eduCat) &
                                          !is.na(BRFSS2018_append$PapAge) ~
                                          paste0(BRFSS2018_append$eduCat, "_", BRFSS2018_append$PapAge))


(strat4 <- with(subset(BRFSS2018_append,HLTHPLN1 == 1),
                table(CheckupCat, CareCatFac, PapAgeEdu)))
(epi.2by2(strat4))

(strat4 <- with(subset(BRFSS2018_append,HLTHPLN1 == 2),
                table(CheckupCat, CareCatFac, PapAgeEdu)))
(epi.2by2(strat4))



### Employment
    # Pap
(strat1 <- with(subset(BRFSS2018_append, EmployCat == 1),
                table(CareCatFac, X_RFPAP34)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append, EmployCat == 2),
                table(CareCatFac, X_RFPAP34)))
(epi.2by2(strat1))

    # Mamm
(strat1 <- with(subset(BRFSS2018_append, EmployCat == 1),
                table(CareCatFac, X_MAM5022)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append, EmployCat == 2),
                table(CareCatFac, X_MAM5022)))
(epi.2by2(strat1))

    # Checkup
(strat1 <- with(subset(BRFSS2018_append, EmployCat == 1),
                table(CareCatFac, CheckupCat)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append, EmployCat == 2),
                table(CareCatFac, CheckupCat)))
(epi.2by2(strat1))

    # Flushot
(strat1 <- with(subset(BRFSS2018_append, EmployCat == 1),
                table(CareCatFac, FLUSHOT6)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append, EmployCat == 2),
                table(CareCatFac, FLUSHOT6)))
(epi.2by2(strat1))


  ## Confounding

  # Education
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

  # Income
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

  # Age

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(X_RFPAP34, CareCatFac, PapAge)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(X_RFPAP34, CareCatFac, PapAge)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(X_MAM5022, CareCatFac, MamAge)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(X_MAM5022, CareCatFac, MamAge)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(FLUSHOT6, CareCatFac, PapAge)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(FLUSHOT6, CareCatFac, PapAge)))
(epi.2by2(strat1))

(strat4 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(CheckupCat, CareCatFac, PapAge)))
(epi.2by2(strat4))

(strat4 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(CheckupCat, CareCatFac, PapAge)))
(epi.2by2(strat4))

  ## Combined vars

BRFSS2018_append$PapAgeEdu <- case_when(!is.na(BRFSS2018_append$eduCat) &
                                          !is.na(BRFSS2018_append$PapAge) ~
                                          paste0(BRFSS2018_append$eduCat, "_", BRFSS2018_append$PapAge))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 1),
                table(X_RFPAP34, CareCatFac, PapAgeEdu)))
(epi.2by2(strat1))

(strat1 <- with(subset(BRFSS2018_append,EmployCat == 2),
                table(X_RFPAP34, CareCatFac, PapAgeEdu)))
(epi.2by2(strat1))

