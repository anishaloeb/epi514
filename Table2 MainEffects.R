BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 0] <- 2
BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 1] <- 1

BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 1] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 2] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 3] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 4] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 8] <- 2

(rrTab <- table(BRFSS2018_append$X_RFPAP34, BRFSS2018_append$CareCatFac)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$X_MAM5022, BRFSS2018_append$CareCatFac)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$FLUSHOT6, BRFSS2018_append$CareCatFac)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$CheckupCat, BRFSS2018_append$CareCatFac)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))


## Confounding vars
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

## Testing for confounding
  ## Education
(rrTab <- table(BRFSS2018_append$X_RFPAP34, BRFSS2018_append$CareCatFac, BRFSS2018_append$eduCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$X_MAM5022, BRFSS2018_append$CareCatFac, BRFSS2018_append$eduCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$FLUSHOT6, BRFSS2018_append$CareCatFac, BRFSS2018_append$eduCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$CheckupCat, BRFSS2018_append$CareCatFac, BRFSS2018_append$eduCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

  ## Income
  
(rrTab <- table(BRFSS2018_append$X_RFPAP34, BRFSS2018_append$CareCatFac, BRFSS2018_append$IncomeCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$X_MAM5022, BRFSS2018_append$CareCatFac, BRFSS2018_append$IncomeCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$FLUSHOT6, BRFSS2018_append$CareCatFac, BRFSS2018_append$IncomeCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$CheckupCat, BRFSS2018_append$CareCatFac, BRFSS2018_append$IncomeCat)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

  ## Age (10 year cats)
 
(rrTab <- table(BRFSS2018_append$X_RFPAP34, BRFSS2018_append$CareCatFac, BRFSS2018_append$ageCatNEW)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab[1:2, 1:2, 1:4]))

(rrTab <- table(BRFSS2018_append$X_MAM5022, BRFSS2018_append$CareCatFac, BRFSS2018_append$ageCatNEW)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab[1:2, 1:2, 3:5]))

(rrTab <- table(BRFSS2018_append$FLUSHOT6, BRFSS2018_append$CareCatFac, BRFSS2018_append$ageCatNEW)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$CheckupCat, BRFSS2018_append$CareCatFac, BRFSS2018_append$ageCatNEW)
                deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))


