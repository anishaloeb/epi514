BRFSS2018_append <- read.csv("Desktop/BRFSS2018_append_CareCat&Missing.csv") #missing and CareCat already there

BRFSS2018_append$FLUSHOT6[BRFSS2018_append$FLUSHOT6 == 7] <- NA
BRFSS2018_append$FLUSHOT6[BRFSS2018_append$FLUSHOT6 == 9] <- NA

BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 1] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 2] <- 1
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 3] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 4] <- 2
BRFSS2018_append$CheckupCat[BRFSS2018_append$CHECKUP1 == 8] <- 2


# set survey weights

options(survey.lonely.psu = "adjust")

bd <- svydesign(data = BRFSS2018_append, id = ~X_PSU, strata = ~X_STSTR,
                        weight = ~X_LLCPWT, nest = TRUE)
                        

# Prevalence between Caregivers and Non Caregivers

svytable(~X_RFPAP34 + CareCat, bd) %>% prop.table(margin=2)

svytable(~X_MAM5022 + CareCat, bd) %>% prop.table(margin=2)

# Unadjusted Associations

BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 0] <- 2
BRFSS2018_append$CareCatFac[BRFSS2018_append$CareCat == 1] <- 1

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

# Adjusted Pap 
(strat1 <- with(BRFSS2018_append,
                table(X_RFPAP34, CareCatFac, ageCat)))

(epi.2by2(strat1[1:2, 1:2, 1:9])) # excluding 70-74

# Adjusted Mammogram
(strat2 <- with(BRFSS2018_append,
                table(X_MAM5022, CareCatFac, ageCat)))

(epi.2by2(strat2[1:2, 1:2, 6:10])) # excluding age cats < 50
