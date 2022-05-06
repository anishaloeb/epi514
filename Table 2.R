BRFSS2018_append <- read.csv("Desktop/BRFSS2018_append_CareCat&Missing.csv #missing and CareCat already there

# set survey weights

options(survey.lonely.psu = "adjust")

bd <- svydesign(data = BRFSS2018_append, id = ~X_PSU, strata = ~X_STSTR,
                        weight = ~X_LLCPWT, nest = TRUE)
                        

# Prevalence between Caregivers and Non Caregivers

svytable(~X_RFPAP34 + CareCat, bd) %>% prop.table(margin=2)

svytable(~X_MAM5022 + CareCat, bd) %>% prop.table(margin=2)

# Unadjusted Associations

(rrTab <- table(BRFSS2018_append$X_RFPAP34, BRFSS2018_append$CareCat, deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

(rrTab <- table(BRFSS2018_append$X_MAM5022, BRFSS2018_append$CareCat, deparse.level = 2))
(rrStrat <- epi.2by2(dat=rrTab))

