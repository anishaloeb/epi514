BRFSS2018_append <- read.csv("Desktop/BRFSS2018_append_CareCat&Missing.csv #missing and CareCat already there

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

BRFSS2018_append <- BRFSS2018_append %>% mutate(
  age1 = case_when(
    ageCat == "30-34" ~ 1,
    ageCat == "25-29" ~ 2),
  age1 = factor(age1, labels = c("30-34", "25-29")),
  age2 = case_when(
    ageCat == "35-39" ~ 1,
    ageCat == "25-29" ~ 2),
  age2 = factor(age2, labels = c("35-39", "25-29")),
  age3 = case_when(
    ageCat == "40-44" ~ 1,
    ageCat == "25-29" ~ 2),
  age3 = factor(age3, labels = c("40-44", "25-29")),
  age4 = case_when(
    ageCat == "45-49" ~ 1,
    ageCat == "25-29" ~ 2),
  age4 = factor(age4, labels = c("45-49", "25-29")),
  age5 = case_when(
    ageCat == "50-54" ~ 1,
    ageCat == "25-29" ~ 2),
  age5 = factor(age5, labels = c("50-54", "25-29")),
  age6 = case_when(
    ageCat == "55-59" ~ 1,
    ageCat == "25-29" ~ 2),
  age6 = factor(age6, labels = c("55-59", "25-29")),
  age7 = case_when(
    ageCat == "60-64" ~ 1,
    ageCat == "25-29" ~ 2),
  age7 = factor(age7, labels = c("60-64", "25-29")),
  age8 = case_when(
    ageCat == "65-69" ~ 1,
    ageCat == "25-29" ~ 2),
  age8 = factor(age8, labels = c("65-69", "25-29")),
  age9 = case_when(
    ageCat == "70-74" ~ 1,
    ageCat == "25-29" ~ 2),
  age9 = factor(age9, labels = c("70-74", "25-29"))
)


(strat1 <- with(BRFSS2018_append, 
               table(X_RFPAP34, CareCatFac, age1)))
(epi.2by2(strat1))

(strat2 <- with(BRFSS2018_append, 
                table(X_RFPAP34, CareCatFac, age2)))
(epi.2by2(strat2))

(strat3 <- with(BRFSS2018_append, 
                table(X_RFPAP34, CareCatFac, age3)))
(epi.2by2(strat3))

(strat4 <- with(BRFSS2018_append, 
                table(X_RFPAP34, CareCatFac, age4)))
(epi.2by2(strat4))

(strat5 <- with(BRFSS2018_append, 
                table(X_RFPAP34, CareCatFac, age5)))
(epi.2by2(strat5))

(strat6 <- with(BRFSS2018_append, 
                table(X_RFPAP34, CareCatFac, age6)))
(epi.2by2(strat6))

(strat7 <- with(BRFSS2018_append, 
                table(X_RFPAP34, CareCatFac, age7)))
(epi.2by2(strat7))

(strat8 <- with(BRFSS2018_append, 
                table(X_RFPAP34, CareCatFac, age8)))
(epi.2by2(strat8))

