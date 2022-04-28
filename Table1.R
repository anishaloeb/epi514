library("tidyverse")
library("haven")
library("boot")

# Read in data
BRFSS18v3 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS18v3.csv")
BRFSS18v1 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS18v1.csv")
BRFSS18v2 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS18v2.csv")
BRFSS2018 <- read.csv("~/Desktop/UW MPH/Year 1/Spring 2022/EPI 514/BRFSS2018.csv")

# Append data
BRFSS2018_append <- bind_rows(BRFSS18v1, BRFSS18v2)
BRFSS2018_append  <- bind_rows(BRFSS2018_append, BRFSS2018)
BRFSS2018_append  <- bind_rows(BRFSS2018_append , BRFSS18v3)

# Sep by Exp Status
# Remove Missing
BRFSS2018_append$CAREGIV1[BRFSS2018_append$CAREGIV1 == 9] <- NA

BRFSS2018_append$X_AGEG5YR <- factor(BRFSS2018_append$X_AGEG5YR, 
                                     levels = 1:14,
                                     labels = c("18-24", "25-29", "30-34", "35-39",
                                                "40-44", "45-49", "50-54", "55-59",
                                                "60-64", "65-69", "70-74", "75-79",
                                                "80+", "idk, ref, missing"))

Caregive18 <- BRFSS2018_append %>% filter(BRFSS2018_append$CAREGIV1 == 1) # are you caregiver
Caregive18 <- Caregive18 %>% filter(Caregive18$CRGVLNG1 > 1) # over 30 days

NonCaregive <- BRFSS2018_append %>% filter(BRFSS2018_append$CAREGIV1 > 1)
CaregiveU30 <- BRFSS2018_append %>% filter(BRFSS2018_append$CRGVLNG1 == 1)
NonCaregive18 <- bind_rows(NonCaregive,CaregiveU30)

# Age
# Pap: 18-64
table(Caregive18$X_AGEG5YR >= 1 & Caregive18$X_AGEG5YR <= 9)
table(NonCaregive18$X_AGEG5YR >= 1 & NonCaregive18$X_AGEG5YR <= 9)
# Mammogram: 50-74
table(Caregive18$X_AGEG5YR >= 7 & Caregive18$X_AGEG5YR <= 11)
table(NonCaregive18$X_AGEG5YR >= 7 & NonCaregive18$X_AGEG5YR <= 11)

# Education
table(Caregive18$EMPLOY1)

# Employment

#library(usethis)
#usethis::use_git_config(user.name = "anishaloeb", user.email = "aloeb@uw.edu")
#usethis::create_github_token() 
#credentials::set_github_pat("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")
