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

# Education (did not graduate high school, graduated high school, attended college or technical school, graduated from college or technical school)
table(Caregive18$EMPLOY1)

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
