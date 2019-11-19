#install the library for import xpt files
library(haven)

BRFSS_a <- read_xpt("/Users/mindcontrol/Documents/R BRFSS Proj/LLCP2014.XPT ")
colnames(BRFSS_a)

#define object list of variables to be kept
BRFSSVarList <- c("VETERAN3", 
			"ALCDAY5",
			"SLEPTIM1",
			"ASTHMA3",
			"_AGE_G",
			"SMOKE100",
			"SMOKDAY2",
			"SEX",
			"_HISPANC",
			"_MRACE1",
			"MARITAL",
			"GENHLTH",
			"HLTHPLN1",
			"EDUCA",
			"INCOME2",
			"_BMI5CAT",
			"EXERANY2")

BRFSS_b <- BRFSS_a[BRFSSVarList]
nrow(BRFSS_b)

#subset the dataset for only veterans
BRFSS_c <- subset(BRFSS_b, VETERAN3==1)
BRFSS_c$VETERAN3
nrow(BRFSS_c)

#only keep the rows with valid alcohol/exposure variable
BRFSS_d <- subset(BRFSS_c, ALCDAY5 < 777 | ALCDAY5 ==888)
BRFSS_d$ALCDAY5
nrow(BRFSS_d)

#only keep rows with valid sleep data
BRFSS_e <- subset(BRFSS_d, SLEPTIM1 < 77)
nrow(BRFSS_e)

#only keep rows with valid asthma data
BRFSS_f <- subset(BRFSS_e, ASTHMA3 <7)
nrow(BRFSS_f)

#adding indicator variables for veterans
#make a copy of the dataset
BRFSS_g <- BRFSS_f

#adding the categorical varialbe set to 9 to the dataset
BRFSS_g$ALCGRP <- 9

#update according to tthe data dictionary
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 <200 ] <- 3
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 >=200 & BRFSS_g$ALCDAY5 <777] <- 2
BRFSS_g$ALCGRP[BRFSS_g$ALCDAY5 == 888] <- 1


#Check variable
table(BRFSS_g$ALCGRP, BRFSS_g$ALCDAY5)

#Add flags
BRFSS_g$DRKMONTHLY <- 0
BRFSS_g$DRKMONTHLY[BRFSS_g$ALCGRP == 2] <- 1
table(BRFSS_g$ALCGRP, BRFSS_g$DRKMONTHLY)

BRFSS_g$DRKWEEKLY <- 0
BRFSS_g$DRKWEEKLY [BRFSS_g$ALCGRP == 1] <- 1
table(BRFSS_g$ALCGRP, BRFSS_g$DRKWEEKLY)

#Make a copy of the dataset
BRFSS_h <- BRFSS_g

#Make and test sleep variable
BRFSS_h$SLEPTIM2 <- NA
BRFSS_h$SLEPTIM2[!is.na(BRFSS_h$SLEPTIM1) & BRFSS_h$SLEPTIM1 !=77 & BRFSS_h$SLEPTIM1 !=99] <- BRFSS_h$SLEPTIM1

table(BRFSS_h$SLEPTIM1, BRFSS_h$SLEPTIM2)

#Make and test asthma variable
BRFSS_h$ASTHMA4 <- 9
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 1] <- 1
BRFSS_h$ASTHMA4[BRFSS_h$ASTHMA3 == 2] <- 0

table(BRFSS_h$ASTHMA3, BRFSS_h$ASTHMA4)

#Make a copy of the dataset
BRFSS_i <- BRFSS_h

#Add and check age variables
BRFSS_i$AGE2 <- 0
BRFSS_i$AGE3 <- 0
BRFSS_i$AGE4 <- 0
BRFSS_i$AGE5 <- 0
BRFSS_i$AGE6 <- 0

BRFSS_i$AGE2[BRFSS_i$"_AGE_G" == 2] <- 1
table(BRFSS_i$"_AGE_G", BRFSS_i$AGE2)

BRFSS_i$AGE3[BRFSS_i$"_AGE_G" == 3] <- 1
table(BRFSS_i$"_AGE_G", BRFSS_i$AGE3)

BRFSS_i$AGE4[BRFSS_i$"_AGE_G" == 4] <- 1
table(BRFSS_i$"_AGE_G", BRFSS_i$AGE4)

BRFSS_i$AGE5[BRFSS_i$"_AGE_G" == 5] <- 1
table(BRFSS_i$"_AGE_G", BRFSS_i$AGE5)

BRFSS_i$AGE6[BRFSS_i$"_AGE_G" == 6] <- 1
table(BRFSS_i$"_AGE_G", BRFSS_i$AGE6)

#make smoking variables

BRFSS_i$NEVERSMK <- 0
BRFSS_i$NEVERSMK [BRFSS_i$SMOKE100 == 2] <- 1
table(BRFSS_i$SMOKE100, BRFSS_i$NEVERSMK)

BRFSS_i$SMOKGRP <- 9
BRFSS_i$SMOKGRP[BRFSS_i$SMOKDAY2 == 1 | BRFSS_i$SMOKDAY2 == 2] <- 1
BRFSS_i$SMOKGRP[BRFSS_i$SMOKDAY2 == 3 | BRFSS_i$NEVERSMK == 1] <- 2

table(BRFSS_i$SMOKGRP, BRFSS_i$SMOKDAY2)
table(BRFSS_i$SMOKGRP, BRFSS_i$SMOKE100)

BRFSS_i$SMOKER <- 0
BRFSS_i$SMOKER[BRFSS_i$SMOKGRP == 1] <- 1

table(BRFSS_i$SMOKGRP, BRFSS_i$SMOKER)

#make sex variable

BRFSS_i$MALE <- 0
BRFSS_i$MALE[BRFSS_i$SEX == 1] <- 1

table(BRFSS_i$MALE, BRFSS_i$SEX)

#make Hispanic variable

BRFSS_i$HISPANIC <- 0
BRFSS_i$HISPANIC[BRFSS_i$"_HISPANC" == 1] <- 1

table(BRFSS_i$HISPANIC, BRFSS_i$"_HISPANC")

#make race variables

BRFSS_i$RACEGRP <- 9
BRFSS_i$RACEGRP[BRFSS_i$"_MRACE1" == 1] <- 1
BRFSS_i$RACEGRP[BRFSS_i$"_MRACE1" == 2] <- 2
BRFSS_i$RACEGRP[BRFSS_i$"_MRACE1" == 3] <- 3
BRFSS_i$RACEGRP[BRFSS_i$"_MRACE1" == 4] <- 4
BRFSS_i$RACEGRP[BRFSS_i$"_MRACE1" == 5] <- 5
BRFSS_i$RACEGRP[BRFSS_i$"_MRACE1" == 6 | BRFSS_i$"_MRACE1" == 7] <- 6

table(BRFSS_i$RACEGRP , BRFSS_i$"_MRACE1")

BRFSS_i$BLACK <- 0
BRFSS_i$ASIAN <- 0
BRFSS_i$OTHRACE <- 0

BRFSS_i$BLACK[BRFSS_i$RACEGRP == 2] <- 1
table(BRFSS_i$RACEGRP, BRFSS_i$BLACK)

BRFSS_i$ASIAN[BRFSS_i$RACEGRP == 4] <- 1
table(BRFSS_i$RACEGRP, BRFSS_i$ASIAN)

BRFSS_i$OTHRACE[BRFSS_i$RACEGRP == 3 | BRFSS_i$RACEGRP == 5 | BRFSS_i$RACEGRP == 6 | BRFSS_i$RACEGRP == 7] <- 1
table(BRFSS_i$RACEGRP, BRFSS_i$OTHRACE)

#make marital variables

BRFSS_i$MARGRP <- 9
BRFSS_i$MARGRP[BRFSS_i$MARITAL == 1 | BRFSS_i$MARITAL == 5] <- 1
BRFSS_i$MARGRP[BRFSS_i$MARITAL == 2 | BRFSS_i$MARITAL == 3 ] <- 2
BRFSS_i$MARGRP[BRFSS_i$MARITAL == 4] <- 3

table(BRFSS_i$MARGRP, BRFSS_i$MARITAL)

BRFSS_i$NEVERMAR <- 0
BRFSS_i$FORMERMAR <- 0

BRFSS_i$NEVERMAR[BRFSS_i$MARGRP == 3] <- 1
table(BRFSS_i$MARGRP, BRFSS_i$NEVERMAR)

BRFSS_i$FORMERMAR[BRFSS_i$MARGRP == 2] <- 1
table(BRFSS_i$MARGRP, BRFSS_i$FORMERMAR)

#Make Genhealth variables

BRFSS_i$GENHLTH2 <- 9
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 1] <- 1
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 2] <- 2
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 3] <- 3
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 4] <- 4
BRFSS_i$GENHLTH2[BRFSS_i$GENHLTH == 5] <- 5

table(BRFSS_i$GENHLTH2, BRFSS_i$GENHLTH)

BRFSS_i$FAIRHLTH <- 0
BRFSS_i$POORHLTH <- 0

BRFSS_i$FAIRHLTH [BRFSS_i$GENHLTH2 == 4] <- 1
table(BRFSS_i$FAIRHLTH, BRFSS_i$GENHLTH2)

BRFSS_i$POORHLTH [BRFSS_i$GENHLTH2 == 5] <- 1
table(BRFSS_i$POORHLTH, BRFSS_i$GENHLTH2)

#Make health plan variables

BRFSS_i$HLTHPLN2 <- 9
BRFSS_i$HLTHPLN2[BRFSS_i$HLTHPLN1 == 1] <- 1
BRFSS_i$HLTHPLN2[BRFSS_i$HLTHPLN1 == 2] <- 2

table(BRFSS_i$HLTHPLN1, BRFSS_i$HLTHPLN2)

BRFSS_i$NOPLAN <- 0
BRFSS_i$NOPLAN [BRFSS_i$HLTHPLN2== 2] <- 1
table(BRFSS_i$NOPLAN, BRFSS_i$HLTHPLN2)

#Make education variables

BRFSS_i$EDGROUP <- 9
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 1 | BRFSS_i$EDUCA == 2 | BRFSS_i$EDUCA == 3] <- 1
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 4] <- 2
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 5] <- 3
BRFSS_i$EDGROUP[BRFSS_i$EDUCA == 6] <- 4

table(BRFSS_i$EDGROUP, BRFSS_i$EDUCA)

BRFSS_i$LOWED <- 0
BRFSS_i$SOMECOLL <- 0

BRFSS_i$LOWED[BRFSS_i$EDGROUP == 1 | BRFSS_i$EDGROUP == 2 ] <- 1
table(BRFSS_i$LOWED, BRFSS_i$EDGROUP)

BRFSS_i$SOMECOLL [BRFSS_i$EDGROUP == 3] <- 1
table(BRFSS_i$SOMECOLL, BRFSS_i$EDGROUP)

#Make income variables

BRFSS_i$INCOME3 <- BRFSS_i$INCOME2
BRFSS_i$INCOME3[BRFSS_i$INCOME2 >=77] <- 9

table(BRFSS_i$INCOME2, BRFSS_i$INCOME3)

BRFSS_i$INC1 <- 0
BRFSS_i$INC2 <- 0
BRFSS_i$INC3 <- 0
BRFSS_i$INC4 <- 0
BRFSS_i$INC5 <- 0
BRFSS_i$INC6 <- 0
BRFSS_i$INC7 <- 0

BRFSS_i$INC1[BRFSS_i$INCOME3 == 1] <- 1
table(BRFSS_i$INC1, BRFSS_i$INCOME3)

BRFSS_i$INC2[BRFSS_i$INCOME3 == 2] <- 1
table(BRFSS_i$INC2, BRFSS_i$INCOME3)

BRFSS_i$INC3[BRFSS_i$INCOME3 == 3] <- 1
table(BRFSS_i$INC3, BRFSS_i$INCOME3)

BRFSS_i$INC4[BRFSS_i$INCOME3 == 4] <- 1
table(BRFSS_i$INC4, BRFSS_i$INCOME3)

BRFSS_i$INC5[BRFSS_i$INCOME3 == 5] <- 1
table(BRFSS_i$INC5, BRFSS_i$INCOME3)

BRFSS_i$INC6[BRFSS_i$INCOME3 == 6] <- 1
table(BRFSS_i$INC6, BRFSS_i$INCOME3)

BRFSS_i$INC7[BRFSS_i$INCOME3 == 7] <- 1
table(BRFSS_i$INC7, BRFSS_i$INCOME3)

#Make BMI variables

BRFSS_i$BMICAT<- 9
BRFSS_i$BMICAT[BRFSS_i$"_BMI5CAT" ==1] <- 1
BRFSS_i$BMICAT[BRFSS_i$"_BMI5CAT" ==2] <- 2
BRFSS_i$BMICAT[BRFSS_i$"_BMI5CAT" ==3] <- 3
BRFSS_i$BMICAT[BRFSS_i$"_BMI5CAT" ==4] <- 4

table(BRFSS_i$BMICAT, BRFSS_i$"_BMI5CAT")

BRFSS_i$UNDWT <- 0
BRFSS_i$OVWT <- 0
BRFSS_i$OBESE <- 0

BRFSS_i$UNDWT[BRFSS_i$BMICAT== 1] <- 1
table(BRFSS_i$UNDWT, BRFSS_i$BMICAT)

BRFSS_i$OVWT[BRFSS_i$BMICAT== 3] <- 1
table(BRFSS_i$OVWT, BRFSS_i$BMICAT)

BRFSS_i$OBESE[BRFSS_i$BMICAT== 4] <- 1
table(BRFSS_i$OBESE, BRFSS_i$BMICAT)

#make exercise variables

BRFSS_i$EXERANY3<- 9
BRFSS_i$EXERANY3[BRFSS_i$EXERANY2 ==1] <- 1
BRFSS_i$EXERANY3[BRFSS_i$EXERANY2 ==2] <- 2

table(BRFSS_i$EXERANY3, BRFSS_i$EXERANY2)

BRFSS_i$NOEXER <- 0
BRFSS_i$NOEXER[BRFSS_i$EXERANY3 ==2] <- 1
table(BRFSS_i$NOEXER, BRFSS_i$EXERANY3)

nrow(BRFSS_i)

#writte out analytical dataset
write.csv(BRFSS_i, file = "analytic.csv")















