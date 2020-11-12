######################################################
######################################################
## DATA DESCRIPTIVES
## Time (hours/day) for Child Care Tasks 
## SOEP 2019 - CoV
## SZ, 04.06.2020
######################################################
######################################################

rm(list=ls())
library(ggplot2)
library(questionr)
library(Hmisc)
library(plyr)

# ---------------------------------------------------------------------
# Load Data
# ---------------------------------------------------------------------
setwd("...") # add path
load("_workspace_dataPrepared_2018.RData") # load workspace stored from prepareData_2018.R

M <- md.pattern(DAT_panel, plot=FALSE)
round(M[nrow(M),]/nrow(DAT_panel),2) # max 2% missing values
table(complete.cases(DAT_panel)) # ~98% with complete data lines

# ---------------------------------------------------------------------
# Composition of sample concerning covariates
# ---------------------------------------------------------------------

# ALL
DAT_panel$sex <- ifelse(DAT_panel$sex %in% 1, 1, ifelse(DAT_panel$sex %in% 2, 0, NA)) # ref. fem
w <- wtd.table(DAT_panel$sex, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$OW, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
DAT_panel$mig <- ifelse(DAT_panel$migback %in% c(1,3), 0, ifelse(DAT_panel$migback %in% 2, 1, NA)) # ref. no mig
w <- wtd.table(DAT_panel$mig, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$migback, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
DAT_panel$ageKat <- ifelse(DAT_panel$ageYoungKid <3,0, ifelse(DAT_panel$ageYoungKid %in% 3:5, 1, ifelse(DAT_panel$ageYoungKid %in% 6:11, 2,3)))
w <- wtd.table(DAT_panel$ageKat, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
DAT_panel$numChHH_cat <- ifelse(DAT_panel$numKids %in% 1,0, ifelse(DAT_panel$numKids %in% 2, 1,2))
w <- wtd.table(DAT_panel$numChHH_cat, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$eduLevel, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$empl2018, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$empl2019, weights=DAT_panel$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

# FEM
DAT_fem <- DAT_panel[DAT_panel$sex==0,]
w <- wtd.table(DAT_fem$OW, weights=DAT_fem$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$mig, weights=DAT_fem$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$ageKat, weights=DAT_fem$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$numChHH_cat, weights=DAT_fem$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$eduLevel, weights=DAT_fem$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$empl2018, weights=DAT_fem$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$empl2019, weights=DAT_fem$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

# MALE
DAT_male <- DAT_panel[DAT_panel$sex==1,]
w <- wtd.table(DAT_male$OW, weights=DAT_male$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$mig, weights=DAT_male$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$ageKat, weights=DAT_male$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$numChHH_cat, weights=DAT_male$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$eduLevel, weights=DAT_male$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$empl2018, weights=DAT_male$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$empl2019, weights=DAT_male$phrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

# ---------------------------------------------------------------------
#  Properties of dep. variable
# ---------------------------------------------------------------------
DAT_panel_m <- DAT_panel[DAT_panel$sex %in% 1,]
DAT_panel_f <- DAT_panel[DAT_panel$sex %in% 2,]

# DV: 2018
DAT_panel_m <- DAT_panel[DAT_panel$sex %in% 1,]
table(DAT_panel_m$hoursCildCare_2018, exclude = NULL) # N=1 NA
weighted.mean(DAT_panel_m$hoursCildCare_2018, w=DAT_panel_m$phrf, na.rm = TRUE)
DAT_panel_f <- DAT_panel[DAT_panel$sex %in% 0,]
table(DAT_panel_f$hoursCildCare_2018, exclude = NULL) # N=1 NA
weighted.mean(DAT_panel_f$hoursCildCare_2018, w=DAT_panel_f$phrf, na.rm = TRUE)

# DV: 2019-2018
DAT_panel_m$diff <- DAT_panel_m$hoursCildCare_2019 - DAT_panel_m$hoursCildCare_2018
table(DAT_panel_m$diff, exclude = NULL) 
weighted.mean(DAT_panel_m$diff, w=DAT_panel_m$phrf, na.rm = TRUE)
DAT_panel_f$diff <- DAT_panel_f$hoursCildCare_2019 - DAT_panel_f$hoursCildCare_2018
table(DAT_panel_f$diff, exclude = NULL) 
weighted.mean(DAT_panel_f$diff, w=DAT_panel_f$phrf, na.rm = TRUE)

# Men Diff for distinct covariates / categories
DAT_panel_m$hours2018_cat <- ifelse(DAT_panel_m$hoursCildCare_2018 <=2,0, 
                                    ifelse(DAT_panel_m$hoursCildCare_2018>2 & DAT_panel_m$hoursCildCare_2018<=5,1,
                                           ifelse(DAT_panel_m$hoursCildCare_2018>5,2,NA)))  # N=1 NA
D <- DAT_panel_m[DAT_panel_m$hours2018_cat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2018_cat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2018_cat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 3,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageKat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageKat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageKat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)

# Men values in 2018 for distinct covariates / categories
D <- DAT_panel_m[DAT_panel_m$hours2018_cat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2018_cat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2018_cat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl2018 == 3,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageKat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageKat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageKat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)

# Fem Diff for distinct covariates / categories
DAT_panel_f$hours2018_cat <- ifelse(DAT_panel_f$hoursCildCare_2018 <=2,0, 
                                    ifelse(DAT_panel_f$hoursCildCare_2018>2 & DAT_panel_f$hoursCildCare_2018<=5,1,
                                           ifelse(DAT_panel_f$hoursCildCare_2018>5,2,NA)))  # N=1 NA
D <- DAT_panel_f[DAT_panel_f$hours2018_cat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2018_cat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2018_cat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 3,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageKat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageKat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageKat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf, na.rm = TRUE)

# Women values in 2018 for distinct covariates / categories
D <- DAT_panel_f[DAT_panel_f$hours2018_cat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2018_cat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2018_cat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl2018 == 3,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageKat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageKat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageKat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2018, w=D$phrf, na.rm = TRUE)

# Plot distribution of care hours for men and women
Dm <- rbind(cbind(DAT_panel_m$hoursCildCare_2018, DAT_panel_m$phrf, 2018), 
            cbind(DAT_panel_m$hoursCildCare_2019, DAT_panel_m$phrf, 2019))
Df <- rbind(cbind(DAT_panel_f$hoursCildCare_2018, DAT_panel_f$phrf, 2018), 
            cbind(DAT_panel_f$hoursCildCare_2019, DAT_panel_f$phrf, 2019))
Dmm <- cbind(Dm, rep(c(3, 4), each=nrow(Dm)/2))
Dff <- cbind(Df, rep(c(1, 2), each=nrow(Df)/2))
D <- rbind(Dmm, Dff)
colnames(D) <- c("CareHours", "Weight", "Year", "Gender")
D <- as.data.frame(D)
D$sex <- ifelse(D$Gender %in% c(3,4), "Men", "Women")
D.wm <- ddply(D, .(Gender), summarize, 
             wmean=round(wtd.mean(CareHours, Weight, na.rm=TRUE), 2))
D.wm$sex <- as.factor(c("Men","Men", "Women", "Women"))

D$Gender <- as.factor(D$Gender)
D$sex <- as.factor(D$sex)
D$Year <- as.factor(D$Year)
ggplot(D, aes(x = Gender, y = CareHours, weight = Weight)) + 
  geom_boxplot(width=0.6, aes(fill=sex), colour="grey40") + 
  scale_fill_manual(values=c("dodgerblue", "indianred")) +
  geom_point(data=D.wm,aes(x=c(1:4),y=wmean),shape = 21, 
             size = 3, fill = c("red", "red", "blue", "blue"),inherit.aes=FALSE) +
  xlab("") +  ylab("child care (in hours per day)") +
 labs(fill = "Gender") +
 scale_x_discrete(labels=c("2018", "2019", "2018", "2019")) 
