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
load("_workspace_dataPrepared.RData") # load workspace stored from prepareData.R

# ---------------------------------------------------------------------
# Select only complete cases and persons in partnership
# ---------------------------------------------------------------------
DAT_panel <- DAT_panel[DAT_panel$la %in% c(0,1),] # N=1051 without single parents: N=965
table(complete.cases(DAT_panel)) # N=39 overall -> <5% miss max.
DAT_panel <- DAT_panel[complete.cases(DAT_panel),] # N=925 

# ---------------------------------------------------------------------
# Composition of sample concerning covariates
# ---------------------------------------------------------------------

# ALL
DAT_panel$sex <- ifelse(DAT_panel$sex %in% 1, 1, ifelse(DAT_panel$sex %in% 2, 0, NA)) # ref. fem
w <- wtd.table(DAT_panel$sex, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$OW, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
DAT_panel$mig <- ifelse(DAT_panel$migback %in% c(1,3), 0, ifelse(DAT_panel$migback %in% 2, 1, NA)) # ref. no mig
w <- wtd.table(DAT_panel$mig, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$migback, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$ageKat, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
DAT_panel$ageYoungChild_cat <- ifelse(DAT_panel$ageYoungChild %in% 0:2, 0, 
                                      ifelse(DAT_panel$ageYoungChild %in% 3:5, 1, ifelse(DAT_panel$ageYoungChild %in% 6:11,2, NA)))
w <- wtd.table(DAT_panel$ageYoungChild_cat, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
DAT_panel$numChHH_cat[DAT_panel$numChHH_cat %in% 3] <- 2 # collapse 3 kids und more than 3 kids
w <- wtd.table(DAT_panel$numChHH_cat, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$eduLevel, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$empl_2019, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_panel$empl_2020, weights=DAT_panel$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

# FEM
DAT_fem <- DAT_panel[DAT_panel$sex==0,]
w <- wtd.table(DAT_fem$OW, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$mig, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$migback, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$ageYoungChild_cat, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$numChHH_cat, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$eduLevel, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$empl_2019, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_fem$empl_2020, weights=DAT_fem$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

# MALE
DAT_male <- DAT_panel[DAT_panel$sex==1,]
w <- wtd.table(DAT_male$OW, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$mig, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$migback, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$ageYoungChild_cat, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$numChHH_cat, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$eduLevel, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$empl_2019, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
w <- wtd.table(DAT_male$empl_2020, weights=DAT_male$phrf_cati)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

# ---------------------------------------------------------------------
# Properties of dep. variable
# ---------------------------------------------------------------------
# DV: 2019
DAT_panel_m <- DAT_panel[DAT_panel$sex %in% 1,]
table(DAT_panel_m$hoursCildCare_2019, exclude = NULL) # N=1 NA
weighted.mean(DAT_panel_m$hoursCildCare_2019, w=DAT_panel_m$phrf_cati, na.rm = TRUE)
DAT_panel_f <- DAT_panel[DAT_panel$sex %in% 0,]
table(DAT_panel_f$hoursCildCare_2019, exclude = NULL) # N=1 NA
weighted.mean(DAT_panel_f$hoursCildCare_2019, w=DAT_panel_f$phrf_cati, na.rm = TRUE)

# DV: 2020
table(DAT_panel_m$hoursCildCare_2020, exclude = NULL)
weighted.mean(DAT_panel_m$hoursCildCare_2020, w=DAT_panel_m$phrf_cati, na.rm = TRUE)
table(DAT_panel_f$hoursCildCare_2020, exclude = NULL)
weighted.mean(DAT_panel_f$hoursCildCare_2020, w=DAT_panel_f$phrf_cati, na.rm = TRUE)

# DV: 2020-2019
DAT_panel_m$diff <- DAT_panel_m$hoursCildCare_2020 - DAT_panel_m$hoursCildCare_2019
table(DAT_panel_m$diff, exclude = NULL) # N=1 NA
weighted.mean(DAT_panel_m$diff, w=DAT_panel_m$phrf_cati, na.rm = TRUE)
DAT_panel_f$diff <- DAT_panel_f$hoursCildCare_2020 - DAT_panel_f$hoursCildCare_2019
table(DAT_panel_f$diff, exclude = NULL) # N=1 NA
weighted.mean(DAT_panel_f$diff, w=DAT_panel_f$phrf_cati, na.rm = TRUE)

# Men Diff for distinct covariates / categories
DAT_panel_m$hours2019_cat <- ifelse(DAT_panel_m$hoursCildCare_2019 <=2,0, 
                                    ifelse(DAT_panel_m$hoursCildCare_2019>2 & DAT_panel_m$hoursCildCare_2019<=5,1,
                                           ifelse(DAT_panel_m$hoursCildCare_2019>5,2,NA)))  # N=1 NA
D <- DAT_panel_m[DAT_panel_m$hours2019_cat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2019_cat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2019_cat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 3,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageYoungChild_cat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageYoungChild_cat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageYoungChild_cat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)

# Men values in 2019 for distinct covariates / categories
D <- DAT_panel_m[DAT_panel_m$hours2019_cat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2019_cat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$hours2019_cat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2019 == 3,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2020 == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2020 == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2020 == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$empl_2020 == 3,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$eduLevel == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageYoungChild_cat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageYoungChild_cat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_m[DAT_panel_m$ageYoungChild_cat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)

# Fem Diff for distinct covariates / categories
DAT_panel_f$hours2019_cat <- ifelse(DAT_panel_f$hoursCildCare_2019 <=2,0, 
                                    ifelse(DAT_panel_f$hoursCildCare_2019>2 & DAT_panel_f$hoursCildCare_2019<=5,1,
                                           ifelse(DAT_panel_f$hoursCildCare_2019>5,2,NA)))  # N=1 NA
D <- DAT_panel_f[DAT_panel_f$hours2019_cat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2019_cat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2019_cat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 3,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageYoungChild_cat == 0,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageYoungChild_cat == 1,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageYoungChild_cat == 2,]
nrow(D)
weighted.mean(D$diff, w=D$phrf_cati, na.rm = TRUE)

# Women values in 2019 for distinct covariates / categories
D <- DAT_panel_f[DAT_panel_f$hours2019_cat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2019_cat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$hours2019_cat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2019 == 3,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)

D <- DAT_panel_f[DAT_panel_f$empl_2020 == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2020 == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2020 == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$empl_2020 == 3,]
nrow(D)
weighted.mean(D$hoursCildCare_2020, w=D$phrf_cati, na.rm = TRUE)


D <- DAT_panel_f[DAT_panel_f$eduLevel == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$eduLevel == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageYoungChild_cat == 0,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageYoungChild_cat == 1,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)
D <- DAT_panel_f[DAT_panel_f$ageYoungChild_cat == 2,]
nrow(D)
weighted.mean(D$hoursCildCare_2019, w=D$phrf_cati, na.rm = TRUE)

# Plot distribution of care hours for men and women
Dm <- rbind(cbind(DAT_panel_m$hoursCildCare_2019, DAT_panel_m$phrf_cati, 2019), 
            cbind(DAT_panel_m$hoursCildCare_2020, DAT_panel_m$phrf_cati, 2020))
Df <- rbind(cbind(DAT_panel_f$hoursCildCare_2019, DAT_panel_f$phrf_cati, 2019), 
            cbind(DAT_panel_f$hoursCildCare_2020, DAT_panel_f$phrf_cati, 2020))
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
 scale_x_discrete(labels=c("2019", "2020", "2019", "2020")) 

# Mean of changes & direction of change in percent
# Men in partnership
#wt <- wtd.table(DAT_panel_m$diff, weights=DAT_panel_m$phrf_cati)
wt <- wtd.table(DAT_panel_m$diff, weights=DAT_panel_m$phrf_cati, na.rm=TRUE)
sum(wt$sum.of.weights[wt$x<0])/sum(wt$sum.of.weights)
sum(wt$sum.of.weights[wt$x==0])/sum(wt$sum.of.weights)
sum(wt$sum.of.weights[wt$x>0])/sum(wt$sum.of.weights)
# Women in partnership
#wt <- wtd.table(DAT_panel_f$diff, weights=DAT_panel_f$phrf_cati)
wt <- wtd.table(DAT_panel_f$diff, weights=DAT_panel_f$phrf_cati, na.rm=TRUE)
sum(wt$sum.of.weights[wt$x<0])/sum(wt$sum.of.weights)
sum(wt$sum.of.weights[wt$x==0])/sum(wt$sum.of.weights)
sum(wt$sum.of.weights[wt$x>0])/sum(wt$sum.of.weights)


