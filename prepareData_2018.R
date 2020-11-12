######################################################
######################################################
## DATA PREPARATION
## Time (hours/day) for Child Care Tasks 
## SOEP 2018 - 2019
## SZ, 17.09.2020
######################################################
######################################################

library(haven)
library(questionr)
library(mice)

setwd("...") # add path

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Data 2018 (SOEP raw data, use preliminary version of SOEP long data because of 2019 raw data)
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

COV <- read_dta("soep_cov_20200812_statav13.dta")
COV2018 <- COV[COV$syear == 2018,]
COV2019 <- COV[COV$syear == 2019,]
pl <- read_dta("ppathl.dta")
pl <- pl[!(pl$psample %in% c(17,18,19)),]

# ---------------------------------------------------------------------
# Restict data set to (i) households with children aged 0-11 in 2018, (ii) parents aged 20-59, (iii) exclude single parents
# ---------------------------------------------------------------------

pl2018 <- pl[pl$syear %in% 2018,]
table(pl2018$gebjahr, exclude=NULL)
kids_0to11 <- pl2018[pl2018$gebjahr %in% c(2007:2018), c("pid", "hid")] # N=11999 kids in SOEP 2018 data
parentMat <- NULL
# find their parents
for(i in 1:nrow(kids_0to11)){
 #i <- 422
 kidsID <- kids_0to11[i,]
 hhm <- pl2018[pl2018$hid %in% kidsID$hid, c("hid" ,"pid", "gebjahr")]
 hhm <- hhm[hhm$gebjahr>0,]
 hkm <- hhm[which(2018 - hhm$gebjahr <=18),] # number kids in household
 numKids <- nrow(hkm)
 ageYoungKid <- min(2018-hkm$gebjahr)
 gebJKid <- hhm[hhm$pid %in% kidsID$pid, "gebjahr"] 
 hhm <- hhm[-which(hhm$pid %in% kidsID$pid),]
 hhm <- hhm[which(2018 - hhm$gebjahr >=18),] # take out persons who are to young to have children
 hhm <- hhm[which(as.numeric(gebJKid) - as.numeric(hhm$gebjahr) >= 20),]
 hhm <- hhm[which(hhm$gebjahr %in% 1959:1998),]
 if(nrow(hhm)>0)
  parentMat <- rbind(parentMat, as.data.frame(cbind(as.data.frame(hhm),ageYoungKid,numKids))) 

 if(i%%500 %in%0)
   cat("It:",i,"\n")
} 
colnames(parentMat) <- c("hid","pid", "gebjahr", "ageYoungKid", "numKids") 
dim(parentMat)
parentMat <- parentMat[order(parentMat$pid),] # one line for each child (i.e. parents are present in the data as often as there are children at HH), we do not need this - only for the youngest child
parentMat <- parentMat[!duplicated(parentMat$pid),] 
dim(parentMat) # N=8048 youngest children in hh with parents aged 20-59 in 2018 (but this also means that we have 2 parents in couple households)
parentMat <- parentMat[order(parentMat$hid, parentMat$pid),]
head(parentMat)
tabNumParentsInHH <- aggregate(parentMat$numKids, by=list(parentMat$hid), length)
table(tabNumParentsInHH$x) # there are hh with more than 2 adults, take these hh (N=37) out, because I don't know what kind of adults they are (shared flats?)
hid_raus <- tabNumParentsInHH$Group.1[which(tabNumParentsInHH$x>2)]
parentMat <- parentMat[!(parentMat$hid %in% hid_raus),] # N=7909

# exclude single parents
parentMat <- merge(parentMat, COV[COV$syear == 2018, c("pid", "sex", "hgtyp1hh")], by="pid", all.x = TRUE)
table(parentMat$sex, parentMat$hgtyp1hh, exclude=NULL) # N=51 single fathers & N=481 single mothers, N=4 miss-specified persons (living alone & no children)
parentMat <- parentMat[parentMat$hgtyp1hh %in% c(4,5,6,7,8, NA),]
table(parentMat$sex, parentMat$hgtyp1hh, exclude=NULL) # 1 male, 2 female parents

# ---------------------------------------------------------------------
# Add Covariates
# ---------------------------------------------------------------------
# East / West Germany 
# Note: take information from 2018 SOEP data, if it is not available take information from previous years (go back yearwise)
COV201118 <- COV[COV$syear %in% 2011:2018,]
pidBula <- COV201118[,c("pid", "syear", "bula")]
pidBula <- pidBula[order(pidBula$pid),]
pidBula <- pidBula[setdiff(1:nrow(pidBula), which(is.na(pidBula$bula), arr.ind=TRUE)),]
pidBula <- pidBula[setdiff(1:nrow(pidBula), which(pidBula$bula<0)),]
pidBula <- pidBula[order(pidBula$pid, pidBula$syear, decreasing = TRUE),]
pidBula <- pidBula[!duplicated(pidBula$pid),]
ostWest <- cbind(as.numeric(pidBula$pid), ifelse(as.numeric(pidBula$bula) %in% 1:10, 1,0)) # 1 west, 0 ost
colnames(ostWest) <- c("pid", "OW")
ostWest <- as.data.frame(ostWest[order(ostWest[,1]),])
table(duplicated(ostWest[,1]))
ostWest <- ostWest[!duplicated(ostWest[,1]),]
parentMat <- merge(parentMat, ostWest, by="pid", all.x=TRUE) 
table(parentMat$OW, exclude=NULL) # code: 0 east, 1 west

# CASMIN (edu level)
# Note: take information from 2018 SOEP data, if it is not available take information from previous years (go back yearwise)
casM <- as.data.frame(COV[COV$syear %in% 2011:2018, c("pid", "syear", "pgcasmin")])
casM <- casM[order(casM$pid),]
casM <- casM[setdiff(1:nrow(casM), which(is.na(casM$pgcasmin), arr.ind=TRUE)),]
casM <- casM[setdiff(1:nrow(casM), which(casM$pgcasmin<0)),]
casM <- casM[order(casM$pid, casM$syear, decreasing = TRUE),]
casM <- casM[!duplicated(casM$pid),]
parentMat <- merge(parentMat, casM[,-2], by="pid", all.x=TRUE)
parentMat$eduLevel <-  ifelse(parentMat$pgcasmin %in% c(0, 1, 2, 4), 0, ifelse(parentMat$pgcasmin %in% c(3,5,6,7), 1, ifelse(parentMat$pgcasmin %in% c(8,9), 2, NA)))
table(parentMat$eduLevel, exclude=NULL) # code: 0 lowEdu, 1 medEdu, 2 highEdu; N=426 NA

# Employment 2018 
# Note: take information from 2018 SOEP data, if it is not available take information from previous years (go back yearwise, last observation carried forward)
table(COV2018$plb0022_h[COV2018$pid %in% parentMat$pid], exclude = NULL) # N=3856 NA for employment status in 2019
erw <- COV201118[, c("pid", "syear", "plb0022_h")] 
erw <- erw[order(erw$pid),]
erw <- erw[setdiff(1:nrow(erw), which(is.na(erw$plb0022_h), arr.ind=TRUE)),]
erw <- erw[setdiff(1:nrow(erw), which(erw$plb0022_h<0)),]
erw <- erw[order(erw$pid, erw$syear, decreasing = TRUE),]
erw <- erw[!duplicated(erw$pid),]
parentMat <- merge(parentMat, erw[,-2], by="pid", all.x=TRUE)
table(parentMat$plb0022_h, exclude=NULL) # Code: 1 fulltime, 2 parttime, 3 apprenticeship, 4 minor, 7 volunt. year, 8 work handicapted, 9 not working, 10 short-time
parentMat$empl2018 <- ifelse(parentMat$plb0022_h %in% 1, 0, ifelse(parentMat$plb0022_h %in% 2, 1, ifelse(parentMat$plb0022_h %in% 9, 2, ifelse(parentMat$plb0022_h %in% c(3,4,7,8,10),3,NA))))
table(parentMat$empl2018, exclude=NULL)  
parentMat <- parentMat[,!(colnames(parentMat) %in% "plb0022_h")]

# Employment 2019 
# Note: take information from 2019 SOEP data, if it is not available take information from previous years (go back yearwise, last observation carried forward)
table(COV2019$plb0022_h[COV2019$pid %in% parentMat$pid], exclude = NULL) # N=3856 NA for employment status in 2019
COV201119 <- COV[COV$syear %in% 2011:2019,]
erw <- COV201119[, c("pid", "syear", "plb0022_h")] 
erw <- erw[order(erw$pid),]
erw <- erw[setdiff(1:nrow(erw), which(is.na(erw$plb0022_h), arr.ind=TRUE)),]
erw <- erw[setdiff(1:nrow(erw), which(erw$plb0022_h<0)),]
erw <- erw[order(erw$pid, erw$syear, decreasing = TRUE),]
erw <- erw[!duplicated(erw$pid),]
parentMat <- merge(parentMat, erw[,-2], by="pid", all.x=TRUE)
table(parentMat$plb0022_h, exclude=NULL) # Code: 1 fulltime, 2 parttime, 3 apprenticeship, 4 minor, 7 volunt. year, 8 work handicapted, 9 not working, 10 short-time
parentMat$empl2019 <- ifelse(parentMat$plb0022_h %in% 1, 0, ifelse(parentMat$plb0022_h %in% 2, 1, ifelse(parentMat$plb0022_h %in% 9, 2, ifelse(parentMat$plb0022_h %in% c(3,4,7,8,10),3,NA))))
table(parentMat$empl2019, exclude=NULL)  
parentMat <- parentMat[,!(colnames(parentMat) %in% "plb0022_h")]  
table(parentMat$empl2018, parentMat$empl2019, exclude=NULL) 

# Migration background
mig <- as.data.frame(COV[COV$syear %in% 2011:2018, c("pid", "syear", "migback")])
mig <- mig[order(mig$pid),]
mig <- mig[setdiff(1:nrow(mig), which(is.na(mig$migback), arr.ind=TRUE)),]
mig <- mig[setdiff(1:nrow(mig), which(mig$migback<0)),]
mig <- mig[order(mig$pid, mig$syear, decreasing = TRUE),]
mig <- mig[!duplicated(mig$pid),]
parentMat <- merge(parentMat, mig[,-2], by="pid", all.x=TRUE)
table(parentMat$migback, exclude=NULL) # code: 1 no migback, 2 direct migback, 3 indirect migback

# Hours spend for child care 2018, monday-friday 
parentMat <- merge(parentMat,COV2018[, c("pid", "pli0044_v3")], by="pid", all.x=TRUE)
colnames(parentMat)[colnames(parentMat) %in% "pli0044_v3"] <- "hoursCildCare_2018"
parentMat$hoursCildCare_2018[parentMat$hoursCildCare_2018<0] <- NA

# Hours spend for child care 2019, monday-friday 
parentMat <- merge(parentMat,COV2019[, c("pid", "pli0044_v3")], by="pid", all.x=TRUE)
colnames(parentMat)[colnames(parentMat) %in% "pli0044_v3"] <- "hoursCildCare_2019"
parentMat$hoursCildCare_2019[parentMat$hoursCildCare_2019<0] <- NA
table(parentMat$hoursCildCare_2018,parentMat$hoursCildCare_2019)

# Weighted freq. of migback in 2019: direct 26%
w18 <- COV2018[,c("pid","phrf")]
parentMat <- merge(parentMat,w18, by="pid", all.x=TRUE)
table(is.na(parentMat$phrf)) # alle haben Gewicht
tt <- wtd.table(parentMat$migback, weights=parentMat$phrf)
tt/sum(tt)

# ---------------------------------------------------------------------
# Create a balanced panel & add CATI (person) weigth 
# ---------------------------------------------------------------------
table(is.na(parentMat$hoursCildCare_2018),is.na(parentMat$hoursCildCare_2019)) # balanced panel N=4503 (~61% of N=7373)
DAT_panel <- parentMat[!is.na(parentMat$hoursCildCare_2018) & !is.na(parentMat$hoursCildCare_2019),]
table(DAT_panel$hoursCildCare_2018)
table(DAT_panel$hoursCildCare_2019)

# # ---------------------------------------------------------------------
# # Remove children 0-1 in 2019
# # ---------------------------------------------------------------------
# table(DAT_panel$ageYoungChild) 
# DAT_panel <- DAT_panel[DAT_panel$ageYoungChild >1,]

# ---------------------------------------------------------------------
# Selectivity Analysis
# ---------------------------------------------------------------------
S <- parentMat
S$pn <- ifelse(parentMat$pid %in% DAT_panel$pid,1,0)
table(S$pn)

M <- md.pattern(S, plot=FALSE) 
round(M[nrow(M),]/nrow(S),2) # up to 36% missing values
table(complete.cases(S)) # ~60% with complete data lines

# HH cluster -> consider women and men separately
# Selection model for women
S_fem <- S[S$sex %in% 2, !(colnames(S)%in% "sex")] 
predM <- mice::make.predictorMatrix(data=S_fem)
predM[,1:2] <- 0
predM[1:2,] <- 0
impM <- mice::make.method(data=S_fem)
impM[impM != ""] <- "cart"
impF <- mice::mice(S_fem, m=20, predictorMatrix=predM, method=impM, maxit=20,seed=987)
impF$loggedEvents # passt
modS_Fem <- with(impF, glm(pn ~ gebjahr+ ageYoungKid + numKids + as.factor(hgtyp1hh) + OW + 
                       as.factor(eduLevel) + as.factor(empl2018) + as.factor(empl2019) +
                       as.factor(migback) + hoursCildCare_2018 + hoursCildCare_2019, 
                       family=binomial(link = "logit")))
smod_Fem <- summary(pool(modS_Fem))
rownames(smod_Fem)[which(smod_Fem[,5]<0.05)]
prIm <- matrix(NA, ncol=impF$m, nrow=nrow(S_fem))
for(i in 1:impF$m){
  datIm <- complete(impF, action=i)
  modImp <- glm(pn ~ gebjahr + ageYoungKid + numKids + as.factor(empl2019) + as.factor(migback), family=binomial(link = "logit"), data=datIm)
  prIm[,i] <- predict(modImp, type="response")
}
phat <- apply(prIm, 1, mean)
hist(phat)
par(mfrow=c(2,1))
hist(phat[S_fem$pn==1])
hist(phat[S_fem$pn==0]) # Look pretty similar. 
par(mfrow=c(1,1))
boxplot(phat ~ S_fem$pn) # Do not adjust weights for panel membership -> would just increase weights
range(phat[S_fem$pn==1])

# Selection model for men
S_male <- S[S$sex %in% 1, !(colnames(S)%in% "sex")] 
predM <- mice::make.predictorMatrix(data=S_male)
predM[,1:2] <- 0
predM[1:2,] <- 0
impM <- mice::make.method(data=S_male)
impM[impM != ""] <- "cart"
impM <- mice::mice(S_male, m=20, predictorMatrix=predM, method=impM, maxit=20,seed=257)
impM$loggedEvents # passt
modS_Male <- with(impM, glm(pn ~ gebjahr+ ageYoungKid + numKids + as.factor(hgtyp1hh) + OW + 
                             as.factor(eduLevel) + as.factor(empl2018) + as.factor(empl2019) +
                             as.factor(migback) + hoursCildCare_2018 + hoursCildCare_2019, 
                           family=binomial(link = "logit")))
smod_Male <- summary(pool(modS_Male))
rownames(smod_Male)[which(smod_Male[,5]<0.05)]
prIm <- matrix(NA, ncol=impM$m, nrow=nrow(S_male))
for(i in 1:impM$m){
  datIm <- complete(impM, action=i)
  modImp <- glm(pn ~ gebjahr + ageYoungKid + numKids + as.factor(empl2019) + as.factor(migback), family=binomial(link = "logit"), data=datIm)
  prIm[,i] <- predict(modImp, type="response")
}
phat <- apply(prIm, 1, mean)
hist(phat)
par(mfrow=c(2,1))
hist(phat[S_male$pn==1])
hist(phat[S_male$pn==0]) # Look pretty similar. 
par(mfrow=c(1,1))
boxplot(phat ~ S_male$pn) # Do not adjust weights for panel membership -> would just increase weights
range(phat[S_male$pn==1])



