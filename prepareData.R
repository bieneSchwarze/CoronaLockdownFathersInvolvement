######################################################
######################################################
## DATA PREPARATION
## Time (hours/day) for Child Care Tasks 
## SOEP 2019 - CoV 2020
## SZ, 04.06.2020
######################################################
######################################################

library(haven)
library(questionr)
library(mice)

setwd("...") # add path

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Data 2019 (SOEP raw data, use preliminary version of SOEP long data)
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

COV <- read_dta("covid_T1T2T3T4.dta") # this is a data set extracted from the SOEP long data
COV2019 <- COV[COV$syear == 2019,]

# ---------------------------------------------------------------------
# Restict data set to (i) households with children aged 0-11 in 2019, (ii) parents aged 20-59, (iii) exclude single fathers
# ---------------------------------------------------------------------

# (i) Kids aged 0-11, i.e. born in 2008-2019
K0 <- COV2019[,c("pid", "bjk_87_02_1","bjk_87_02_2","bjk_87_02_3","bjk_87_02_4","bjk_87_02_5","bjk_87_02_6","bjk_87_02_7","bjk_87_02_8","bjk_87_02_9")]
K0 <- K0[apply(K0,1, function(rr){TRUE %in% !(table(is.na(rr[-1]))==(length(rr)-1))}),]
K1 <- K0[apply(K0, 1, function(rr){TRUE %in% (c(2008:2019) %in% unlist(rr[-1]))}),] # N=8144 persons with children aged 0-11 in the 2019 data

# (ii) Parents aged 20-59, i.e. born between 1960 and 1999
# Note: yet no birth year in 2019 data available, impute from years before (2011-2018)
COV201118 <- COV[COV$syear %in% 2011:2018,]
bb <- COV201118[,c("pid", "syear", "gebjahr")]
bb <- bb[order(bb$pid),]
bb <- bb[setdiff(1:nrow(bb), which(is.na(bb$gebjahr), arr.ind=TRUE)),]
bb <- bb[setdiff(1:nrow(bb), which(bb$gebjahr<0)),]
bb <- bb[order(bb$pid, bb$syear, decreasing = TRUE),]
bb <- bb[!duplicated(bb$pid),]
table(duplicated(bb[,1]))
table(K1$pid %in% bb$pid) # birth year only for N=7505 of the N=8144 with kids aged 0-11, i.e. miss birth year if N=639 persons
bb <- bb[bb$pid %in% K1$pid,]
K2 <- merge(K1, bb[,-2], by="pid") 
K2 <- K2[K2$gebjahr %in% c(1960:1999),] # N=7207

# (iii) exclude single fathers (so far: info only available for year 2018, take this info, TODO: update as soon as 2019 data is available)
S0 <- COV[COV$syear == 2018, c("pid", "sex", "hgtyp1hh")]
S1 <- merge(K2, S0, by="pid", all.x = TRUE)
table(S1$sex, S1$hgtyp1hh, exclude=NULL) # N=56 single fathers
single_fahters <- S1[S1$hgtyp1hh %in% 3 & S1$sex %in% 1, "pid"]
K3 <- S1[!(S1$pid %in% single_fahters), ] # N=7151

DAT <- K3[, c("pid", "gebjahr", "sex", "hgtyp1hh")]

# ---------------------------------------------------------------------
# Add Covariates
# ---------------------------------------------------------------------
# (i) age of the youngest child
ageYoungChild <- apply(K3[,2:10],1,function(rr){return(2019-max(rr, na.rm=TRUE))})
table(ageYoungChild, exclude=NULL)
DAT <- as.data.frame(cbind(DAT, ageYoungChild))

# (ii) age category for parents
agePar <- 2019 - DAT$gebjahr
table(agePar, exclude=NULL)
ageKat <- ifelse(agePar %in% 20:29, 0, ifelse(agePar %in% 30:39, 1, ifelse(agePar %in% 40:49, 2, ifelse(agePar %in% 50:59, 3, NA))))
table(ageKat, exclude=NULL)
DAT <- as.data.frame(cbind(DAT, ageKat))

# (iii) east / west Germany 
# Note: take information from 2018 SOEP data, if it is not available take information from previous years (go back yearwise)
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
DAT <- merge(DAT, ostWest, by="pid", all.x=TRUE) 
table(DAT$OW, exclude=NULL) # code: 0 east, 1 west

# (iv) CASMIN (edu level)
# Note: take information from 2018 SOEP data, if it is not available take information from previous years (go back yearwise)
casM <- as.data.frame(COV[COV$syear %in% 2011:2018, c("pid", "syear", "pgcasmin")])
casM <- casM[order(casM$pid),]
casM <- casM[setdiff(1:nrow(casM), which(is.na(casM$pgcasmin), arr.ind=TRUE)),]
casM <- casM[setdiff(1:nrow(casM), which(casM$pgcasmin<0)),]
casM <- casM[order(casM$pid, casM$syear, decreasing = TRUE),]
casM <- casM[!duplicated(casM$pid),]
DAT <- merge(DAT, casM[,-2], by="pid", all.x=TRUE)
DAT$eduLevel <-  ifelse(DAT$pgcasmin %in% c(0, 1, 2, 4), 0, ifelse(DAT$pgcasmin %in% c(3,5,6,7), 1, ifelse(DAT$pgcasmin %in% c(8,9), 2, NA)))
table(DAT$eduLevel, exclude=NULL) # code: 0 lowEdu, 1 medEdu, 2 highEdu; N=426 NA

# (v) Employment 
# Note: take information from 2019 SOEP data, if it is not available take information from previous years (go back yearwise, last observation carried forward)
table(COV2019$plb0022_h, exclude = NULL) # N=3856 NA for employment status in 2019
COV201119 <- COV[COV$syear %in% 2011:2019,]
erw <- COV201119[, c("pid", "syear", "plb0022_h")] 
erw <- erw[order(erw$pid),]
erw <- erw[setdiff(1:nrow(erw), which(is.na(erw$plb0022_h), arr.ind=TRUE)),]
erw <- erw[setdiff(1:nrow(erw), which(erw$plb0022_h<0)),]
erw <- erw[order(erw$pid, erw$syear, decreasing = TRUE),]
erw <- erw[!duplicated(erw$pid),]
DAT <- merge(DAT, erw[,-2], by="pid", all.x=TRUE)
table(DAT$plb0022_h, exclude=NULL) # Code: 1 fulltime, 2 parttime, 3 apprenticeship, 4 minor, 7 volunt. year, 8 work handicapted, 9 not working, 10 short-time
DAT$empl <- ifelse(DAT$plb0022_h %in% 1, 0, ifelse(DAT$plb0022_h %in% 2, 1, ifelse(DAT$plb0022_h %in% 9, 2, ifelse(DAT$plb0022_h %in% c(3,4,7,8,10),3,NA))))
table(DAT$empl, exclude=NULL)  
  
# (vi) Living arrangement (TODO)
la_2019 <- as.data.frame(COV2019[COV2019$pid %in% DAT$pid, c("pid","bjp_197", "bjp_276_q154")]) 
la_2019[which(la_2019<0, arr.ind = TRUE)] <- NA
table(la_2019$bjp_197, la_2019$bjp_276_q154, exclude=NULL) 
table(la_2019$bjp_197, exclude=NULL) # code: 3: single never married, 4 divorced, 5 widowed, 6 registered same-sex cohabitation, 8 married  
table(la_2019$bjp_276_q15, exclude=NULL) # code: 1 single never married, 2 married, 3 registered cohabitation, 4 divorced, 5 registered cohabitation cancelled, 6 widowed, 7 partner of registered cohabitation has died 
same_sex <- la_2019[la_2019[,2] %in% 6,1] # N=3, take out same-sex couples
DAT <- DAT[!(DAT$pid %in% same_sex),] # N=7148
la_2019 <- la_2019[!(la_2019$pid %in% same_sex),]
la_1 <- ifelse(la_2019[,2] %in% c(8,6) , 0, ifelse(la_2019[,2] %in% c(3,4,5), 1, NA)) 
la_2 <- ifelse(la_2019[,3] %in% c(2,3), 0, ifelse(la_2019[,3] %in% c(1,4,5,6,7), 1, NA)) 
la <- la_1
la[is.na(la)] <- la_2[is.na(la)] # code: 0 married & registered cohabitation, 1 single never married & divorced & widowed & cohab. cancelled (this last category: make later two categ. using info on single parents)
table(la, exclude=NULL) # N=4 NA 
DAT$la <- la
# Identify single mothers by hh-variable from 2018, TODO: update as soon as 2019 data is available
table(DAT$hgtyp1hh, DAT$la, exclude=NULL) # Take all hhtyp=3 as single parents, though the proportion of 351/(351+110)=76% of married among them is pretty high
DAT$la[DAT$hgtyp1hh %in% 3] <- 2 # code la: 0 married, 1 in union, 2 single parent
table(DAT$la, exclude=NULL)
round(table(DAT$la, exclude=NULL)/nrow(DAT),3)

# (vii) total number of children in the HH
numChHH <- apply(K0, 1, function(rr){return(length(rr[which(!is.na(rr))][-1]))})
numChHH  <- as.data.frame(cbind(K0$pid, numChHH))
colnames(numChHH) <- c("pid","numChHH")
DAT <- merge(DAT, numChHH, by="pid", all.x=TRUE)
table(DAT$numChHH, exclude = NULL)
DAT$numChHH_cat <- ifelse(DAT$numChHH %in% 1, 0, ifelse(DAT$numChHH %in% 2, 1, ifelse(DAT$numChHH %in% 3, 2, ifelse(DAT$numChHH %in% c(4:8), 3, NA))))
table(DAT$numChHH_cat, exclude=NULL)

# (viii) migration background
mig <- as.data.frame(COV[COV$syear %in% 2011:2018, c("pid", "syear", "migback")])
mig <- mig[order(mig$pid),]
mig <- mig[setdiff(1:nrow(mig), which(is.na(mig$migback), arr.ind=TRUE)),]
mig <- mig[setdiff(1:nrow(mig), which(mig$migback<0)),]
mig <- mig[order(mig$pid, mig$syear, decreasing = TRUE),]
mig <- mig[!duplicated(mig$pid),]
DAT <- merge(DAT, mig[,-2], by="pid", all.x=TRUE)
table(DAT$migback, exclude=NULL) # code: 1 no migback, 2 direct migback, 3 indirect migback

# ---------------------------------------------------------------------
# Add AV: Hours spend for child care, monday-friday 
# ---------------------------------------------------------------------
DAT <- merge(DAT,COV2019[, c("pid", "pli0044_v3")], by="pid", all.x=TRUE) # N=7148
colnames(DAT)[colnames(DAT) %in% "pli0044_v3"] <- "hoursCildCare_2019"
DAT$hoursCildCare_2019[DAT$hoursCildCare_2019<0] <- NA

# ---------------------------------------------------------------------
# Data 2020 (SOEP raw data, use preliminary version of SOEP long data)
# ---------------------------------------------------------------------
COV2020 <- COV[COV$syear %in% 2020,]

# ---------------------------------------------------------------------
# Create a balanced panel & add CATI (person) weigth 
# ---------------------------------------------------------------------
ww_cov <- read_dta("2020-06-04_SOEP_CoV_Gewichte.dta")
table(is.na(ww_cov$phrf_cati)) # no NA, ok
ww_cati <- ww_cov[ww_cov$phrf_cati > 0,] # N=5214 cases in CoV Tranche 1,2,3,4

COV2020_kids <- COV2020[COV2020$pkkinder %in% 1 | COV2020$pskinder %in% 1 ,] # N=1842 (here we only know that these kids are school children or children who do not yet go to school)
table(COV2020_kids$pid %in% ww_cati$pid) # N=79 not in weights file, check who this is
table(COV2020_kids[!(COV2020_kids$pid %in% ww_cati$pid), "tranche"]) # N=76: tranche 5 pre-data, okay no weight for these so far & three cases with zero weigths
COV2020_kids <- COV2020_kids[COV2020_kids$pid %in% ww_cati$pid,] # N=1763
table(DAT$pid %in% COV2020_kids$pid) # N=1051
table(ww_cati$pid %in% DAT$pid) # only N=1053 cases in 2019 and 2020 (with children 0-11 and parents aged 20-59)
pid1 <- DAT[DAT$pid %in% COV2020_kids$pid,]
pid2 <- ww_cati[ww_cati$pid %in% DAT$pid,]
setdiff(pid2$pid,pid1$pid) # N=2 cases have kids 0-11 in 2019, but stated not to have in 2020 (N=1053 in intersection 2019 & 2020 reduces to N=1051)
DAT_panel <- DAT[DAT$pid %in% pid1$pid,]
DAT_panel <- merge(DAT_panel, ww_cati[, c("pid", "phrf_cati")], by="pid", all.x=TRUE)

# ---------------------------------------------------------------------
# Add AV: Hours spend for child care, monday-friday 
# ---------------------------------------------------------------------
DAT_panel <- merge(DAT_panel,COV2020[, c("pid", "pli0044_v3")], by="pid", all.x=TRUE)
colnames(DAT_panel)[colnames(DAT_panel) %in% "pli0044_v3"] <- "hoursCildCare_2020"

# ---------------------------------------------------------------------
# Outlier
# ---------------------------------------------------------------------
dim(DAT_panel[DAT_panel$hoursCildCare_2019 >20 | DAT_panel$hoursCildCare_2020 >20,])

# ---------------------------------------------------------------------
# Add time-varying covariate: employment status
# ---------------------------------------------------------------------
DAT_panel <- merge(DAT_panel,COV2020[, c("pid", "plb0022_h")] , by="pid", all.x=TRUE)
colnames(DAT_panel)[colnames(DAT_panel) %in% "plb0022_h.x"] <- "employm_2019"
colnames(DAT_panel)[colnames(DAT_panel) %in% "plb0022_h.y"] <- "employm_2020"
colnames(DAT_panel)[colnames(DAT_panel) %in% "empl"] <- "empl_2019"
table(DAT_panel$employm_2020, exclude=NULL)
DAT_panel$empl_2020 <- ifelse(DAT_panel$employm_2020 %in% 1, 0, ifelse(DAT_panel$employm_2020 %in% 2, 1, 
                          ifelse(DAT_panel$employm_2020 %in% 9, 2, ifelse(DAT_panel$employm_2020 %in% c(3,4,5,7,12),3,NA))))
table(DAT_panel$empl_2020, exclude=NULL)  

# # ---------------------------------------------------------------------
# # Remove children 0-1 in 2019
# # ---------------------------------------------------------------------
# table(DAT_panel$ageYoungChild) 
# DAT_panel <- DAT_panel[DAT_panel$ageYoungChild >1,]

# ---------------------------------------------------------------------
# Selectivity Analysis
# ---------------------------------------------------------------------
S <- COV2020_kids[, c("pid", "psex", "pkkinder", "pskinder", "plb0022_h", "pli0044_v3")]
colnames(S)[5] <- "employm_2020"
colnames(S)[6] <- "hoursCildCare_2020"
S$empl2020 <- ifelse(S$employm_2020 %in% 1, 0, ifelse(S$employm_2020 %in% 2, 1, 
                              ifelse(S$employm_2020 %in% 9, 2, ifelse(S$employm_2020 %in% c(3,4,5,7,12),3,NA))))
S$pskinder[S$pskinder<0] <- NA
S <- merge(S, mig[,-2], by="pid", all.x=TRUE)
S <- merge(S, ostWest, by="pid", all.x=TRUE) 
S <- merge(S, casM[,-2], by="pid", all.x=TRUE)
S$eduLevel <-  ifelse(S$pgcasmin %in% c(0, 1, 2, 4), 0, 
                      ifelse(S$pgcasmin %in% c(3,5,6,7), 1, ifelse(S$pgcasmin %in% c(8,9), 2, NA)))
S <- merge(S, erw[,-2], by="pid", all.x=TRUE)
S$empl2019 <- ifelse(S$plb0022_h %in% 1, 0, ifelse(S$plb0022_h %in% 2, 1, 
                    ifelse(S$plb0022_h %in% 9, 2, ifelse(S$plb0022_h %in% c(3,4,7,8,10),3,NA))))
S$pn <- ifelse(COV2020_kids$pid %in% DAT_panel$pid,1,0)
table(S$psex, exclude=NULL) 
table(S$eduLevel, exclude=NULL) # N=36
table(S$hoursCildCare_2020, exclude=NULL) 
S$hoursCildCare_2020[S$hoursCildCare_2020<0] <- NA # N=1 NA
table(S$empl2019, exclude=NULL) # N=12 NA
table(S$empl2020, exclude=NULL) 
S <- S[, c("pid", "psex", "pkkinder", "pskinder", "empl2019", "empl2020", "migback", "OW", "eduLevel", "hoursCildCare_2020", "pn")]
# make a single imputation step
S$eduLevel <- as.factor(S$eduLevel)
S$empl2019 <- as.factor(S$empl2019)
S$pskinder <- as.factor(S$pskinder)
S$migback <- as.factor(S$migback) #N=3 NA
S$OW <- as.factor(S$OW) # N=3
predM <- mice::make.predictorMatrix(data=S)
impM <- mice::make.method(data=S)
impM["hoursCildCare_2020"] <- "cart"
imp1 <- mice::mice(S, m=1, predictorMatrix=predM, method=impM, maxit=20,seed=987)
imp1$loggedEvents # passt
SI <- complete(imp1, action=1)
sMod <- glm(pn ~ psex + as.factor(pkkinder) + as.factor(pskinder) +
         OW + as.factor(eduLevel) + as.factor(empl2019) + as.factor(empl2020) + 
         as.factor(migback) + hoursCildCare_2020, family=binomial(link = "logit"), data=SI)
summary(sMod)
which(coefficients(summary(sMod))[,4]<0.05)
# take only sign. effects
sMod <- glm(pn ~ psex +  OW + as.factor(eduLevel) + as.factor(migback), family=binomial(link = "logit"), data=SI)
summary(sMod)
phat <- predict(sMod, type="response", data=SI)
hist(phat)
par(mfrow=c(2,1))
hist(phat[S$pn==1])
hist(phat[S$pn==0]) # Look pretty similar. 
par(mfrow=c(1,1))
boxplot(phat ~ SI$pn) # Do not adjust weights for panel membership -> would just increase weights
range(phat[SI$pn==1])

