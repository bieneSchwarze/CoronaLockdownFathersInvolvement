######################################################
######################################################
## Model it
## Time (hours/day) for Child Care Tasks 
## SOEP 2019 - CoV
## SZ, 04.06.2020
######################################################
#####################################################

rm(list=ls())
library(reshape2)
library(ggplot2)

# ---------------------------------------------------------------------
# Load Data
# ---------------------------------------------------------------------
setwd("...") # add path
load("_workspace_dataDescribed.RData") # load workspace stored from descriptives.R

# ---------------------------------------------------------------------------
# Function to get predicted diff times for categories & confidence intervals
# --------------------------------------------------------------------------
# FUNCTION linear model
getPredDiffTimes <- function(DDat) {

  MODEL <- lm(diff ~ hoursCildCare_2019 + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
                  as.factor(numChHH_cat) +  as.factor(eduLevel) + as.factor(empl_2019),  
                weights=phrf_cati, data = DDat)
  summary(MODEL)
  
  pr <- predict(MODEL, type="response")  
  
  m <- rep(NA,20)
  m[1] <- mean(pr[DDat$OW==0], na.rm=TRUE)
  m[2] <- mean(pr[DDat$OW==1], na.rm=TRUE)
  m[3] <- mean(pr[DDat$mig==0], na.rm=TRUE)
  m[4] <- mean(pr[DDat$mig==1], na.rm=TRUE)
  m[5] <- mean(pr[DDat$ageYoungChild_cat==0], na.rm=TRUE)
  m[6] <- mean(pr[DDat$ageYoungChild_cat==1], na.rm=TRUE)
  m[7] <- mean(pr[DDat$ageYoungChild_cat==2], na.rm=TRUE)
  m[8] <- mean(pr[DDat$numChHH_cat==0], na.rm=TRUE)
  m[9] <- mean(pr[DDat$numChHH_cat==1], na.rm=TRUE)
  m[10] <- mean(pr[DDat$numChHH_cat==2], na.rm=TRUE)
  m[11] <- mean(pr[DDat$eduLevel==0], na.rm=TRUE)
  m[12] <- mean(pr[DDat$eduLevel==1], na.rm=TRUE)
  m[13] <- mean(pr[DDat$eduLevel==2], na.rm=TRUE)
  m[14] <- mean(pr[DDat$empl_2019==0], na.rm=TRUE)
  m[15] <- mean(pr[DDat$empl_2019==1], na.rm=TRUE)
  m[16] <- mean(pr[DDat$empl_2019==2], na.rm=TRUE)
  m[17] <- mean(pr[DDat$empl_2019==3], na.rm=TRUE)
  m[18] <- mean(pr[DDat$hours2019_cat==0], na.rm=TRUE)
  m[19] <- mean(pr[DDat$hours2019_cat==1], na.rm=TRUE)
  m[20] <- mean(pr[DDat$hours2019_cat==2], na.rm=TRUE)
  names(m) <- c("east", "west", "noMig", "mig", "age02", "age35", "age611",
                "1ch", "2ch", "3+ch", "lowEdu", "medEdu", "highEdu", 
                "fullT", "partT", "notEmpl", "otherEmpl",
                "h02", "h35", "h6+")
  m <- round(m,2)

  makeTheBoot <- function(it){
    sam <- sample(size=nrow(DDat), x=1:nrow(DDat), replace=T)
    DDatB <- DDat[sam,]
    MODEL_boot <- lm(diff ~ hoursCildCare_2019 + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
                  as.factor(numChHH_cat) +  as.factor(eduLevel) + as.factor(empl_2019),  
                weights=phrf_cati, data = DDatB)
    prB <- predict(MODEL_boot, type="response")  
    mb <- rep(NA,20)
    mb[1] <- mean(prB[DDatB$OW==0], na.rm=TRUE)
    mb[2] <- mean(prB[DDatB$OW==1], na.rm=TRUE)
    mb[3] <- mean(prB[DDatB$mig==0], na.rm=TRUE)
    mb[4] <- mean(prB[DDatB$mig==1], na.rm=TRUE)
    mb[5] <- mean(prB[DDatB$ageYoungChild_cat==0], na.rm=TRUE)
    mb[6] <- mean(prB[DDatB$ageYoungChild_cat==1], na.rm=TRUE)
    mb[7] <- mean(prB[DDatB$ageYoungChild_cat==2], na.rm=TRUE)
    mb[8] <- mean(prB[DDatB$numChHH_cat==0], na.rm=TRUE)
    mb[9] <- mean(prB[DDatB$numChHH_cat==1], na.rm=TRUE)
    mb[10] <- mean(prB[DDatB$numChHH_cat==2], na.rm=TRUE)
    mb[11] <- mean(prB[DDatB$eduLevel==0], na.rm=TRUE)
    mb[12] <- mean(prB[DDatB$eduLevel==1], na.rm=TRUE)
    mb[13] <- mean(prB[DDatB$eduLevel==2], na.rm=TRUE)
    mb[14] <- mean(prB[DDatB$empl_2019==0], na.rm=TRUE)
    mb[15] <- mean(prB[DDatB$empl_2019==1], na.rm=TRUE)
    mb[16] <- mean(prB[DDatB$empl_2019==2], na.rm=TRUE)
    mb[17] <- mean(prB[DDatB$empl_2019==3], na.rm=TRUE)
    mb[18] <- mean(prB[DDatB$hours2019_cat==0], na.rm=TRUE)
    mb[19] <- mean(prB[DDatB$hours2019_cat==1], na.rm=TRUE)
    mb[20] <- mean(prB[DDatB$hours2019_cat==2], na.rm=TRUE)
    names(mb) <- c("east_germ", "west_germ", "noMig", "mig", "age02", "age35", "age611",
                  "1ch", "2ch", "3+ch", "lowEdu", "medEdu", "highEdu", 
                  "fullT", "partT", "notEmpl", "otherEmpl", 
                  "h02", "h35", "h6+")
    mb <- round(mb,2)    
    return(mb)
  }
  res <- sapply(1:200, makeTheBoot)
  qu <- apply(res, 1, quantile, probs = seq(from=0, to=1, by=0.025), na.rm=T)
  confLC <- 2*m-qu[40,] # 97,5% Perzentil
  confUC <- 2*m-qu[2,] # 2,5% Perzentil
  res <-  cbind(m, confLC, confUC)
  names(res) <- c("p.est", "ci.low", "ci.up")
  res <- round(res,2)
  return(res)
}

# FUNCTION linear model (without employment status 2019 as indep. var)
getPredDiffTimes_2 <- function(DDat) {

  MODEL <- lm(diff ~ hoursCildCare_2019 + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
                as.factor(numChHH_cat) +  as.factor(eduLevel),  
              weights=phrf_cati, data = DDat)
  summary(MODEL)
  
  pr <- predict(MODEL, type="response")  
  
  m <- rep(NA,16)
  m[1] <- mean(pr[DDat$OW==0], na.rm=TRUE)
  m[2] <- mean(pr[DDat$OW==1], na.rm=TRUE)
  m[3] <- mean(pr[DDat$mig==0], na.rm=TRUE)
  m[4] <- mean(pr[DDat$mig==1], na.rm=TRUE)
  m[5] <- mean(pr[DDat$ageYoungChild_cat==0], na.rm=TRUE)
  m[6] <- mean(pr[DDat$ageYoungChild_cat==1], na.rm=TRUE)
  m[7] <- mean(pr[DDat$ageYoungChild_cat==2], na.rm=TRUE)
  m[8] <- mean(pr[DDat$numChHH_cat==0], na.rm=TRUE)
  m[9] <- mean(pr[DDat$numChHH_cat==1], na.rm=TRUE)
  m[10] <- mean(pr[DDat$numChHH_cat==2], na.rm=TRUE)
  m[11] <- mean(pr[DDat$eduLevel==0], na.rm=TRUE)
  m[12] <- mean(pr[DDat$eduLevel==1], na.rm=TRUE)
  m[13] <- mean(pr[DDat$eduLevel==2], na.rm=TRUE)
  m[14] <- mean(pr[DDat$hours2019_cat==0], na.rm=TRUE)
  m[15] <- mean(pr[DDat$hours2019_cat==1], na.rm=TRUE)
  m[16] <- mean(pr[DDat$hours2019_cat==2], na.rm=TRUE)
  names(m) <- c("east", "west", "noMig", "mig", "age02", "age35", "age611",
                "1ch", "2ch", "3+ch", "lowEdu", "medEdu", "highEdu", 
                "h02", "h35", "h6+")
  m <- round(m,2)
  
  makeTheBoot <- function(it){
    sam <- sample(size=nrow(DDat), x=1:nrow(DDat), replace=T)
    DDatB <- DDat[sam,]
    MODEL_boot <- lm(diff ~ hoursCildCare_2019 + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
                       as.factor(numChHH_cat) +  as.factor(eduLevel),  
                     weights=phrf_cati, data = DDatB)
    prB <- predict(MODEL_boot, type="response")  
    mb <- rep(NA,16)
    mb[1] <- mean(prB[DDatB$OW==0], na.rm=TRUE)
    mb[2] <- mean(prB[DDatB$OW==1], na.rm=TRUE)
    mb[3] <- mean(prB[DDatB$mig==0], na.rm=TRUE)
    mb[4] <- mean(prB[DDatB$mig==1], na.rm=TRUE)
    mb[5] <- mean(prB[DDatB$ageYoungChild_cat==0], na.rm=TRUE)
    mb[6] <- mean(prB[DDatB$ageYoungChild_cat==1], na.rm=TRUE)
    mb[7] <- mean(prB[DDatB$ageYoungChild_cat==2], na.rm=TRUE)
    mb[8] <- mean(prB[DDatB$numChHH_cat==0], na.rm=TRUE)
    mb[9] <- mean(prB[DDatB$numChHH_cat==1], na.rm=TRUE)
    mb[10] <- mean(prB[DDatB$numChHH_cat==2], na.rm=TRUE)
    mb[11] <- mean(prB[DDatB$eduLevel==0], na.rm=TRUE)
    mb[12] <- mean(prB[DDatB$eduLevel==1], na.rm=TRUE)
    mb[13] <- mean(prB[DDatB$eduLevel==2], na.rm=TRUE)
    mb[14] <- mean(prB[DDatB$hours2019_cat==0], na.rm=TRUE)
    mb[15] <- mean(prB[DDatB$hours2019_cat==1], na.rm=TRUE)
    mb[16] <- mean(prB[DDatB$hours2019_cat==2], na.rm=TRUE)
    names(mb) <- c("east_germ", "west_germ", "noMig", "mig", "age02", "age35", "age611",
                   "1ch", "2ch", "3+ch", "lowEdu", "medEdu", "highEdu", 
                   "h02", "h35", "h6+")
    mb <- round(mb,2)    
    return(mb)
  }
  res <- sapply(1:200, makeTheBoot)
  qu <- apply(res, 1, quantile, probs = seq(from=0, to=1, by=0.025), na.rm=T)
  confLC <- 2*m-qu[40,] # 97,5% Perzentil
  confUC <- 2*m-qu[2,] # 2,5% Perzentil
  res <-  cbind(m, confLC, confUC)
  names(res) <- c("p.est", "ci.low", "ci.up")
  res <- round(res,2)
  return(res)
}

# FUNCTION FE panel
getPredDiffTimes_RE <- function(DDat) {

  MODEL <- lm(hoursCildCare  ~  time + as.factor(empl), weights=phrf_cati, data = DDat)
  pr <- predict(MODEL, type="response")  
  m <- rep(NA,6)
  m[1] <- mean(pr[DDat$time==1], na.rm=TRUE)
  m[2] <- mean(pr[DDat$time==2], na.rm=TRUE)
  m[3] <- mean(pr[DDat$empl==0], na.rm=TRUE)
  m[4] <- mean(pr[DDat$empl==1], na.rm=TRUE)
  m[5] <- mean(pr[DDat$empl==2], na.rm=TRUE)
  m[6] <- mean(pr[DDat$empl==3], na.rm=TRUE)
  names(m) <- c("2019", "2020", "fullT", "partT", "notEmpl", "otherEmpl")
  m <- round(m,2)
  
  makeTheBoot <- function(it){
    sam <- sample(size=nrow(DDat), x=1:nrow(DDat), replace=T)
    DDatB <- DDat[sam,]
    MODEL_boot <- lm(hoursCildCare  ~  as.factor(time) + as.factor(empl), weights=phrf_cati, data = DDatB)
    prB <- predict(MODEL_boot, type="response")  
    mb <- rep(NA,6)
    mb[1] <- mean(prB[DDatB$time==1], na.rm=TRUE)
    mb[2] <- mean(prB[DDatB$time==2], na.rm=TRUE)
    mb[3] <- mean(prB[DDatB$empl==0], na.rm=TRUE)
    mb[4] <- mean(prB[DDatB$empl==1], na.rm=TRUE)
    mb[5] <- mean(prB[DDatB$empl==2], na.rm=TRUE)
    mb[6] <- mean(prB[DDatB$empl==3], na.rm=TRUE)
    names(mb) <- c("2019", "2020", "fullT", "partT", "notEmpl", "otherEmpl")
    mb <- round(mb,2)    
    return(mb)
  }
  res <- sapply(1:200, makeTheBoot)
  qu <- apply(res, 1, quantile, probs = seq(from=0, to=1, by=0.025), na.rm=T)
  confLC <- 2*m-qu[40,] # 97,5% Perzentil
  confUC <- 2*m-qu[2,] # 2,5% Perzentil
  res <-  cbind(m, confLC, confUC)
  names(res) <- c("p.est", "ci.low", "ci.up")
  res <- round(res,2)
  return(res)
}

# ---------------------------------------------------------------------
# Test for strict exogeneity (using residuals)
# ---------------------------------------------------------------------
MODEL <- lm(diff ~ hoursCildCare_2019 + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
              as.factor(numChHH_cat) +  as.factor(eduLevel) + as.factor(empl_2019),  
            weights=phrf_cati, data = DAT_panel_m)
summary(lm(MODEL$residuals ~ DAT_panel_m$hoursCildCare_2019))

MODEL <- lm(diff ~ hoursCildCare_2019 + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
              as.factor(numChHH_cat) +  as.factor(eduLevel) + as.factor(empl_2019),  
            weights=phrf_cati, data = DAT_panel_f)
summary(lm(MODEL$residuals ~ DAT_panel_f$hoursCildCare_2019)) 

# ---------------------------------------------------------------------
# Model 1 (diff in diff)
# ---------------------------------------------------------------------
DAT_panel$diff <- DAT_panel$hoursCildCare_2020 - DAT_panel$hoursCildCare_2019
hist(DAT_panel$diff)

# Men in Partnership
res_m <- getPredDiffTimes(DAT_panel_m)
res_m <- getPredDiffTimes_2(DAT_panel_m)

mod_m_EW <- lm(diff ~ as.factor(hours2019_cat) + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
              as.factor(numChHH_cat) +  as.factor(eduLevel) + as.factor(empl_2019),  
            weights=phrf_cati, data = DAT_panel_m)
summary(mod_m_EW)

mod_m_oEW <- lm(diff ~ as.factor(hours2019_cat) + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
                 as.factor(numChHH_cat) +  as.factor(eduLevel),  
               weights=phrf_cati, data = DAT_panel_m)
summary(mod_m_oEW)

# Women in Partnership
res_f <- getPredDiffTimes(DAT_panel_f)
res_f <- getPredDiffTimes_2(DAT_panel_f)

mod_f_EW <- lm(diff ~ as.factor(hours2019_cat) + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
                 as.factor(numChHH_cat) +  as.factor(eduLevel) + as.factor(empl_2019),  
               weights=phrf_cati, data = DAT_panel_f)
summary(mod_f_EW)
mod_f_oEW <- lm(diff ~ as.factor(hours2019_cat) + as.factor(la) + OW + mig + as.factor(ageYoungChild_cat) + 
                  as.factor(numChHH_cat) +  as.factor(eduLevel),  
                weights=phrf_cati, data = DAT_panel_f)
summary(mod_f_oEW)

# Plot it
namM <- c("Eastern Germamny", "Western Germany", "no migration background", "migration background", 
          "age youngest child: 0-2", "age youngest child: 3-5", "age youngest child: 6-11",
          "number of children in household: 1", "number of children in household: 2", "number of children in household: 3 and more",
          "low education", "medium education", "high education", 
          "employment status 2019: full-time", "employment status 2019: part-time", "employment status 2019: not employed", "employment 2019: other",
          "care hours 2019: 0-2","care hours 2019: 3-5", "care hours 2019: 6 and more")
model_m <- data.frame(Predictor = namM,
                      B = res_m[,1],
                      CI_low= res_m[,2],
                      CI_up = res_m[,3],
                      Gender="Men")
model_m$Predictor <- as.factor(model_m$Predictor)
model_f <- data.frame(Predictor = namM,
                      B = res_f[,1],
                      CI_low= res_f[,2],
                      CI_up = res_f[,3],
                      Gender="Women")
model_f$Predictor <- as.factor(model_f$Predictor)

allModelFrame <- data.frame(rbind(model_f, model_m))
levNam <- rev(namM)
allModelFrame$Predictor <- factor(allModelFrame$Predictor, levels=c(levNam))
zp <- ggplot(allModelFrame, aes(colour = Gender))
zp <- zp + geom_hline(yintercept = 0, colour = gray(1/2), lty=2)
zp <- zp + geom_linerange(aes(x=Predictor, ymin= CI_low, ymax=CI_up),
                            lwd=1, position=position_dodge(width = 1/2))
zp <- zp + geom_point(aes(x=Predictor, y=B),
                        shape=21, fill="WHITE",
                        position=position_dodge(width = 1/2))
zp <- zp + coord_flip() + theme_bw() +  xlab("") +  
      ylab("difference in child care time\n(in hours per day)") 
print(zp)


# ---------------------------------------------------------------------
# Model 2 (fixed effects, panel)
# ---------------------------------------------------------------------
DAT_fe <- DAT_panel[,c("pid", "sex", "la", "hoursCildCare_2019", "hoursCildCare_2020", 
                       "empl_2019", "empl_2020", "phrf_cati")] 
colnames(DAT_fe)[4:7] <- c("hoursCildCare.2019", "hoursCildCare.2020", "empl.2019", "empl.2020")
DAT_long <- reshape(DAT_fe, idvar="pid", direction ="long", varying=list(c(4,5),c(6,7)),
                    v.names=c("hoursCildCare", "empl"))  
head(DAT_long)
DAT_long <- DAT_long[order(DAT_long$pid),]
head(DAT_long)

# Men in Partnership
DAT_long_m <- DAT_long[DAT_long$sex %in% 1 & DAT_long$la %in% c(0,1),]
MODEL01 <- lm(hoursCildCare  ~  as.factor(time) + as.factor(empl), weights=phrf_cati, data = DAT_long_m)
summary(MODEL01)
getPredDiffTimes_RE(DAT_long_m)

# Women in Partnership
DAT_long_f <- DAT_long[DAT_long$sex %in% 0 & DAT_long$la %in% c(0,1),]
MODEL02 <- lm(hoursCildCare  ~  as.factor(time) + as.factor(empl), weights=phrf_cati, data = DAT_long_f)
summary(MODEL02)
getPredDiffTimes_RE(DAT_long_f)

# ---------------------------------------------------------------------------
# Model 2 (fixed effects, panel), remove persons with children below age one
# ---------------------------------------------------------------------------
DAT_panel_res <- DAT_panel[DAT_panel$ageYoungChild>1,] # Note: age has been computed: 2019 - birthyear, thus several of the kids born in 2019 then may not have turn one until April-May 2020, thus be very conservative in this analysis taking age>1
DAT_res_fe <- DAT_panel_res[,c("pid", "sex", "la", "hoursCildCare_2019", "hoursCildCare_2020", 
                       "empl_2019", "empl_2020", "phrf_cati")] 
colnames(DAT_res_fe)[4:7] <- c("hoursCildCare.2019", "hoursCildCare.2020", "empl.2019", "empl.2020")
DAT_res_long <- reshape(DAT_res_fe, idvar="pid", direction ="long", varying=list(c(4,5),c(6,7)),
                    v.names=c("hoursCildCare", "empl"))  
head(DAT_res_long)
DAT_res_long <- DAT_res_long[order(DAT_res_long$pid),]
head(DAT_res_long)

# Men in Partnership
DAT_res_long_m <- DAT_res_long[DAT_res_long$sex %in% 1 & DAT_res_long$la %in% c(0,1),]
MODEL01 <- lm(hoursCildCare  ~  as.factor(time) + as.factor(empl), weights=phrf_cati, data = DAT_res_long_m)
summary(MODEL01)
getPredDiffTimes_RE(DAT_res_long_m)

# Women in Partnership
DAT_res_long_f <- DAT_res_long[DAT_res_long$sex %in% 0 & DAT_res_long$la %in% c(0,1),]
MODEL02 <- lm(hoursCildCare  ~  as.factor(time) + as.factor(empl), weights=phrf_cati, data = DAT_res_long_f)
summary(MODEL02)
getPredDiffTimes_RE(DAT_res_long_f)


