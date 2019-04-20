# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# R2D2
# Panel descirptive manuscript analysis
# Amy Finnegan, amy.finnegan@duke.edu
# .............................................................................
# This is the analysis file for the descriptive paper using the panel data
# published in PLOS One 2019
# 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# setup
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load ========================================================================
library(Rmisc)
library(ggplot2)
library(stargazer)
library(alluvial)
library(reshape2)
library(tools) # toTitleCase
library(tidyr)
library(ggalluvial)
library(Cairo)
library(gridExtra)
library(grid)
library(lattice)
library(useful) # vplayout
library(survminer)
library(survival)
library(stringr)
library(xlsx)
library(xtable)
library(dplyr)

# document settings ===========================================================

# formatting ------------------------------------------------------------------
# color blind
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7") # The palette with grey
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
# dghi colors
orange <- "#f09906"
blue <- "#04598fff"
green <- "#849a0bff"
darkgrey <- "#888888ff"
red <- "#cc4125"

# blue colors =================================================================
colfunc <- colorRampPalette(c("skyblue", "navyblue"))
colfunc(10)

# functions ===================================================================
# rounding functions to keep trailing zeros -----------------------------------
rd0 <- function(y) sprintf("%.0f", round(y, 0))
rd1 <- function(y) sprintf("%.1f", round(y, 1))
rd2 <- function(y) sprintf("%.2f", round(y, 2))
rd3 <- function(y) sprintf("%.3f", round(y, 3))

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# DATA
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load cleaned data  ----------------------------------------------------------
load("public/input/datL.RData")

# create wide verison for analysis --------------------------------------------
datW <-
  datL %>%
  reshape(.,
          idvar="pid",
          timevar="wave",
          direction="wide")
names(datW) <- gsub(x = names(datW), pattern = "\\_", replacement = ".")


# load scaleInfo
scaleInfo <- read.xlsx2("public/resources/scales.1.1.xlsx", sheetName="alphas")
scaleInfo <- subset(scaleInfo,
                    select=c("scales", "english", "valence", "possMin", "possMax"))

# merge english to questions
dictionary <- read.xlsx2("public/resources/dictionary.xlsx", sheetName="survey-rd1")
dictionary <- subset(dictionary, 
                     select=c("name", "english"))
vars <- names(datL)
vars <- data.frame(vars)
qLookUp <- merge(vars, dictionary, by.x="vars", by.y="name")

## age dummies -----------------------------------------------------------------
datW$age12 <- ifelse(datW$c.age.r1 >= 12, "12+ yrs", "< 12 yrs")
datW$age10 <- ifelse(datW$c.age.r1 >= 10, "10+ yrs", "< 10 yrs")

# age learned and who told, how satisfied =====================================

# age learned chronic - r1 = doesKnow15, r2/r3 = doesKnow12now
datW$doesKnow15.r2 <- ifelse(datW$doesKnow12now.r2=="yes, child knows", datW$c.age.r1, NA)
datW$doesKnow15.r3 <- ifelse(datW$doesKnow12now.r3=="yes, child knows", datW$c.age.r1+1, NA)

datW$ageLearnedChronic <- as.numeric(as.character(datW$doesKnow15.r1))
datW$ageLearnedChronic <- ifelse(is.na(datW$ageLearnedChronic), datW$doesKnow15.r2, datW$ageLearnedChronic)
datW$ageLearnedChronic <- ifelse(is.na(datW$ageLearnedChronic), datW$doesKnow15.r3, datW$ageLearnedChronic)

# age learned HIV - r1 = doesKnow19, r2/r3 = doesKnow16now
datW$doesKnow19.r2 <- ifelse(datW$doesKnow16now.r2=="yes, child knows", datW$c.age.r1, NA)
datW$doesKnow19.r3 <- ifelse(datW$doesKnow16now.r3=="yes, child knows", datW$c.age.r1+1, NA)

datW$ageLearnedHIV <- as.numeric(as.character(datW$doesKnow19.r1))
datW$ageLearnedHIV <- ifelse(is.na(datW$ageLearnedHIV), datW$doesKnow19.r2, datW$ageLearnedHIV)
datW$ageLearnedHIV <- ifelse(is.na(datW$ageLearnedHIV), datW$doesKnow19.r3, datW$ageLearnedHIV)

datW$doesKnow23.r2 <- ifelse(datW$doesKnow20.r2=="yes, child knows", datW$c.age.r1, NA) 
datW$doesKnow23.r3 <- ifelse(datW$doesKnow20now.r3=="yes, child knows" | datW$doesKnow20.r3=="yes, child knows", datW$c.age.r1+1, NA)

datW$ageLearnedHowInf <- as.numeric(as.character(datW$doesKnow23.r1))
datW$ageLearnedHowInf <- ifelse(is.na(datW$ageLearnedHowInf), datW$doesKnow23.r2, datW$ageLearnedHowInf)
datW$ageLearnedHowInf <- ifelse(is.na(datW$ageLearnedHowInf), datW$doesKnow23.r3, datW$ageLearnedHowInf)

datW$doesKnow27.r2 <- ifelse(datW$doesKnow24.r2=="yes, child knows", datW$c.age.r1, NA)
datW$doesKnow27.r3 <- ifelse(datW$doesKnow24now.r3=="yes, child knows" | datW$doesKnow24.r3=="yes, child knows", datW$c.age.r1+1, NA)

datW$ageLearnedCanSpread <- as.numeric(as.character(datW$doesKnow27.r1))
datW$ageLearnedCanSpread <- ifelse(is.na(datW$ageLearnedCanSpread), datW$doesKnow27.r2, datW$ageLearnedCanSpread)
datW$ageLearnedCanSpread <- ifelse(is.na(datW$ageLearnedCanSpread), datW$doesKnow27.r3, datW$ageLearnedCanSpread)

# merge ageLearned variables to long df
ageLearned <- subset(datW,
                     select=c("pid",
                              "ageLearnedChronic",
                              "ageLearnedHIV",
                              "ageLearnedCanSpread",
                              "ageLearnedHowInf"))

datL <- merge(datL, ageLearned, by="pid")
addmargins(table(datL$ageLearnedHIV, datL$wave, exclude=NULL))

mAgeLearnedHIV <- rd1(mean(datW$ageLearnedHIV, na.rm=T))
sdAgeLearnedHIV <- rd1(sd(datW$ageLearnedHIV, na.rm=T))

# child's age at each wave
datL <- within(datL, {childAge=ave(c_age, pid, FUN=function(x) max(x, na.rm=T))}) # child age
datL$childAge[datL$wave=="r2"] <- datL$childAge[datL$wave=="r2"] + .5 # age by 6 months 
datL$childAge[datL$wave=="r3"] <- datL$childAge[datL$wave=="r3"] + 1 # age by 1 year

# child age at baseline
datL <- within(datL, {childAgeB=ave(c_age, pid, FUN=function(x) max(x, na.rm=T))}) # child age

# learns HIV
datL <- within(datL, {everKnowsHIV=ave(p_knowsHIV, pid, FUN=function(x) mean(x, na.rm=T))})
datL$everKnowsHIV[datL$everKnowsHIV != 0] <- 1

# age full disclosure
datW$ageFullDisclosure <- pmax(datW$ageLearnedCanSpread, datW$ageLearnedHowInf, na.rm=T)
datW$ageFullDisclosure[datW$p.fullDisclosure.r3==0] <- NA 
table(datW$ageFullDisclosure, datW$p.fullDisclosure.r3, exclude=NULL)

# age full disclosure for wave 1
datW$ageFullDisclosure.r1 <- datW$ageFullDisclosure
datW$ageFullDisclosure.r1[datW$p.fullDisclosure.r1==0] <- NA

# age partial disclosure
datW$agePartialDisclosure <- pmin(datW$ageLearnedCanSpread, datW$ageLearnedHowInf, na.rm=T)


# wave learned each element of HIV status -------------------------------------

# HIV
datW$waveLearnedHIV <- "does not know"
datW$waveLearnedHIV[datW$p.knowsHIV.r1==1] <- "r1"
datW$waveLearnedHIV[datW$p.knowsHIV.r2==1 & datW$p.knowsHIV.r3==1 & datW$waveLearnedHIV =="does not know"] <- "r2"
datW$waveLearnedHIV[datW$p.knowsHIV.r2==0 & datW$p.knowsHIV.r3==1 & datW$waveLearnedHIV =="does not know"] <- "r3"
datW$waveLearnedHIV[datW$p.knowsHIV.r2==1 & is.na(datW$p.knowsHIV.r3) & datW$waveLearnedHIV =="does not know"] <- "r2"
datW$waveLearnedHIV[is.na(datW$p.knowsHIV.r2) & datW$p.knowsHIV.r3==1 & datW$waveLearnedHIV =="does not know"] <- "r3"
table(datW$waveLearnedHIV, exclude=NULL)

# can spread
datW$waveLearnedCanSpread <- "does not know"
datW$waveLearnedCanSpread[datW$p.knowsCanSpread.r1==1] <- "r1"
datW$waveLearnedCanSpread[datW$p.knowsCanSpread.r2==1 & datW$p.knowsCanSpread.r3==1 & datW$waveLearnedCanSpread =="does not know"] <- "r2"
datW$waveLearnedCanSpread[datW$p.knowsCanSpread.r2==0 & datW$p.knowsCanSpread.r3==1 & datW$waveLearnedCanSpread =="does not know"] <- "r3"
datW$waveLearnedCanSpread[datW$p.knowsCanSpread.r2==1 & is.na(datW$p.knowsCanSpread.r3) & datW$waveLearnedCanSpread =="does not know"] <- "r2"
datW$waveLearnedCanSpread[is.na(datW$p.knowsCanSpread.r2) & datW$p.knowsCanSpread.r3==1 & datW$waveLearnedCanSpread =="does not know"] <- "r3"
table(datW$waveLearnedCanSpread, exclude=NULL)

# how infected
datW$waveLearnedHowInf <- "does not know"
datW$waveLearnedHowInf[datW$p.knowsHowInf.r1==1] <- "r1"
datW$waveLearnedHowInf[datW$p.knowsHowInf.r2==1 & datW$p.knowsHowInf.r3==1 & datW$waveLearnedHowInf =="does not know"] <- "r2"
datW$waveLearnedHowInf[datW$p.knowsHowInf.r2==0 & datW$p.knowsHowInf.r3==1 & datW$waveLearnedHowInf =="does not know"] <- "r3"
table(datW$waveLearnedHowInf, exclude=NULL)

# chronic
datW$waveLearnedChronic <- "does not know"
datW$waveLearnedChronic[datW$p.knowsChronic.r1==1] <- "r1"
datW$waveLearnedChronic[datW$p.knowsChronic.r2==1 & datW$p.knowsChronic.r3==1 & datW$waveLearnedChronic =="does not know"] <- "r2"
datW$waveLearnedChronic[datW$p.knowsChronic.r2==0 & datW$p.knowsChronic.r3==1 & datW$waveLearnedChronic =="does not know"] <- "r3"
table(datW$waveLearnedChronic, exclude=NULL)


# disclosure status (excluding chronic) =====================================

datL$p_disclosureStatusNoChronic <- 
  ifelse(datL$p_knowsHIV==0, "No disclosure (none of b-d)",
  ifelse(datL$p_knowsHIV==1 & datL$p_knowsCanSpread==1 & datL$p_knowsHowInf==1, "Full disclosure (all three of b-d)",
  ifelse(datL$p_knowsHIV==1 & (datL$p_knowsCanSpread==0 | datL$p_knowsHowInf==0), "Partial disclosure (at least one of b-d)", "WTF")))
datL$p_disclosureStatusNoChronic <- factor(datL$p_disclosureStatusNoChronic,
                                           levels=c("No disclosure (none of b-d)",
                                                    "Partial disclosure (at least one of b-d)",
                                                    "Full disclosure (all three of b-d)"))
prop.table(table(datL$p_disclosureStatusNoChronic, datL$wave), 2)

nc <- subset(datL,
             select=c("pid", "wave", "p_disclosureStatusNoChronic")) %>%
  reshape(.,
          idvar="pid",
          timevar="wave",
          direction="wide") 
datW <- merge(nc, datW, by.x="pid", by.y="pid")

# respondents at each wave ==================================================

# number of observations
datR1 <- subset(datW, round.r1==1)
datR2 <- subset(datW, round.r2==2)
datR3 <- subset(datW, round.r3==3)

# wave 1
nAllR1 <- length(datR1$pid)
nDiscR1 <- length(datR1$pid[datR1$p.knowsHIV.r1==1])
nNDiscR1 <- length(datR1$pid[datR1$p.knowsHIV.r1==0])

full.prev.r1 <- rd1(mean(datW$p.fullDisclosure.r1, na.rm=T)*100)
discBy12 <- rd1(((sum(datW$p.knowsHIV.r1, na.rm=T) + sum(datW$p.knowsHIV.r3, na.rm=T))/372)*100)

full.r1 <- rd1(length(datR1$pid[datR1$p.disclosureNPF.r1=="full disclosure"])/nDiscR1*100)
none.r1 <- rd1(length(datR1$pid[datR1$p.disclosureNPF.r1=="no disclosure"])/nAllR1*100)

pctFoundR1 <- rd1((nAllR1/400)*100)

# wave 2
nAllR2 <- length(datR2$pid)
nDiscR2 <- length(datR2$pid[datR2$p.knowsHIV.r2==1])
nNDiscR2 <- length(datR2$pid[datR2$p.knowsHIV.r2==0])
pctFoundR2 <- rd1(nAllR2/nNDiscR1*100)
knowsHIV.r2 <- rd1(mean(datW$p.knowsHIV.r2, na.rm=T)*100)
learnedHIV.r2 <- nDiscR2

# wave 3
nAllR3 <- length(datR3$pid)
nDiscR3 <- length(datR3$pid[datR2$p.knowsHIV.r3==1])
nNDiscR3 <- length(datR3$pid[datR2$p.knowsHIV.r3==0 & !is.na(datR2$p.knowsHIV.r3)])
pctFoundR3 <- rd1(nAllR3/nNDiscR1*100)
knowsHIV.r3 <- rd1(mean(datW$p.knowsHIV.r3, na.rm=T)*100)
learnedHIV.r3 <- nDiscR3 - nDiscR2

# pooled
nAllPld <- length(datR1$pid)
nDiscPld <- length(datW$pid[datW$p.knowsHIV.r1 == 1 |
                              datW$p.knowsHIV.r2 == 1 |
                              datW$p.knowsHIV.r3 == 1])
nNDiscPld <- length(datR3$pid[datR3$p.knowsHIV.r1==0 & 
                                datR3$p.knowsHIV.r2==0 &
                                datR3$p.knowsHIV.r3==0])

pctDiscEnd <- rd1(nDiscPld/nAllPld*100)

# Ns by combos
compSvy <- 
  datL %>%
  dplyr::select(carePresent, wave, pid) %>%
  mutate(present=ifelse(!is.na(carePresent), 1, 0)) %>%
  dplyr::select(present, wave, pid) %>%
  reshape(.,
          idvar="pid",
          timevar="wave",
          direction="wide") %>%
  group_by(pid) %>%
  mutate(nwaves=sum(present.r1, present.r2, present.r3)) %>%
  group_by(nwaves) %>%
  dplyr::summarise(n=n())


# lost to follow-up =========================================================

datW <-
  datW %>%
  mutate(compSvy.r2=ifelse(!is.na(p.knowsHIV.r2), 1, 0),
         compSvy.r3=ifelse(!is.na(p.knowsHIV.r3), 1, 0)) 


# prevalence of disclosure ==================================================
prevDiscR1 <- rd1(mean(datW$p.knowsHIV.r1, na.rm=T)*100)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# WHAT IS THE PREVALENCE OF DISCLOSURE? (Baseline)
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# number of eligible caregivers
E <- 400

# number of caregivers who completed survey  
dat <- subset(datL, wave=="r1")
N <- nrow(dat)
n.bikita <- nrow(dat[dat$district==1,])
n.zaka <- nrow(dat[dat$district==2,])

# overall prevalence of full disclosure
disFullAllN <- sum(dat$p_knowsHIV, na.rm = T)
disFullAll <- mean(dat$p_knowsHIV, na.rm = T)*100
disFullAll95CI <- prop.test(sum(dat$p_knowsHIV, na.rm=T), # 95% CI
                            N,
                            conf.level=0.95, 
                            correct = FALSE)
disFullAll95CIl <- disFullAll95CI$conf.int[1]*100
disFullAll95CIu <- disFullAll95CI$conf.int[2]*100

# prevalence by district
disFullDistrict <- aggregate(p_knowsHIV ~ district, 
                             data=dat, FUN=mean, na.rm=T)
disFullB <- disFullDistrict$p_knowsHIV[disFullDistrict$district==1]*100
disFullZ <- disFullDistrict$p_knowsHIV[disFullDistrict$district==2]*100

disFullDistrict95CI <- summarySE(data=dat,    
                                 measurevar="p_knowsHIV",  
                                 groupvars="district",    
                                 conf.interval = 0.95)
disFullB95CIl <- (disFullDistrict95CI$p_knowsHIV[
  disFullDistrict95CI$district==1]-
    disFullDistrict95CI$ci[disFullDistrict95CI$district==1])*100
disFullB95CIu <- (disFullDistrict95CI$p_knowsHIV[
  disFullDistrict95CI$district==1]+
    disFullDistrict95CI$ci[disFullDistrict95CI$district==1])*100
disFullZ95CIl <- (disFullDistrict95CI$p_knowsHIV[
  disFullDistrict95CI$district==1]-
    disFullDistrict95CI$ci[disFullDistrict95CI$district==2])*100
disFullZ95CIu <- (disFullDistrict95CI$p_knowsHIV[
  disFullDistrict95CI$district==1]+
    disFullDistrict95CI$ci[disFullDistrict95CI$district==2])*100

# prevalence by clinic
disFullClinic <- aggregate(p_knowsHIV ~ clinic, 
                           data=dat, FUN=mean, na.rm=T)
disFullClinicMin <- min(disFullClinic$p_knowsHIV)*100
disFullClinicMax <- max(disFullClinic$p_knowsHIV)*100

# prevalence by age
disFullAge <- aggregate(p_knowsHIV ~ MISC3, 
                        data=dat, FUN=mean, na.rm=T)

# predicted probabilities by age
m1 <- glm(p_knowsHIV ~ factor(MISC3), 
          data = dat, 
          family = "binomial")
exp(cbind(OR = coef(m1), confint(m1)))

newdata1 <- with(dat,
                 data.frame(MISC3 = factor(9:15)))
newdata1$ageP <- predict(m1, newdata = newdata1, type = "response")
newdata1

ppAge9 <- newdata1$ageP[newdata1$MISC3==9]
ppAge15 <- newdata1$ageP[newdata1$MISC3==15]

# full disclosure probabilities
m1 <- glm(p_fullDisclosure ~ factor(MISC3), 
          data = dat, 
          family = "binomial")
exp(cbind(OR = coef(m1), confint(m1)))

newdata1 <- with(dat,
                 data.frame(MISC3 = factor(9:15)))
newdata1$ageP <- predict(m1, newdata = newdata1, type = "response")
newdata1

fAge9 <- newdata1$ageP[newdata1$MISC3==9]
fAge15 <- newdata1$ageP[newdata1$MISC3==15]

# know method of transmission
knowsHowInfected <- ifelse(dat$doesKnow20=="yes, child knows", 1, 0)
knowsHowInfected <- prop.test(sum(knowsHowInfected, na.rm=T), # 95% CI
                              disFullAllN,
                              conf.level=0.95, 
                              correct = FALSE)
knowsHowInfectedP <- knowsHowInfected$estimate*100
knowsHowInfected95CIl <- knowsHowInfected$conf.int[1]*100
knowsHowInfected95CIu <- knowsHowInfected$conf.int[2]*100

# knows could infect others
knowsCouldInfect <- ifelse(dat$doesKnow24=="yes, child knows", 1, 0)
knowsCouldInfect <- prop.test(sum(knowsCouldInfect, na.rm=T), # 95% CI
                              disFullAllN,
                              conf.level=0.95, 
                              correct = FALSE)
knowsCouldInfectP <- knowsCouldInfect$estimate*100
knowsCouldInfect95CIl <- knowsCouldInfect$conf.int[1]*100
knowsCouldInfect95CIu <- knowsCouldInfect$conf.int[2]*100

# when child learned status
ageLearnedM <- mean(as.numeric(as.character(dat$doesKnow19)), na.rm=T)
ageLearnedSD <- sd(as.numeric(as.character(dat$doesKnow19)), na.rm=T)

# how learned
learnedFromCare <- prop.table(table(dat$doesKnow17))["From me"]*100
learnedFromHW <- prop.table(table(dat$doesKnow17))["From a health worker"]*100

# disfull
pFullBase <- rd1(mean(datW$p.fullDisclosure.r1, na.rm=T)*100)



# when and how child found out ==============================================

# doesKnow13: how found out chronic
# doesKnow17: how found out HIV
# doesKnow21: how found out how infected
# doesKnow25: how found can spread


# Table 1: demographics at wave 1 by disclosure status ========================

datW <-
  datW %>%
  mutate(h.wealthQb2.r1=ifelse(h.wealthQ.r1==1 | h.wealthQ.r1==2, 1, 0))

descTable <-
  datW %>%
  bind_rows("total" = ., "not" = ., .id="id") %>%
  #mutate(learnsHIV=ifelse(waveLearnedHIV=="r2" | waveLearnedHIV=="r3", 1, 0)) %>%
  mutate(p.knowsHIV.r1=ifelse(id=="total", "Total", p.knowsHIV.r1)) %>%
  group_by(p.knowsHIV.r1) %>%
  summarise(n=sum(!is.na(c.age.r1)),
            npct=paste0(sum(!is.na(c.age.r1)),
                        " (",
                        rd1((n/372)*100),
                        ")"),
            p.female=rd1(mean(p.female.r1, na.rm=T)*100),
            p.age=paste0(rd1(mean(p.age.r1, na.rm=T)), " (",
                         rd1(sd(p.age.r1, na.rm=T)), ")"),
            p.bio=rd1(mean(p.bio.r1, na.rm=T)*100),
            p.grandparent=rd1(mean(p.grandparent.r1, na.rm=T)*100),
            p.eduCompPri=rd1(mean(p.eduCompPri.r1, na.rm=T)*100),
            p.married=rd1(mean(p.currentMarried.r1, na.rm=T)*100),
            p.hiv=rd1(mean(p.HIVpos.r1, na.rm=T)*100),
            h.wealthQ=rd1(mean(h.wealthQb2.r1, na.rm=T)*100),
            c.female=rd1(mean(c.female.r1, na.rm=T)*100),
            c.age=paste0(rd1(mean(c.age.r1, na.rm=T)), " (",
                         rd1(sd(c.age.r1, na.rm=T)), ")"),
            c.eduCompPri=rd1(mean(c.eduCompPri.r1, na.rm=T)*100)) %>%
  select(-n) %>%
  gather(., key, value, -p.knowsHIV.r1) %>%
  data.frame(.) %>%
  reshape(.,
          idvar="key",
          timevar="p.knowsHIV.r1",
          direction="wide") %>%
  select(key, value.Total, value.1, value.0)

# add OR to descTable
descVars <-
  c("p.female.r1",
    "p.age.r1",
    "p.bio.r1",
    "p.grandparent.r1",
    "p.eduCompPri.r1",
    "p.currentMarried.r1",
    "p.HIVpos.r1",
    "h.wealthQb2.r1",
    "c.female.r1",
    "c.age.r1",
    "c.eduCompPri.r1"
  )

# univariate
r <- 2
for (v in descVars) {

  datW$var <- datW[,v]

  m <- glm(p.knowsHIV.r1 ~ var,
           family = binomial(link = "logit"),
           data=datW)

  descTable[r,5] <- paste0(rd2(exp(m$coefficients)[2]),
                           " (", rd2(exp(confint(m))[2,1]),
                           ", ", rd2(exp(confint(m))[2,2]), ")") # OR
  r <- r + 1
}


names(descTable) <- c("key", "cx", "disclosed", "nondisclosed", "uni")

# labels for the table

descTable <-
  descTable %>%
  mutate(key=ifelse(key=="npct", "\\hspace{0.25cm}N (\\%)",
             ifelse(key=="p.female", "\\hspace{0.25cm}Female (\\%)",
             ifelse(key=="p.age", "\\hspace{0.25cm}Mean Age (SD)",
             ifelse(key=="p.bio", "\\hspace{0.25cm}Biological Caregiver (\\%)",
             ifelse(key=="p.grandparent", "\\hspace{0.25cm}Grandparent Caregiver (\\%)",
             ifelse(key=="p.eduCompPri", "\\hspace{0.25cm}Completed Primary (\\%)",
             ifelse(key=="p.married", "\\hspace{0.25cm}Married (\\%)",
             ifelse(key=="p.hiv", "\\hspace{0.25cm}HIV + (\\%)",
             ifelse(key=="h.wealthQ", "\\hspace{0.25cm}Poorest 2 Wealth Quintiles (\\%)",
             ifelse(key=="c.female", "\\hspace{0.25cm}Female (\\%)",
             ifelse(key=="c.age", "\\hspace{0.25cm}Mean Age (SD)",
             ifelse(key=="c.eduCompPri", "\\hspace{0.25cm}Completed Primary (\\%)",
             "WTF")))))))))))))

care.age.r1 <- rd1(mean(datW$p.age.r1, na.rm=T))
child.age.r1 <- rd1(mean(datW$c.age.r1, na.rm=T))

# Table B.2 status at each wave ===============================================

npctDisc <-
  datL %>%
  select(pid, p_knowsChronic, p_knowsHIV,
         p_knowsHowInf, p_knowsCanSpread, wave) %>%
  melt(., id=c("pid", "wave")) %>%
  group_by(wave, variable) %>%
  filter(!is.na(value)) %>%
  mutate(resp=sum(!is.na(value))) %>%
  mutate(resp=ifelse(wave=="r1", 372,
                     ifelse(wave=="r2", 112,
                            ifelse(wave=="r3", 117, NA)))) %>%
  group_by(variable, wave) %>%
  summarise(n=as.integer(sum(value)),
            resp=mean(resp)) %>%
  mutate(pct=rd1(n/resp*100)) %>%
  data.frame(.)

for (r in 1:nrow(npctDisc)) {
  npctDisc$ci1[r] <- rd1(prop.test(npctDisc$n[r],
                                   npctDisc$resp[r],
                                   conf.level=0.95,
                                   correct=FALSE)$conf.int[1]*100)
  npctDisc$ci2[r] <- rd1(prop.test(npctDisc$n[r],
                                   npctDisc$resp[r],
                                   conf.level=0.95,
                                   correct=FALSE)$conf.int[2]*100)
  npctDisc$ci[r] <- paste0("(", npctDisc$ci1[r],
                           ", ", npctDisc$ci2[r], ")")
}

npctDisc <-
  npctDisc %>%
  reshape(.,
          idvar="variable",
          timevar="wave",
          direction="wide") %>%
  select(variable, pct.r1, ci.r1, pct.r2, ci.r2, pct.r3, ci.r3) %>%
  mutate(variable=ifelse(variable=="p_knowsChronic", "a. He/she has a medical condition requiring ongoing treatment",
                  ifelse(variable=="p_knowsHIV", "b. Medical condition is called HIV",
                  ifelse(variable=="p_knowsHowInf", "c. How he/she aquired HIV",
                  ifelse(variable=="p_knowsCanSpread", "d. He/she can spread the disease to others", "WTF")))))

npctDiscCat <-
  datL %>%
  select(pid, wave, p_disclosureStatusNoChronic) %>%
  melt(., id=c("pid", "wave")) %>%
  group_by(wave) %>%
  mutate(resp=sum(!is.na(value))) %>%
  filter(!is.na(value)) %>%
  group_by(value, wave) %>%
  summarise(n=n(),
            N=n(),
            resp=mean(resp),
            pct=rd1(n/resp*100),
            se=sqrt((as.numeric(pct)*(100-as.numeric(pct)))/n())) %>%
  data.frame(.)

for (r in 1:nrow(npctDiscCat)) {
  npctDiscCat$ci1[r] <- rd1(prop.test(npctDiscCat$n[r],
                                      npctDiscCat$resp[r],
                                      conf.level=0.95,
                                      correct=FALSE)$conf.int[1]*100)
  npctDiscCat$ci2[r] <- rd1(prop.test(npctDiscCat$n[r],
                                      npctDiscCat$resp[r],
                                      conf.level=0.95,
                                      correct=FALSE)$conf.int[2]*100)
  npctDiscCat$ci[r] <- paste0("(", npctDiscCat$ci1[r],
                              ", ", npctDiscCat$ci2[r], ")")
}

npctDiscCat <-
  npctDiscCat %>%
  reshape(.,
          idvar="value",
          timevar="wave",
          direction="wide") %>%
  select(value, pct.r1, ci.r1, pct.r2, ci.r2, pct.r3, ci.r3)


full.r3 <- npctDiscCat[1,7]
hiv.r3 <- npctDisc[2,7]





# have you assessed your child? & plans =======================================
# pdb1 - begun assessing - 1/0
# pdb2 - when=never, etc.
# pdb3 = who do you want to disclose
# pdb4 - do you have a plan 1/0
# pdb5 = likely follow-through 0-3
# pdb6 - steps? 1/0
# pdb7 - discussed w/hcw - 1/0


# Table 2. following through on plans =========================================

# i need intetions at r1 <12 months and status at r3
# pdb1 - assess?
# pdq3 - prepare?
# pdq8 - one time event?
# pdq7 - hw?
# pdq9 - plan?

# function to change all columns to numeric from factor
toNum <- function(x) {
  l <- length(x)
  for (i in 1:l) {
    if (as.factor(x[,i])==TRUE) {
      x[,i] <- as.numeric(as.character(x[,i]))
    }
  }
  return(x)
}

# ever took a step if your child was in prosp cohort
# or admitted to making a plan if they had disclosed
datW$assessChild <- 0
datW$assessChild [datW$PDB1.r1==1] <- 1 #before
datW$assessChild [datW$PDB1.r2==1] <- 1 #before
datW$assessChild [datW$PDB1.r3==1] <- 1 #before

datW$prepChildAny <- 0
datW$prepChildAny[datW$PDB6.r1==1] <- 1 #before
datW$prepChildAny[datW$PDB6.r2==1] <- 1 #before
datW$prepChildAny[datW$PDB6.r3==1] <- 1 #before
datW$prepChildAny[datW$PDQ1.r2==1] <- 1 #if knows
datW$prepChildAny[datW$PDQ1.r3==1] <- 1 #if knows

datW$talkHW <- 0
datW$talkHW[datW$PDB7.r1==1] <- 1 #before
datW$talkHW[datW$PDB7.r2==1] <- 1 #before
datW$talkHW[datW$PDB7.r3==1] <- 1 #before
datW$talkHW[datW$PDQ7.r2==1] <- 1 #if knows
datW$talkHW[datW$PDQ7.r3==1] <- 1 #if knows

datW$planAny <- 0
datW$planAny[datW$PDB4.r1==1] <- 1 #before
datW$planAny[datW$PDB4.r2==1] <- 1 #before
datW$planAny[datW$PDB4.r3==1] <- 1 #before
datW$planAny[datW$PDQ9.r2==1] <- 1 #if knows
datW$planAny[datW$PDQ9.r3==1] <- 1 #if knows

datW <-
  datW %>%
  mutate(anyOfPlans=ifelse(assessChild==1 | prepChildAny==1 |
                             talkHW==1 | planAny==1, 1, 0),
         allPlans=ifelse(assessChild==1 & prepChildAny==1 &
                           talkHW==1 & planAny==1, 1, 0),
         noPlans=ifelse(assessChild==0 & prepChildAny==0 &
                          talkHW==0 & planAny==0, 1, 0))


ft <-
  datW %>%
  filter(p.knowsHIV.r1==0) %>% # only prosp cohort
  filter(compSvy.r3==1) %>%
  select(age12, c.age.r1,
         PDB2.r1,
         p.knowsHIV.r2, p.knowsHIV.r3,
         shouldKnow3.r1, shouldKnow3.r2,
         prepChildAny, talkHW, planAny,
         assessChild,
         noPlans, anyOfPlans, allPlans) %>%
  mutate(intend12mos=ifelse(PDB2.r1 == "in the next few weeks" |
                              PDB2.r1 == "in the next few months" |
                              PDB2.r1 == "between 6 months and 1 year from now", 1, 0)) %>%
  mutate(shouldKnowHIV=ifelse(shouldKnow3.r1 == 1, 1, 0)) %>%
  toNum(.) %>% # change all to numeric
  mutate(knowsHIV=ifelse(p.knowsHIV.r2 == 1 | p.knowsHIV.r3 == 1, 1, 0)) %>%
  mutate(N=n())

ft <-
  ft %>% # replicate the df with grouping variable as total
  mutate(age12="Total",
         age12="Total",
         intend12mos="Total") %>% # total group gets called "NA"
  rbind(., ft)

mainFT <-
  ft %>%
  group_by(age12) %>%
  filter(!is.na(intend12mos)) %>% # drop 2 that didn't answer this question
  summarise(
    shouldKnowHIV=rd1(mean(shouldKnowHIV, na.rm=T)*100),
    intend12mosAvg=rd1(mean(as.numeric(intend12mos), na.rm=T)*100),
    knowsHIV12mos=rd1(mean(as.numeric(p.knowsHIV.r3), na.rm=T)*100),
    n=n()) %>%
  select(age12, n, shouldKnowHIV, intend12mosAvg)

byIntend <-
  ft %>%
  group_by(age12, intend12mos) %>%
  filter(!is.na(intend12mos)) %>% # drop 4 that didn't answer this question
  summarise(
    assessChild=rd1(mean(assessChild, na.rm=T)*100),
    prepChildAny=rd1(mean(prepChildAny, na.rm=T)*100),
    talkHW=rd1(mean(talkHW, na.rm=T)*100),
    planAny=rd1(mean(planAny, na.rm=T)*100),
    anyOfPlans=rd1(mean(anyOfPlans, na.rm=T)*100),
    allPlans=rd1(mean(allPlans, na.rm=T)*100),
    noPlans=rd1(mean(noPlans, na.rm=T)*100),
    knowsHIV=rd1(mean(knowsHIV, na.rm=T)*100),
    n=n()) %>%
  filter(intend12mos==1 | intend12mos=="Total") %>%
  select(-intend12mos, -n) %>%
  merge(mainFT, ., by="age12", all.x=TRUE)

mainFTlong <-
  ft %>%
  group_by(age12) %>%
  filter(!is.na(intend12mos)) %>% # drop 2 that didn't answer this question
  summarise(
    shouldKnowHIV=rd1(mean(shouldKnowHIV, na.rm=T)*100),
    intend12mosAvg=rd1(mean(as.numeric(intend12mos), na.rm=T)*100),
    #noIntend12mosAvg=100-as.numeric(rd1(mean(intend12mos, na.rm=T)*100)),
    n=n()) %>%
  select(age12, n, shouldKnowHIV, intend12mosAvg) %>%
  melt(., id=c("age12", "n")) %>%
  reshape(.,
          idvar=c("variable"),
          timevar=c("age12"),
          direction="wide") %>%
  mutate(variable=as.character(variable))

tellIntend <-
  ft %>%
  select(knowsHIV, intend12mos, age12) %>%
  group_by(intend12mos, age12) %>%
  summarise(n=n(),
            value=rd1(mean(knowsHIV, na.rm=T)*100)) %>%
  filter(!is.na(intend12mos)) %>%
  data.frame(.) %>%
  reshape(.,
          idvar="intend12mos",
          timevar="age12",
          direction="wide") %>%
  mutate(intend12mos=factor(intend12mos,
                            levels=c("Total",
                                     "1",
                                     "0"))) %>%
  arrange(intend12mos) %>%
  rename(., variable = intend12mos)

# add in totals
tellIntend$`value.< 12 yrs`[tellIntend$variable=="Total"] <- rd1(mean(datW$p.knowsHIV.r3[datW$age12=="< 12 yrs"], na.rm=T)*100)
tellIntend$`value.12+ yrs`[tellIntend$variable=="Total"] <- rd1(mean(datW$p.knowsHIV.r3[datW$age12=="12+ yrs"], na.rm=T)*100)
tellIntend$`value.Total`[tellIntend$variable=="1"] <- ft %>%
  filter(intend12mos=="1") %>%
  summarise(x=rd1(mean(p.knowsHIV.r3, na.rm=T)*100))
tellIntend$`value.Total`[tellIntend$variable=="0"] <- ft %>%
  filter(intend12mos=="0") %>%
  summarise(x=rd1(mean(p.knowsHIV.r3, na.rm=T)*100))

intendTold <- tellIntend$`value.Total`[tellIntend$variable=="1"][[1]]
notIntendTold <- tellIntend$`value.Total`[tellIntend$variable=="0"][[1]]

# total column for intend folks
intendOnly <-
  ft %>%
  filter(intend12mos==1) %>%
  mutate(intend12mos="Total") %>%
  mutate(age12="Total")

intendOnlyN <-
  ft %>%
  filter(intend12mos==1) %>%
  group_by(intend12mos, age12) %>%
  summarise(n=n())

byIntendDat <-
  ft %>%
  filter(!is.na(intend12mos)) %>% # drop 2 that didn't answer this question
  filter(intend12mos == 1) %>%
  rbind(., intendOnly) %>%
  group_by(age12, intend12mos) %>%
  summarise(
    assessChild=rd1(mean(assessChild, na.rm=T)*100),
    prepChildAny=rd1(mean(prepChildAny, na.rm=T)*100),
    talkHW=rd1(mean(talkHW, na.rm=T)*100),
    planAny=rd1(mean(planAny, na.rm=T)*100),
    noPlans=rd1(mean(noPlans, na.rm=T)*100),
    anyOfPlans=rd1(mean(anyOfPlans, na.rm=T)*100),
    allPlans=rd1(mean(allPlans, na.rm=T)*100),
    n=n()) %>%
  filter(intend12mos==1 | intend12mos=="Total") %>%
  select(-intend12mos)

Total <- tellIntend[1,]
amongIntend <- tellIntend[2,]

byIntendLong <-
  byIntendDat %>%
  melt(., id=c("age12", "n")) %>%
  reshape(.,
          idvar=c("variable"),
          timevar=c("age12"),
          direction="wide") %>%
  mutate(variable=as.character(variable)) %>%
  #mutate(value.Total=NA) %>%
  rbind(mainFTlong, Total, NA,
        c("Among", rep(as.character(intendOnlyN[1,3]), 2),
          rep(as.character(intendOnlyN[2,3]), 2),
          rep(as.numeric(intendOnlyN[1,3])+as.numeric(intendOnlyN[2,3]), 2)),
        amongIntend, .) %>%
  select(variable, `value.< 12 yrs`, `value.12+ yrs`, value.Total) %>%
  mutate(variable=ifelse(variable=="shouldKnowHIV", "Child should know HIV status (\\%, Wave 1)",
                  ifelse(variable=="intend12mosAvg", "Caregiver intends to tell child within 12 months (\\%, Wave 1)",
                  ifelse(variable=="Total", "Child knows HIV status at 12 months (\\%, Wave 3)",
                  ifelse(variable=="1", "\\hspace{0.25cm}Child knows HIV status at 12 months (\\%)",
                  #ifelse(variable=="0", "\\hspace{0.25cm}Among caregivers who \\textit{did not} intend to disclose (\\%)",
                  ifelse(variable=="Among", "Among caregivers who intended to disclose, N:",
                  ifelse(variable=="N", "N-intend",
                  ifelse(variable=="assessChild", "\\hspace{0.25cm}a. Assessed child's readiness (\\%)",
                  ifelse(variable=="prepChildAny", "\\hspace{0.25cm}b. Took steps to prepare child (\\%)",
                  ifelse(variable=="talkHW", "\\hspace{0.25cm}c. Consulted health care worker (\\%)",
                  ifelse(variable=="planAny", "\\hspace{0.25cm}d. Made a plan to disclose (\\%)",
                  ifelse(variable=="anyOfPlans", "\\hspace{0.25cm}Any of a-d (\\%)",
                  ifelse(variable=="allPlans", "\\hspace{0.25cm}All of a-d (\\%)",
                  ifelse(variable=="noPlans", "\\hspace{0.25cm}None of a-d (\\%)",
                  ifelse(variable=="knowsHIV", "Child knows HIV at 12 months (\\%)", "WTF"))))))))))))))) %>%
  mutate(`value.Total`=ifelse(`value.Total`=="NaN", rd1(mean(as.numeric(ft$intend12mos[ft$intend12mos!="Total"]), na.rm=T)*100), `value.Total`))

intendN <- data.frame(variable="N",
                      a=byIntend[1,2],
                      b=byIntend[2,2],
                      c=byIntend[1,2]+byIntend[2,2])

names(intendN) <- names(byIntendLong)

byIntendLong <- rbind(intendN, byIntendLong)

intends <- byIntendLong[3,4]
ft.lt12 <- byIntendLong[5,2]

age.began.prep.m <- rd1(mean(datL$PDQ5[datL$wave=="r1"], na.rm=T))
age.began.prep.sd <- rd1(sd(datL$PDQ5[datL$wave=="r1"], na.rm=T))



# objects used in the who discloses section  ----------------------------------

disc.cs <-
  datL %>%
  filter(wave=="r1") %>%
  mutate(before=ifelse(MISC21=="no, before", 1, 0),
         after=ifelse(MISC21=="no, after", 1, 0),
         sameDay=ifelse(MISC21=="yes, same day", 1, 0))

all.discR1.n <- sum(datW$p.knowsHIV.r1, na.rm=T)
before.cs <- rd1(mean(disc.cs$before, na.rm=T)*100)
before.cs.n <- sum(disc.cs$before, na.rm=T)

after.cs <- rd1(mean(disc.cs$after, na.rm=T)*100)
after.cs.n <- sum(disc.cs$after, na.rm=T)

sameDay.cs <- rd1(mean(disc.cs$sameDay, na.rm=T)*100)
sameDay.cs.n <- sum(disc.cs$sameDay, na.rm=T)

whoTable <- table(datW$doesKnow17.r1, datW$MISC21.r1)
how.cg.before.cs.n <- whoTable["From me", "no, before"]
how.cg.before.cs <- rd1(how.cg.before.cs.n/sum(whoTable[,"no, before"])*100)
how.cg.before.cs.NOT.self.n <- sum(whoTable[-1,"no, before"])


same.day.cs.hw <-
  datL %>%
  filter(wave=="r1") %>%
  filter(MISC21=="yes, same day") %>%
  filter(doesKnow17=="From a health worker") %>%
  mutate(PDQ13=as.numeric(as.character(PDQ13))) %>%
  mutate(doesKnow18=as.numeric(as.character(doesKnow18))) %>%
  mutate(svgood=ifelse(PDQ13==2 | PDQ13==3, 1, 0))

same.day.cs.hw.n <- sum(same.day.cs.hw$p_knowsHIV, na.rm=T)  

same.day.cs.hw.sat <- rd0(mean(same.day.cs.hw$doesKnow18, na.rm=T)*100)
same.day.cs.hw.sat.n <- sum(same.day.cs.hw$doesKnow18, na.rm=T)

same.day.cs.hw.svgood <- rd1(mean(same.day.cs.hw$svgood, na.rm=T)*100)
same.day.cs.hw.svgood.n <- sum(same.day.cs.hw$svgood, na.rm=T)


before.cs.self <-
  datL %>%
  filter(wave=="r1") %>%
  filter(MISC21=="no, before") %>%
  filter(doesKnow17=="From me") %>%
  mutate(PDQ1=as.numeric(as.character(PDQ1)),
         PDQ6=as.numeric(as.character(PDQ6)),
         PDQ7=as.numeric(as.character(PDQ7)),
         PDQ8=ifelse(PDQ8=="unfolded over time", 1, 0),
         PDQ10=ifelse(as.numeric(as.character(PDQ10))==0, 1, 0),
         PDQ12=ifelse(as.numeric(as.character(PDQ12))>=2, 1, 0),
         PDQ13=ifelse(as.numeric(as.character(PDQ13))>=2, 1, 0))

before.cs.self.prepare <- rd1(mean(before.cs.self$PDQ1, na.rm=T)*100)
before.cs.self.prepare.n <- sum(before.cs.self$PDQ1, na.rm=T)

before.cs.self.seek.hw <- rd1(mean(before.cs.self$PDQ7, na.rm=T)*100)
before.cs.self.seek.hw.n <- sum(before.cs.self$PDQ7, na.rm=T)

before.cs.self.seek.ff <- rd1(mean(before.cs.self$PDQ6, na.rm=T)*100)
before.cs.self.seek.ff.n <- sum(before.cs.self$PDQ6, na.rm=T)

before.cs.self.process <- rd1(mean(before.cs.self$PDQ8, na.rm=T)*100)
before.cs.self.process.n <- sum(before.cs.self$PDQ8, na.rm=T)

before.cs.self.noDay <- rd1(mean(before.cs.self$PDQ10, na.rm=T)*100)
before.cs.self.noDay.n <- sum(before.cs.self$PDQ10, na.rm=T)

before.cs.self.process.svgood.cg <- rd1(mean(before.cs.self$PDQ12, na.rm=T)*100)
before.cs.self.process.svgood.cg.n <- sum(before.cs.self$PDQ12, na.rm=T)

before.cs.self.process.svgood.child <- rd1(mean(before.cs.self$PDQ13, na.rm=T)*100)
before.cs.self.process.svgood.child.n <- sum(before.cs.self$PDQ13, na.rm=T)

before.cs.NOT.self <-
  datL %>%
  filter(wave=="r1") %>%
  filter(MISC21=="no, before") %>%
  filter(doesKnow17!="From me") %>%
  mutate(PDQ1=as.numeric(as.character(PDQ1)),
         PDQ6=as.numeric(as.character(PDQ6)),
         PDQ7=as.numeric(as.character(PDQ7)),
         PDQ8=ifelse(PDQ8=="unfolded over time", 1, 0),
         PDQ10=ifelse(as.numeric(as.character(PDQ10))==0, 1, 0),
         PDQ12=ifelse(as.numeric(as.character(PDQ12))>=2, 1, 0),
         PDQ13=ifelse(as.numeric(as.character(PDQ13))>=2, 1, 0),
         doesKnow18=as.numeric(as.character(doesKnow18)),
         hw=ifelse(doesKnow17=="From a health worker", 1, 0),
         other=ifelse(doesKnow17!="From a health worker", 1, 0))

before.cs.NOT.self.sat <- rd1(mean(before.cs.NOT.self$doesKnow18, na.rm=T)*100)
before.cs.NOT.self.sat.n <- sum(before.cs.NOT.self$doesKnow18, na.rm=T)

before.cs.NOT.self.process.svgood.cg <- rd1(mean(before.cs.NOT.self$PDQ12, na.rm=T)*100)
before.cs.NOT.self.process.svgood.cg.n <- sum(before.cs.NOT.self$PDQ12, na.rm=T)

before.cs.NOT.self.process.svgood.child <- rd1(mean(before.cs.NOT.self$PDQ13, na.rm=T)*100)
before.cs.NOT.self.process.svgood.child.n <- sum(before.cs.NOT.self$PDQ13, na.rm=T)

before.cs.NOT.self.hw <- rd1(mean(before.cs.NOT.self$hw, na.rm=T)*100)
before.cs.NOT.self.hw.n <- sum(before.cs.NOT.self$hw, na.rm=T)

before.cs.NOT.self.other <- rd1(mean(before.cs.NOT.self$other, na.rm=T)*100)
before.cs.NOT.self.other.n <- sum(before.cs.NOT.self$other, na.rm=T)



# Table B.3 among non-d at wave 3, worries, etc. ==============================

valence <- c("+", "-")

risks <-
  datW %>%
  filter(p.knowsHIV.r3==0) %>%
  select(c.age.r1, starts_with("attDR")) %>%
  select(c.age.r1, ends_with("r3")) %>%
  melt(., id="c.age.r1") %>%
  mutate(d=ifelse(as.numeric(value)>=2, 1, 0)) %>%
  mutate(variable=substr(as.character(variable), 1, nchar(as.character(variable))-3)) %>%
  group_by(variable) %>%
  summarize(svworry=rd1(mean(as.numeric(d), na.rm=T)*100),
            n=n()) %>%
  left_join(., qLookUp, by=c("variable" = "vars")) %>%
  arrange(., desc(svworry)) %>%
  select(english, svworry) %>%
  data.frame(.) %>%
  mutate(label=str_split(as.character(english), "will", simplify=TRUE)[,2]) %>%
  mutate(label=paste0("- ", label)) %>%
  select(label, svworry)

mean.age.nd.r3 <- rd1(mean(datW$c.age.r1[datW$p.knowsHIV.r3==0], na.rm=T))
mean.age.nd.r3 <- as.numeric(mean.age.nd.r3)+1

# Table B.1 demographics of caregivers ========================================

datW$rel <- ifelse(!is.na(datW$MISC5.r1),
                   as.character(datW$MISC5.r1),
                   as.character(datW$replaceRel.r1))

rel <-
  datW %>%
  select(rel) %>%
  mutate(rel=ifelse(rel=="adopted_foster_step", "Adopted, foster, or stepparent",
                    ifelse(rel=="foster parent", "Adopted, foster, or stepparent",
                           ifelse(rel=="step parent", "Adopted, foster, or stepparent", rel)))) %>%
  mutate(rel=ifelse(rel=="sibling", "Sibling",
                    ifelse(rel=="brother or sister", "Sibling", rel))) %>%
  mutate(rel=ifelse(rel=="other", "Other", rel)) %>%
  mutate(rel=ifelse(rel=="other relative", "Other relative", rel)) %>%
  mutate(rel=ifelse(rel=="grandparent", "Grandparent",
                    ifelse(rel=="grandchild", "Grandparent", rel))) %>%
  mutate(rel=ifelse(rel=="biological parent", "Biological parent",
                    ifelse(rel=="son or daughter (biological)", "Biological parent", rel))) %>%
  mutate(rel=ifelse(rel=="aunt or uncle", "Aunt or uncle",
                    ifelse(rel=="son or daughter (in law)", "Son or daughter (in law)", rel))) %>%
  group_by(rel) %>%
  summarise(n=n()) %>%
  ungroup(.) %>%
  mutate(N=sum(n),
         pct=rd1(n/N*100)) %>%
  mutate(pct=as.numeric(pct)) %>%
  select(-N) %>%
  arrange(-pct)


# when learned x rel which is recoded relationship to child
same.day.cs.bio <- table(datW$MISC21.r1, datW$p.bio.r1)["yes, same day","1"]

# average age of kids who learned on the same day as caregiver
same.day.cs.child.age.mean <- rd1(mean(datW$c.age.r1[datW$MISC21.r1=="yes, same day"], na.rm=T))


# what percentage of kids are on pre-ART vs. ART ==============================
artCsv <- read.csv("public/resources/BZclinics.csv")

artPctSelected <-
  artCsv %>%
  filter(selected == 1) %>%
  summarise(art.915=sum(art.915, na.rm=T),
            art.1014=sum(art.1014, na.rm=T),
            all.915=sum(all.915, na.rm=T),
            preart.915=sum(preart.915, na.rm=T),
            pctPre=preart.915/all.915*100)

artPctEligible <-
  artCsv %>%
  filter(eligible == 1) %>%
  summarise(art.915=sum(art.915, na.rm=T),
            art.1014=sum(art.1014, na.rm=T),
            all.915=sum(all.915, na.rm=T),
            preart.915=sum(preart.915, na.rm=T),
            pctPre=preart.915/all.915*100)

preArt.eligible <- artPctEligible$all.915
preArt.eligible.pct <- rd1(artPctEligible$pctPre)
art.eligible.pct <- 100-as.numeric(preArt.eligible.pct)

exluded13 <- sum(artCsv$all.915[artCsv$eligible==1 & artCsv$selected==0], na.rm=T)

# create figures ==============================================================
source("public/reports/plos2019/panel-descriptive-figures-plos2019.R")  

# create tables ===============================================================
source("public/reports/plos2019/panel-descriptive-tables-plos2019.R")
