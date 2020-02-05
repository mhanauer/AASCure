# AASCure
---
title: "SurvivalPower"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Try the smcure model.  Including the probabiltiy of not dying, which is relatively common for those who are at risk of suicide.  This is not a fatal disease.  

25% for lost to follow-up and 50% because we are going to have an extra group and alpha /3, because you are running three non-independent tests

Accrualtime: The time you spend recruiting participants
Followuptime: The time you spend following up after the study has enrolled their last participant

.4 is rate of cure for treatment .2 is rate of cure for control
Cure rate = (.4/(1-.4))/(.2/(1-.2))
x = treatment cure rate; y = control cure rate

Hazard ratio cure: The difference in mean survival times for those uncured (i.e. those who died double check this) 

```{r}
library(NPHMC)
data(e1684szdata)

odds_ratio_cure  = function(x,y){
  (x/(1-x))/(y/(1-y))
}
odds_ratio_cure(.9,.8)
test_power = NPHMC(power = .8, alpha = .05, accrualtime = 1, followuptime = 3, p = .5, accrualdist = "uniform", hazardratio = 2/2.5, oddsratio = odds_ratio_cure(.4,.2), pi0 = .9, survdist = "exp", lambda0 = .5)
test_power
```
Try example from smcure
https://stats.idre.ucla.edu/stata/seminars/stata-survival/
Hazzard rate: Unobserved rate at time t at which events will occur for those who the event has not already occured.  Measured in the units of time that you have in the data.  For example a hazzard rate of 1.5 when the unit of time measurement is weeks is 1.5 event within one week.   
Relative risk: percentage change in the risk of death (deaths / people at risk)
Odds ratio: percentage change in percentage of peoeple dying versus not dying (deaths / not deaths)
Hazard ratios: Rate of change (p-change)

```{r}
library(smcure)
data(e1684)
data(bmt)
bmtfit <- smcure(Surv(Time,Status)~TRT,cureform=~TRT,
data=bmt,model="aft",nboot=200)
bmtfit

```

