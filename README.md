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

Accrualtime: The time you spend recruiting participants usually in years
Followuptime: The time you spend following up after the study has enrolled their last participant usually in years

.4 is rate of cure for treatment .2 is rate of cure for control
Cure rate = (.4/(1-.4))/(.2/(1-.2))
x = treatment cure rate; y = control cure rate

Hazard ratio cure: The difference in mean survival times for those uncured (i.e. those who died double check this) 

```{r}
library(NPHMC)

odds_ratio_cure  = function(x,y){
  (x/(1-x))/(y/(1-y))
}
odds_ratio_cure(.9,.8)
test_power = NPHMC(power = .8, alpha = .05, accrualtime = 1, followuptime = 3, p = .5, accrualdist = "uniform", hazardratio = 2/2.5, oddsratio = 4, pi0 = .9, survdist = "exp", lambda0 = .5)
test_power
```
Try example from smcure
https://stats.idre.ucla.edu/stata/seminars/stata-survival/
Hazzard rate: Unobserved rate at time t at which events will occur for those who the event has not already occured.  Measured in the units of time that you have in the data.  For example a hazzard rate of 1.5 when the unit of time measurement is weeks is 1.5 event within one week.   
Relative risk: percentage change in the risk of death (deaths / people at risk)
Odds ratio: percentage change in odds of peoeple dying versus not dying (deaths / not deaths)
Hazard ratios: Rate of change (p-change)

```{r}
library(smcure)
data(e1684)
library(survminer)
data(bmt)

bmtfit <- smcure(Surv(Time,Status)~TRT,cureform=~TRT,
data=bmt,model="aft",nboot=200)
bmtfit
predbmt=predictsmcure(bmtfit,newX=c(0,1),newZ=c(0,1),model='aft')
plotpredictsmcure(predbmt,model='aft', xlab = "Days")

compmeans(bmt$Time, bmt$TRT)
surv_path = Surv(bmt$Time,bmt$Status)
fit_surv = fit_surv = survfit(surv_path ~ TRT, data = bmt)
fit_surv
ggsurvplot(fit_surv, data = bmt, surv.scale = "percent", title = "Generic Survival Analysis", xlab = "Days")


```
Run mulitple power analyses for n
```{r}

n = seq(from =  200, to= 500, by = 20)

n_results_n = list()
odds_ratio_cure  = function(x,y){
  (x/(1-x))/(y/(1-y))
}
odds_ratio_cure(.9,.8)

for(i in 1:length(n)){
  n_results_n[[i]] = NPHMC(n = n[[i]], alpha = .05, accrualtime = 1, followuptime = 3, p = .5, accrualdist = "uniform", hazardratio = 2/2.5, oddsratio = odds_ratio_cure(.9,.75), pi0 = .9, survdist = "exp", lambda0 = .5)
  n_results_n
}

n_results_n= unlist(n_results_n)
n_results_n = matrix(n_results_n, ncol = 2, byrow = TRUE)
colnames(n_results_n) = c("cure", "standard")
n_results_n = data.frame(n_results_n)
n_results_n$n = n
library(reshape2)
n_results_n = melt(n_results_n, measure.vars = c("cure", "standard"))
colnames(n_results_n) = c("n", "model", "power") 
n_results_n
```
Now plot the graphs
```{r}
library(ggplot2)
ggplot(n_results_n, aes(x = n, y = power, colour = model))+
  geom_line()+
  labs(title = "Figure 1: Power: Cure vs.Standard: n = 200 to 500", subtitle = "Assumptions: 15% less deaths and 20% decrease in risk of dying")+
  geom_hline(yintercept = .8)

```
Now run sequence for hazard ratio
```{r}
hazard_ratio = seq(from =  .6, to= .9, by = .01)
n_results_hr = list()
odds_ratio_cure  = function(x,y){
  (x/(1-x))/(y/(1-y))
}

for(i in 1:length(hazard_ratio)){
  n_results_hr[[i]] = NPHMC(n = 200, alpha = .05, accrualtime = 1, followuptime = 3, p = .5, accrualdist = "uniform", hazardratio = hazard_ratio[[i]], oddsratio = odds_ratio_cure(.9,.8), pi0 = .9, survdist = "exp", lambda0 = .5)
  n_results_hr
}

n_results_hr= unlist(n_results_hr)
n_results_hr = matrix(n_results_hr, ncol = 2, byrow = TRUE)
colnames(n_results_hr) = c("cure", "standard")
n_results_hr = data.frame(n_results_hr)
n_results_hr$hazard_ratio = hazard_ratio
n_results_hr
library(reshape2)
n_results_hr = melt(n_results_hr, measure.vars = c("cure", "standard"))
colnames(n_results_hr) = c("hazard_ratio", "model", "power") 
n_results_hr


```
Now plot the graphs for hazard
Risk of dying goes down
```{r}
library(ggplot2)
ggplot(n_results_hr, aes(x = hazard_ratio, y = power, colour = model))+
  geom_line()+
  labs(title = "Figure 2: Power Cure vs.Standard: 40% to 10% decrease in risk of dying", subtitle = "Assumptions: n = 200; 15% less deaths", x = "hazard ratio")+
  geom_hline(yintercept = .8)


```
Now run sequence for odds ratio
```{r}

odds_ratio = seq(from =  2.25, to= 3, by = .05)
n_results_or = list()

#odds_ratio_cure  = function(x,y){(x/(1-x))/(y/(1-y))}

for(i in 1:length(odds_ratio)){
  n_results_or[[i]] = NPHMC(n = 300, alpha = .05, accrualtime = 1, followuptime = 3, p = .5, accrualdist = "uniform", hazardratio = 2/2.5, oddsratio = odds_ratio[[i]], pi0 = .9, survdist = "exp", lambda0 = .5)
  n_results_or
}

n_results_or= unlist(n_results_or)
n_results_or = matrix(n_results_or, ncol = 2, byrow = TRUE)
colnames(n_results_or) = c("cure", "standard")
n_results_or = data.frame(n_results_or)
n_results_or$odds_ratio = odds_ratio
n_results_or
library(reshape2)
n_results_or = melt(n_results_or, measure.vars = c("cure", "standard"))
colnames(n_results_or) = c("odds_ratio", "model", "power") 
n_results_or
```
Now plot the graphs for hazard
Risk of dying goes down
As the difference between those who are dying (i.e. cured) between treatment and control we get more power from the cure model.
```{r}
library(ggplot2)
ggplot(n_results_or, aes(x = odds_ratio, y = power, colour = model))+
  geom_line()+
  labs(title = "Figure 3: Power Cure vs.Standard: Changes in odds of dying", subtitle = "Assumptions: n = 300; 10% decrease in risk of dying", x = "odds ratio")+
  geom_hline(yintercept = .8)

```

