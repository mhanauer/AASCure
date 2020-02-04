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
```{r}
library(NPHMC)
test = NPHMC(power = .8, alpha = .05/3, accrualtime = 1, followuptime = 1, p = .5, accrualdist = "uniform", hazardratio = .3, oddsratio = .1, pi0 = .9, survdist = "exp", lambda0 = .5)
test

```
Try example from smcure
```{r}
library(smcure)
data(e1684)
data(bmt)
bmtfit <- smcure(Surv(Time,Status)~TRT,cureform=~TRT,
data=bmt,model="aft",nboot=200)
```

