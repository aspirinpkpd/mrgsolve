
rm(list = ls())
library(dplyr)
library(mrgsolve)


#' Here is a PBPK model for DDI between a statin and CsA
#' 
moddir<-"/cloud/project/model"

mod <- mread("yoshikado", moddir, delta=0.1, end = 12) 


##see publication under documents

see(mod)

param(mod)


#' A single CsA dose
csa <- ev(amt = 2000, cmt = 2)

#' A single pitavastatin dose 0.5 hours after CsA
pit <- ev(amt = 30, cmt = 1, time = 0.5)

#' The ddi dosing intervention
ddi <- seq(csa, wait = 0.5, pit)


#' Find the ikiu parameter value
#' Generate a sensitivity analysis on this parameter, 
#' varying with uniform distribution between 0.1 and 5 times
#' the nominal value; do this with an idata set
#' 
#' - Make a plot
#' - Summarize the variability in Cmax
#' - Summarize the variability in AUC


#ikiu value 0.0118
idata <- expand.idata(ikiu = seq(0.001,0.5,0.05))

idata


mod %>% 
  ev(ddi) %>% 
  idata_set(idata) %>%
  mrgsim(end = 24, delta = 0.1) %>%
  plot(CP~time)

