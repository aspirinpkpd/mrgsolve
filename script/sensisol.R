rm(list = ls())

library(dplyr)
library(mrgsolve)


#' Load the model called "pk2" (2-cmt pk) from the internal library
#' 
#' Construct a simulation that shows how time to steady state depends
#' on volume of distribution (V2); look at 10, 50 and 100 L
#' while dosing 100 mg every day for a month
#' 


mod <- mread("pk2",  modlib())

see(mod)
#creating a dataset with several values of V2 using expand.idata

idata<-expand.idata(V2=c(10,20,50,100))

mod%>%ev(amt=100,ii=24,addl=29)%>%idata_set(idata)%>%mrgsim(end=30*24,delta=1)%>%plot(CP~time)

