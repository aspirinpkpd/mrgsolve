rm(list = ls())
#library(devtools)
#install.packages("mrgsolve",repo="https://mpn.metworx.com/snapshots/stable/2022-06-15")
library(mrgsolve)
library(tidyverse)
library(dplyr)

#location of default mrgsolve model library
modlib()

modlib(list=TRUE)


#read a model from the default mrgsolve model library 
mod <- mread("pk2",    modlib())

#lets see the parameters in the model
mrgsolve:::code(mod)

see(mod)
#let's run a simple simulation with 1 dose

mod %>% ev(amt = 100) %>% mrgsim()
simu<-mod %>% ev(amt = 100) %>% mrgsim()%>%as_tibble()
#plotting amounts and concentrations

mod %>% ev(amt = 100) %>% mrgsim()%>% plot()

mod %>% ev(amt = 100) %>% mrgsim()%>% plot(CP~time)
mod %>% ev(amt = 100) %>% mrgsim(end=240)%>% plot(CP~time)

mod %>% ev(amt = 100,ii=12,addl=15,CL=3) %>% mrgsim(end=240)%>% plot(CP~time)
#having more than 1 ID



mod %>% ev(amt = 100, ID=1:10) %>% mrgsim()%>% plot(CP~time)

d<-mod %>% ev(amt = 100, ID=1:10) %>% mrgsim()%>%as_tibble()


mrgsolve:::reserved()
param(mod)
omat(mod)

# let's pick another model with omega values

modpop <- mread("popex",    modlib())
see(modpop)
param(modpop)
omat(modpop)

modpop %>% ev(amt = 100) %>% mrgsim()%>% plot(CP~time)

#Why it didnt work?

modpop %>% ev(amt = 100) %>% mrgsim()%>% plot(DV~time)

modpop %>% ev(amt = 100,ID=1:10) %>% mrgsim()%>% plot(DV~time)

#let's build on ev options
##########################
###     tgrid   ##########

peak <- tgrid(0,6,0.2)
peak

sparse <- tgrid(0,24,4)

day1 <- c(peak,sparse)

design <- c(day1, day1+24, day1+48)

modpop %>% ev(amt=1000, ii=24, addl=10) %>% mrgsim(tgrid=design)%>% plot(DV~time)

modpop %>% ev(amt=1000, ii=24, addl=10,ID=1:25) %>% mrgsim(tgrid=design)%>% plot(DV~time)

out<-modpop %>% ev(amt=1000, ii=24, addl=10,ID=1:25) %>% mrgsim(tgrid=design)%>% as_tibble()


#####YOUR TURN########

#Simulate 25 subjects after single dose for 240 hrs

#simulate 25 subjects QD until steady-state has been achieved

#simulate 25 subjects at steady state fr a 24 hrs period

#simulate typical subject at steady state and  after single dose

#####Answers########

#Simulate 25 subjects after single dose for 300 hrs
see(modpop)

modpop %>% ev(amt=1000, ii=0, addl=0,ID=1:25) %>% mrgsim(end=300,delta=2)%>% 
  plot(DV~time)

#simulate 25 subjects QD until steady-state has been achieved

design2<-tgrid(0,1000,2)
modpop %>% ev(amt=1000, ii=24, addl=25,ID=1:25) %>% mrgsim(tgrid=design2)%>% 
  plot(DV~time)

#simulate 25 subjects at steady state fr a 24 hrs period
#day 14
modpop %>% ev(amt=1000, ii=24, ss=1,ID=1:25) %>% mrgsim(end=24,delta=2)%>% 
  plot(DV~time)

test<-modpop %>% ev(amt=1000, ii=24, ss=1,ID=1:25) %>% mrgsim(end=24,delta=2)%>% 
  as_tibble()%>%filter(GUT!=0)
modpop %>% ev(amt=1000, ii=24, ss=1,ID=1:25) %>% mrgsim(end=24,delta=2)%>%plot(DV~time)

#simulate 1 subject at steady state and one after single dose

data.single<-modpop %>% ev(amt=1000, ii=0, addl=0) %>% mrgsim(end=24,delta=2)%>%
  as_tibble()%>%mutate(ARM="SAD")
data.ss<-modpop %>% ev(amt=1000, ii=24, ss=1) %>% mrgsim(end=24,delta=2)%>% 
  as_tibble()%>%mutate(ARM="MAD")%>%filter(DV!=0)

data.all<-rbind(data.single,data.ss)

plot<-ggplot(data.all,aes(x=time,y=DV,group=ARM, colour=as.factor(ARM)))+
      geom_line()
plot

#are those the typical subjects?

#zero_re(): Sets all elements of the OMEGA or SIGMA matrix to zero


modpop.zero<-zero_re(modpop)

param(modpop.zero)
omat(modpop.zero)

data.single.zero<-modpop.zero %>% ev(amt=1000, ii=0, addl=0) %>% 
  mrgsim(end=24,delta=2)%>%as_tibble()%>%mutate(ARM="SAD")
data.ss.zero<-modpop.zero %>% ev(amt=1000, ii=24, ss=1) %>% 
  mrgsim(end=24,delta=2)%>% as_tibble()%>%mutate(ARM="MAD")%>%filter(DV!=0)

data.all.zero<-rbind(data.single.zero,data.ss.zero)

plot.zero<-ggplot(data.all.zero,aes(x=time,y=DV,group=ARM, colour=as.factor(ARM)))+
  geom_line()

#schedule a sequence of events

e1 <- ev(amt = 100, ii = 12, addl = 1)

e2 <- ev(amt = 200)



#####seq() and ev+seq()
#Use this function when you want to schedule two 
#or more event objects in time according the dosing interval (ii) and additional doses (addl)

seq(e1,e2)
#If between the second 100 mg dose and the 200 mg dose we want 8 hrs instead of 12

seq(e1, ii = 8, e2)

#If between the second 100 mg dose and the 200 mg dose we want 8 additional hours (24+8==> 32) hrs instead of 12

seq(e1, wait=8,e2)

seq(e1, wait=8,e2, ID=seq(10))


ev_seq(ii = 12, e1, ii = 120, e2, ii = 120, e1) # a waiting period is the first event==> ev_seq
seq(ii = 12, e1, ii = 120, e2, ii = 120, e1) 

seq(ev(amt = 100, ii = 12), ev(time = 8, amt = 200))

####### ev_rep() #############################
#An event sequence can be replicated a certain number of times in a certain number of IDs.
ev_rep(e1,1:5)


#expand all combinations of arguments using expand.grid()

data<-expand.ev(amt=c(100,300,1000),ii=24,addl=3)

data

#as_data_set(): Combine event objects into a single data set
data2 <- as_data_set(
  ev(amt = 100, ii = 12, addl = 19, ID = 1:2),
  ev(amt = 200, ii = 24, addl = 9,  ID = 1:3),
  ev(amt = 150, ii = 24, addl = 9,  ID = 1:4)
)

data2

tabbla<-modpop.zero %>% ev(seq(ev(amt = 100, ii = 12), ev(time = 8, amt = 200))) %>% 
  mrgsim(end=48,delta=2)%>%as_tibble()


param(modpop.zero)
omat(modpop.zero)

pop<-expand.idata(CL=seq(0.8,1.2,0.05),V=seq(20,24,0.5),KA=seq(0.4,0.6,0.1),WT=seq(60,90,5))

head(pop)
modpop.zero %>% idata_set(pop)%>% ev(amt=100,ii=24,addl=2)%>%mrgsim(end=120)%>%
  plot(DV~., logy=TRUE)


t<-modpop.zero %>% idata_set(pop)
data <- expand.ev(amt = c(100, 300, 1000), ii = 24, addl = 3)

head(data)


modpop.zero %>%
  data_set(data) %>%
  mrgsim(end = 120) %>% plot(log(DV) ~ .)

#data_set can carry parameters
data <- expand.ev(
  amt = c(100,300,1000),
  ii = 24, addl = 3,
  CL = seq(0.5,1.55, 0.5)
)

head(data)

data <- mutate(data, dose = amt)

mod %>%
  data_set(data) %>%
  mrgsim(carry_out = "dose", end = 120) %>% 
  plot(log(CP)~time|factor(dose), group = ID, scales = "same")



mod <- mread("popex", modlib()) %>% zero_re()

see(mod)
param(mod)

data <- expand.ev(amt = c(100,150), WT = seq(40,140,20)) %>% mutate(dose = amt)

head(data)

modpop.zero %>% 
  data_set(data) %>% 
  carry_out(dose,WT) %>%
  mrgsim(delta = 0.1, end = 72) %>% 
  plot(DV~time|factor(dose),scales = "same")


see(mod)

#open exercises/
#meropenem.R
#sensitivity.R
#sensi_pbpk.R
