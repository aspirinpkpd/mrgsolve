library(dplyr)
library(mrgsolve)

#' - Load the `meropenem_pk` model from the model directory
#' - Simulate the following scenarios:
#'   - 500 mg IV bolus q8h x 3
#'   - 1000 mg IV over 3 hours q8h x3

#' Look at the `CP` output
#' 

moddir<-"/cloud/project/model"

mod <- mread("meropenem_pk",moddir)

see(mod) 

param(mod)



mod%>%ev(amt=500,ii=8,addl=2)%>%mrgsim()%>%plot(CC~time)

mod%>%ev(amt=1000,rate=1000/3,ii=8,addl=2)%>%mrgsim()%>%plot(CC~time)

mod%>%ev(amt=1000,tinf=3,ii=8,addl=2)%>%mrgsim()%>%plot(CC~time)

#create a uniform distribution of age (idata)

tlook <- 8

idata <- tibble(AGE = runif(100, mod$AGE/2, mod$AGE*2))

mod %>% 
  idata_set(idata) %>% 
  ev(amt = 1000, tinf = 3,ii=8,addl=2) %>% 
  mrgsim() %>% plot(CC~time)

#select ctrough (time=8)

mod %>% 
  idata_set(idata) %>% 
  ev(amt = 1000, tinf = 3,ii=8,addl=2) %>% 
  mrgsim() %>% 
  filter(time==tlook)%>%
  dplyr::summarise(mean=mean(Y), sd = sd(Y))


#     mean    sd
#  1  1.54 0.607


#create a uniform distribution of CRCL (idata)
idata <- tibble(CLCR = runif(100,mod$CLCR/2,mod$CLCR*2))

mod %>% 
  idata_set(idata) %>% 
  ev(amt = 1000,tinf = 3,ii=8,addl=2) %>% 
  mrgsim() %>% plot(CC~time)


mod %>% 
  idata_set(idata) %>% 
  ev(amt = 1000, tinf = 3,ii=8,addl=2) %>% 
  mrgsim() %>% 
  filter(time==tlook) %>% 
  summarise(mean=mean(Y),sd = sd(Y))

#mean        sd
#1 1.184216 0.8349741

idata <- tibble(WT = runif(100,mod$WT/2,mod$WT*2))

mod %>% 
  idata_set(idata) %>% 
  ev(amt = 1000, tinf = 3,ii=8,addl=2) %>% 
  mrgsim() %>% plot(CC~time)



mod %>% 
  idata_set(idata) %>% 
  ev(amt = 1000, tinf = 3,ii=8,addl=2) %>% 
  mrgsim() %>% 
  filter(time==tlook) %>% 
  summarise(mean=mean(Y),sd = sd(Y))

#mean        sd
#1 1.401753 0.3277516
