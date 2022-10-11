
#install.packages("pmtables",repo="https://mpn.metworx.com/snapshots/stable/2022-06-15")

#install.packages("pmforest",repo="https://mpn.metworx.com/snapshots/stable/2022-06-15")

rm(list=ls())
library(mrgsolve)
library(tidyverse)
library(pmtables)
library(pmforest)

suppressPackageStartupMessages({
  library(PKNCA)
  library(dplyr)
  library(cowplot)
  library(knitr)
})

modlib<-"/cloud/project/script"

mod.closed<-mread("1cmptclosed",modlib)
param(mod.closed)


mod.closed%>%ev(amt=100,ii=24,addl=29)%>%mrgsim(end=30*24,delta=1)%>%plot(CP~time)

mod.ode<-mread("1cmptode",modlib)
param(mod.ode)

mod.ode%>%ev(amt=100,ii=24,addl=29)%>%mrgsim(end=30*24,delta=1)%>%plot(CP~time)


#2 cmt

mod2.closed<-mread("2cmptclosed",modlib)
param(mod2.closed)


mod2.closed%>%ev(amt=100,ii=24,addl=29)%>%mrgsim(end=30*24,delta=1)%>%plot(CP~time)

mod2.ode<-mread("2cmptode",modlib)
param(mod.ode)

mod2.ode%>%ev(amt=100,ii=24,addl=29)%>%mrgsim(end=30*24,delta=1)%>%plot(CP~time)



#bio
mod<-mread("2cmptodebio",modlib)
param(mod)
omat(mod)


mod%>%ev(amt=100,FORM=1)%>%mrgsim%>%plot(CP~time)

pop<-expand.idata(CL=runif(10,min=17,max=24),V2=runif(10,min=26,max=38),
                  KA=runif(10,min=0.035,max=0.079),Q=1.5,V3=120)



sample<-sample(c(1:1000),20)


dose <- as_data_set(
  ev(amt = 100000, FORM=0,ID=1:10,seq=1,dose=100000),
  ev(time=500 ,amt = 100000,ID=1:10, FORM=1,seq=1,dose=100000),
  ev(amt = 100000, FORM=1,ID=1:10,seq=2,dose=100000),
  ev(time=500 ,amt = 100000, FORM=0,ID=1:10,seq=2,dose=100000)
)%>% mutate(ID=ifelse(FORM==1 & seq==1,ID-10,
                      ifelse(FORM==0 & seq==2 ,ID-20,
                                                    ifelse (FORM==1 & seq==2,ID-10,ID))))




data<-pop%>%filter(ID %in% sample)%>%mutate(ID=1:20)

data.set<-left_join(dose,data,by=c("ID"))

table<-mod %>% data_set(data.set)%>% 
  carry_out(FORM,dose)%>%
  mrgsim()%>%filter(TAD>=0,TAD<=500)%>%
  mutate(FORM2=ifelse(FORM==0,"SALT","BASE"))%>%as_tibble()

names(table)

table.red<-table%>%select(ID,time,TAD,FORM2,CP)

write.csv(table.red,"/cloud/project/Data/BE_example1.csv",quote=F,na=".",row.names = F)


dataBE<-table.red%>%
  mutate(sequence=ifelse(ID>10,2,1),
         period=ifelse(time>500,2,1))%>%
  filter(TAD!=0)%>%distinct(ID,FORM2, .keep_all = TRUE)%>%
  mutate(FORM=ifelse(FORM2=="SALT",0,1))%>%
  select(ID,FORM,FORM2,sequence,period)



mod %>% data_set(data.set)%>% 
  carry_out(FORM,TAD,dose)%>%
  mrgsim()%>%plot(CP~TAD|factor(FORM),scales = "same")

plot<-ggplot(table.red,aes(x=TAD,y=CP,color=as.factor(ID)))+
      geom_line()+
      facet_grid(.~FORM2)+
  ylab("Concentration (ng/mL)")+
  xlab("Time (hrs)")+
  theme(legend.position="none")
  
plot
conc_obj <- PKNCAconc(table, CP~TAD|ID+FORM2)
data_obs_obj <- PKNCAdata(conc_obj, 
                          intervals=data.frame(start=0, end=500, aucinf.pred=TRUE,cmax=TRUE)
                          )
results_obs_obj <- pk.nca(data_obs_obj)
PKresults<-as.data.frame(results_obs_obj)

kable(as.data.frame(results_obs_obj))

AUC<-PKresults%>%filter(PPTESTCD=="aucinf.pred" | PPTESTCD=="cmax")%>%
     pivot_wider(names_from = PPTESTCD, values_from = PPORRES)%>%
     select(ID,FORM2,cmax,aucinf.pred)

names(table)


dataBEf<-join(AUC,dataBE,by=c("ID","FORM2"))%>%select(-FORM2)

write.csv(dataBEf,"/cloud/project/Data/BE_param.csv",quote=F,na=".",row.names = F)


table.red<-table%>%select(ID,FORM2,iCL,iV2,iKA,iF1)%>%distinct(ID,FORM2,.keep_all=TRUE)

merge<-join(table.red,AUC,by=c("ID","FORM2"))


write.csv(merge,"/cloud/project/Data/BE_example.csv",quote=F,na=".",row.names = F)

summ1 <- summarize_data(
  data = merge, 
  value = "aucinf.pred", 
  group = "FORM2"
)

summ2 <- summarize_data(
  data = merge, 
  value = "cmax", 
  group = "FORM2"
)

fp_AUC<-plot_forest(
  summ1, 
  vline_intercept = NULL, 
  x_lab = "AUCinf (ng*hr/mL)", 
  x_limit = c(1000,6000)
)
fp_AUC

fp_cmax<-plot_forest(
  summ2, 
  vline_intercept = NULL, 
  x_lab = "Cmax (ng/mL)", 
  x_limit = c(0,300)
)

fp_cmax

ratio<-merge%>%select(ID,FORM2,cmax,aucinf.pred)%>%
       pivot_wider(names_from=FORM2,values_from=c(cmax,aucinf.pred))%>%
      mutate(rcmax=cmax_BASE/cmax_SALT,rauc=aucinf.pred_BASE/aucinf.pred_SALT)%>%
      pivot_longer(cols=starts_with("r"),names_to="parameter",values_to="value")%>%
      mutate(parameter=ifelse(parameter=="rcmax","Cmax ratio","AUCinf ratio"))

summ.ratio <- summarize_data(
  data = ratio, 
  value = "value", 
  group = "parameter"
)
      


fp_param<-plot_forest(
  summ.ratio, 
  vline_intercept = NULL, 
  x_lab = "Ratio Base/Salt", 
  x_limit = c(0.1,1.4)
)
fp_param
