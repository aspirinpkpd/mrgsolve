##BE example

##libraries


rm(list=ls())
library(mrgsolve)
library(tidyverse)
library(pmtables)
library(pmforest)
library(mrggsave)

library(nlme)

suppressPackageStartupMessages({
  library(PKNCA)
  library(dplyr)
  library(cowplot)
  library(knitr)
})



#open file
setwd(file.path("/cloud/project/exercises"))
figDir <- file.path("../Figures")
tabDir <- file.path("../Tables")
options(mrggsave.dir = figDir, mrg.script = "BE_sol.R",pmtables.dir = tabDir)

modlib<-"/cloud/project/script"

data<-read.csv("/cloud/project/Data/BE_example1.csv")


plot<-ggplot(data,aes(x=TAD,y=CP,color=as.factor(ID)))+
  geom_line()+
  facet_grid(.~FORM2)+
  ylab("Concentration (ng/mL)")+
  xlab("Time (hrs)")+
  theme(legend.position="none")

plot

conc_obj <- PKNCAconc(data, CP~TAD|ID+FORM2)
data_obs_obj <- PKNCAdata(conc_obj, 
                          intervals=data.frame(start=0, end=500, aucinf.pred=TRUE,cmax=TRUE)
)
results_obs_obj <- pk.nca(data_obs_obj)
PKresults<-as.data.frame(results_obs_obj)

kable(as.data.frame(results_obs_obj))

AUC<-PKresults%>%filter(PPTESTCD=="aucinf.pred" | PPTESTCD=="cmax")%>%
  pivot_wider(names_from = PPTESTCD, values_from = PPORRES)%>%
  select(ID,FORM2,cmax,aucinf.pred)


summ1 <- summarize_data(
  data = AUC, 
  value = "aucinf.pred", 
  group = "FORM2"
)

summ2 <- summarize_data(
  data = AUC, 
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

mrggsave(list(fp_AUC,fp_cmax), tag = "AUC_cmax",width = 7, height = 7)

ratio<-AUC%>%
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
  shaded_interval = c(0.8, 1.25),
  x_limit = c(0.1,1.4)
)
fp_param



mrggsave(fp_param, tag = "forest_ratio",width = 7, height = 7,onefile=FALSE,dev=c("pdf,png"))

dataBEf<-read.csv("/cloud/project/Data/BE_param.csv")


#FORM==0 salt (reference)

fit1 <- lme(log(cmax) ~ sequence + period + FORM,
            random = ~1|ID, data=dataBEf)


#create a table with the summary of the fit1

res1 <- summary(fit1)$tTable       

#print the table

res1

#calculate CI

ci1 <- exp(res1[4,1] + c(0,-1,1) * qt(.95,df=res1[4,3])*res1[4,2])

#print GMR ,lower, and upper CI

ci1

t<-data.frame(ci1)
t<-data.frame(t(ci1))

names(t)<-c("mid","lo","hi")

t1<-t%>%mutate(group="Cmax")%>%select(group,mid,lo,hi)

#repeat for AUCinf.pred


fit2 <- lme(log(aucinf.pred) ~ sequence + period + FORM,
            random = ~1|ID, data=dataBEf)


#create a table with the summary of the fit1

res2 <- summary(fit2)$tTable       

#print the table

res2

#calculate CI

ci2 <- exp(res2[4,1] + c(0,-1,1) * qt(.95,df=res2[4,3])*res2[4,2])

#print GMR, lower, and upper CI

ci2

tb<-data.frame(t(ci2))

names(tb)<-c("mid","lo","hi")

t2<-tb%>%mutate(group="AUCinf")%>%select(group,mid,lo,hi)


#create a dataframe with Ci1 and ci2 and  plot it using pmForest

results<-rbind(t1,t2)

results


fp_GMR<-plot_forest(
  results, 
  vline_intercept = NULL, 
  x_lab = "GMR Base/Salt", 
  shaded_interval = c(0.8, 1.25),
  x_limit = c(0.1,1.4)
)
fp_GMR


mrggsave(fp_GMR, tag = "forest_GMR",width = 7, height = 7,onefile=FALSE,dev=c("pdf,png"))

#write the mrsgolve model for a 2 cmp model 
#with first order absorption and formulation as covariate 
#on ka and relative F


# CL  :  21 : clearance
# V2  :  32 : central volume
# Q  :  1.5: intercompartmental clearance
# V3  : 120  :peripheral volume
# KA  :  0.052: absorption rate constant
# COVKA  :   0.075: formulation effect on KA
# COVF  : 0.35  : formulation effect on F
# WT      : 70   : Baseline body weight
# FORM    : 0    : salt vs base

#explore a different schedule for base to obtain similar exposure than salt

mod<-mread("2cmptodebio", modlib)


mod%>%ev(amt=100000,ii=24,addl=4,ID=1:10)%>%mrgsim(end=120,delta=1)%>%
  plot(CP~time)

table.QD<-mod%>%ev(amt=100000,ii=24,addl=4,ID=1:10)%>%mrgsim(end=120,delta=1)%>%as_tibble()%>%
          mutate(FORM="SALT QD")%>%filter(time>95)
mod%>%ev(amt=100000,ii=8,addl=14,FORM=1,ID=1:10)%>%mrgsim(end=120,delta=1)%>%plot(CP~time)

table.TID<-mod%>%ev(amt=100000,ii=8,addl=14,FORM=1,ID=1:10)%>%mrgsim(end=120,delta=1)%>%as_tibble()%>%
  mutate(FORM="BASE TID",ID=ID+10)%>%filter(time>95)

mod%>%ev(amt=100000,ii=12,addl=9,FORM=1,ID=1:10)%>%mrgsim(end=120,delta=1)%>%plot(CP~time)

table.BID<-mod%>%ev(amt=100000,ii=12,addl=9,FORM=1,ID=1:10)%>%mrgsim(end=120,delta=1)%>%as_tibble()%>%
  mutate(FORM="BASE BID",ID=ID+20)%>%filter(time>95)

mod%>%ev(amt=100000,ii=24,addl=4,FORM=1,ID=1:10)%>%mrgsim(end=120,delta=1)%>%plot(CP~time)

table.QD.BASE<-mod%>%ev(amt=100000,ii=24,addl=4,FORM=1,ID=1:10)%>%mrgsim(end=120,delta=1)%>%as_tibble()%>%
  mutate(FORM="BASE QD",ID=ID+30)%>%filter(time>95)


data.all<-rbind(table.QD,table.TID,table.BID,table.QD.BASE)%>%select(ID,time,TAD,CP,FORM)



conc_obj <- PKNCAconc(data.all, CP~time|ID+FORM)
data_obs_obj <- PKNCAdata(conc_obj, 
                          intervals=data.frame(start=96, end=120, auclast=TRUE,cmax=TRUE)
)
results_obs_obj <- pk.nca(data_obs_obj)
PKresults<-as.data.frame(results_obs_obj)

kable(as.data.frame(results_obs_obj))

AUC<-PKresults%>%filter(PPTESTCD=="auclast" | PPTESTCD=="cmax")%>%
  pivot_wider(names_from = PPTESTCD, values_from = PPORRES)%>%
  select(ID,FORM,Cmax=cmax,AUClast=auclast)


cont_long_custom <- function(value, ...) {
  value <- na.omit(value) #remove NAs
  ans <- data.frame(
    GeoMean = exp(mean(log(value))), 
    CV = sqrt(exp(sd(log(value))^2)-1)*100,
    Median=median(value),
    Mean=mean(value),
    SD=sd(value)
  )
  mutate(ans, across(everything(), sig))
}

cont_long_custom(AUC$AUClast)

tab<-pt_cont_long(
  data=AUC,
  cols="Cmax,AUClast",
  panel=as.panel("FORM",prefix="Formulation:"),
  fun = cont_long_custom
)

st_new(tab)%>%
  st_files(output="../Tables/QDvsTID_GEOM_param.tex")%>%
  stable(cols_rename = c("Cmax(ng/mL)" = "cmax", "AUC24 (ng*hr/mL)"="auclast"),
         cols_bold = TRUE,
         notes = c("GeoMean: Geometric Mean", "CV: Coefficient of variation of the geometric mean",
                   "SD: standard deviation"))%>%
  stable_save()%>%
  st_preview()

st2report(list(stable(tab)) ,
          ntex=2,
          stem = "preview_sumstats",
          output_dir = tabDir)


#parallel comparison?
