##BE example
#install.packages("pmforest",repo="https://mpn.metworx.com/snapshots/stable/2022-06-15")
##libraries


rm(list=ls())
library(mrgsolve)
library(tidyverse)
library(pmforest)
library(pmtables)
library(texPreview)
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
options(mrggsave.dir = figDir, mrg.script = "BE.R",pmtables.dir = tabDir)


# Set any script options ----------------------------


data<-read.csv("/cloud/project/Data/BE_example1.csv")

#let's tabulate the dat
data1<-data%>%dplyr::rename(DV=CP)
out<-pt_data_inventory(
  data1,
  by=c(Formulation="FORM2"),
  stacked=TRUE
)

out2<-st_new(out)%>%stable(r_file="BE.R", output_file="../Tables/summary.tex")%>%
  stable_save()%>%st_preview()
  #st2report(show_pdf = TRUE) %>%
 

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

results<-PKresults%>%
        pivot_wider(names_from=PPTESTCD, values_from=PPORRES)

tab<-pt_cont_wide(
  data=results,
  cols="cmax,tmax,half.life,aucinf.pred",
  panel=as.panel("FORM2",prefix="Formulation:")
)



  st_new(tab)%>%
  st_files(output="../Tables/summary_param.tex")%>%
  stable(cols_rename = c("Cmax(ng/mL)" = "cmax", "Tmax (hrs)" ="tmax","t1/2 (hrs)"="half.life",
                         "AUCinf (ng*hr/mL)"="aucinf.pred"),cols_bold = TRUE)%>%
  stable_save()%>%st_preview()

  st2report(stable(tab),
            caption = c("Parameter summary by Formulation"),
            ntex=2,
            stem="tab_param",
            output_dir = tabDir)
  
  st2report(list(stable(out),stable(tab)) ,
            ntex=2,
            stem = "preview",
            output_dir = tabDir)
            


kable(as.data.frame(results_obs_obj))



AUC<-PKresults%>%filter(PPTESTCD=="aucinf.pred" | PPTESTCD=="cmax")%>%
  pivot_wider(names_from = PPTESTCD, values_from = PPORRES)%>%
  select(ID,FORM2,cmax,aucinf.pred)

#libreria forest plot

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
  x_limit = c(1000,8000)
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


GMR<-exp(res1[4,1] )

LB<-exp(res1[4,1] -1 * qt(.95,df=res1[4,3])*res1[4,2])

UB<-exp(res1[4,1] +1 * qt(.95,df=res1[4,3])*res1[4,2])

#print GMR, lower, and upper CI

ci1

#repeat for AUCinf.pred
#create a dataframe with Ci1 and ci2 and  plot it using pmForest

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


