

#removes all objects from the current workspace
rm(list=ls()) 
#load library
library(tidyverse)
library(haven) #open xpt,sas files
library(readxl)# open xls files

#set working directory

setwd(file.path("/cloud/project/exercises"))


##########data Types###################


#mode tells you the type of storage
mode(pi)

is.numeric(pi)

mode("Hi")
is.character(5)

mode(3<4)
as.numeric(TRUE)
as.numeric(FALSE)

# convert character to raw
raw_variable <- charToRaw("Welcome to Programiz")# converts character data to raw data

raw_variable = charToRaw("Welcome to Programiz")

print(raw_variable)
print(class(raw_variable))

# convert raw to character
char_variable <- rawToChar(raw_variable)# converts raw data to character data

print(char_variable)
print(class(char_variable))


drinks <- factor(c("beer", "wine", "rum", "whiskey","cocktail","whiskey","rum"))
print(drinks)
drinksn<-as.numeric(drinks)
#change the order of the levels

drinks2 <- factor(drinks, levels = c("whiskey","rum","cocktail", "wine", "beer"))
print(drinks2)


##############VECTORS##########################


# time vector
time<- c(0,  1, 2, 4, 6, 12, 24)
time


x <- 0:10
x
#let's create concentration values for the created times
time<-x
conc <- 10*exp(-0.32*time)
conc

plot(time,conc)
### Seq() and rep()
y <- seq(0, 10, by = 0.2)
y

y2 <- seq(0, 10, length.out=20)

y2
v <- c("a", "b", "z")

v
v1 <- rep(c("a", "b", "z"), each = 3)
v1

z2 <- rep(c("a", "b", "z"), times = 3)
z2

z3 <- rep(c("red", "green", "blue", "yellow"), 10)

z3

str(z3)
f3 <- as.factor(z3)
levels(f3)
f3
str(f3)

a3<-as.numeric(f3)

a3

#########OPEN FILES################

#open sas files

sas<-read_sas("../Data/iris.sas7bdat")

head(sas)
view(sas)
unique(sas$Species)
sas2<-filter(sas,Species=="setosa")
write_sas(sas2,"../Data/setosa.sas7bdat")

head(sas2)
view(sas2)
#open xpt files

cars<-read_xpt("../Data/mtcars.xpt")
view(cars)
names(cars)
str(cars)
cars2<-mutate(cars, new=carb*gear)

names(cars2)
view(cars2)

write_xpt(cars2,"../Data/mtcars_v2.xpt",name = NULL, label = attr(data, "label"))

?write_xpt
#open xls files

v<-read_excel("../Data/file_example_XLSX_50.xlsx", sheet = 1)
view(v)
str(v)


write.csv(v, file = "../Data/chickwts.csv",quote=F,na=".",row.names = F)

#open nonmem output

d <- read.table("../Data/simu34",header = FALSE, skip = 0)
head(d)

#bring the columns names from nonmem output table

names<-c("REP", "ID", "ROUTE","AMT", "TIME", "DAY" ,"TAD", "DV" ,
         "FOOD", "SEX" ,"BWT", "DOSE","EVID" ,"MDV" ,"CMT","CMAX","TMAX","START")


names
names(d)<-names
head(d)
str(d)


dim(d)
nrow(d)
ncol(d)

names(d)
names(d)[1] <- "rep"
names(d)
colnames(d)
rownames(d)



#Understanding the Data
#This simulated dataset contains single dose data (DAYS 1-5 and 18-22) 
#and steady-state data (DAYS 11 and 28)  
#after oral administration with [FOOD=1] and without food [FOOD=0] at 
#2 dose levels (20 and 100 mg). 
#The half-life of the drug is approximately 22 hrs. IV administration 
#after single dose is also included  (ROUTE=1).
view(d)

#filter dosing records (MDV==1)
d.1<-filter(d,MDV==0)
#create labels
d.2<- mutate(d.1, AMT= ifelse(AMT==0,NA,AMT),FOODlabel=ifelse(FOOD==0,"Fasted","Fed"),
         sexlabel=ifelse(SEX==0,"Male","Female"),
         routelabel=ifelse(ROUTE==2,"Oral","IV"),
         EXPDV=ifelse(DV==0,0,exp(DV)))
#more labels
d.3<- mutate(d.2,label2=ifelse(DAY>=1 & DAY<=5 & DOSE==20|  DAY>=18 & DAY<=22 & DOSE==20,"Single Dose 20 mg",
                       ifelse(DAY==11 & DOSE==20| DAY==28 & DOSE==20,"Steady-State 20 mg",
                              ifelse(DAY>=1 & DAY<=5 & DOSE==100 |  DAY>=18 & DAY<=22 & DOSE==100,
                                     "Single Dose 100 mg","Steady-State 100 mg"))))


view(d.3)
d.4<-mutate(d.3,label3=ifelse(routelabel=="Oral" & FOODlabel=="Fasted", "Oral Fasted",
                       ifelse(routelabel=="Oral" & FOODlabel=="Fed","Oral Fed", "IV")))



#group data by label2,label3 and ID

d.5<-group_by(d.4,label2,label3,ID)

#summarize the max value (Cmax) per patient, by Route, Food consumption, SAD vs MAD 

d.6<-dplyr::summarize(d.5, uCMAX=max(CMAX),across())

#piping

d.all<-d%>%
      filter(MDV==0)%>%
      mutate(AMT= ifelse(AMT==0,NA,AMT),
             EXPDV=ifelse(DV==0,0,exp(DV)),
             FOODlabel=ifelse(FOOD==0,"Fasted","Fed"),
             sexlabel=ifelse(SEX==0,"Male","Female"),
             routelabel=ifelse(ROUTE==2,"Oral","IV"),
             label2=ifelse(DAY>=1 & DAY<=5 & DOSE==20|  DAY>=18 & DAY<=22 & DOSE==20,"Single Dose 20 mg",
                           ifelse(DAY==11 & DOSE==20| DAY==28 & DOSE==20,"Steady-State 20 mg",
                                  ifelse(DAY>=1 & DAY<=5 & DOSE==100 |  DAY>=18 & DAY<=22 & DOSE==100,
                                         "Single Dose 100 mg","Steady-State 100 mg"))),
             label3=ifelse(ROUTE==2 & FOOD==0, "Oral Fasted",
                           ifelse(ROUTE==2 & FOOD==1,"Oral Fed", "IV")))%>%
      group_by(label2,label3,ID)%>%
      dplyr::summarize(uCMAX=max(CMAX),across())%>%
      ungroup()



d.all.unique<-d.all%>%distinct(label2,label3,ID,.keep_all = TRUE)

             
             
