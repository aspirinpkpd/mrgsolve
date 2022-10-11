$PROB
#two compt model with first order absorption
$GLOBAL
#define CP (CENT/V2)
#define CT (PERIPH/V3)


$CMT  @annotated
GUT    : Extravascular compartment (mass)
CENT   : Central compartment (mass)
PERIPH : Peripheral compartment (mass) 
  
$ODE
  
dxdt_GUT = -iKA*GUT;
dxdt_CENT = iKA*GUT  - iCL*CP - iQ*CP + iQ*CT;
dxdt_PERIPH = iQ*CP  - iQ*CT;

$SET end = 1000, delta = 1
  
  
$PARAM @annotated
  CL  :  21 : clearance
  V2  :  32 : central volume
  Q  :  1.5: intercompartmental clearance
  V3  : 120  :peripheral volume
  KA  :  0.052: absorption rate constant
  COVKA  :   0.075: formulation effect on KA
  COVF  : 0.35  : formulation effect on F
  WT      : 70   : Baseline body weight
  FORM    : 0    : salt vs base
  
$MAIN
double TAD = self.tad();

// Formulation effect on KA
double iCOVKA=0;
if(FORM == 1) iCOVKA=COVKA; // Formulation

double iCOVF=1;
if(FORM == 1) iCOVF=COVF; // Formulation

double V_WT    = 1;
double CL_WT = 0.75;
double LOGCOVWT  = log((WT/70));
double TVF1      = 1;

double iCL =  exp(log(CL) + CL_WT*LOGCOVWT +  ETA(1)) ;  
double iV2 =  exp(log(V2) + V_WT*LOGCOVWT +  ETA(2)) ;  
double iQ =  exp(log(Q) + CL_WT*LOGCOVWT +  ETA(3)) ;  
double iV3 =  exp(log(V3) + V_WT*LOGCOVWT +  ETA(4)) ;  
double iKA =  exp(log(KA) + iCOVKA +  ETA(5)) ;    
double iF1 =  iCOVF*TVF1*exp(ETA(6));
F_GUT=iF1;

$OMEGA 0.04,0.04,0,0,0.09,0.04

$CAPTURE @annotated
  CP : Plasma concentration (mass/volume)
  CT : Peripheral concentration (mass/volume)
  iCL :  Clearance
  iV2 : :Central Volume
  KA:  typical value of KA
  iKA : KA: absorption rate constant
  iQ  : intercompartmental clearance
  iV3 : peripheral volume
  iF1 : Bioavailability
  TAD: TAD: Time after last dose