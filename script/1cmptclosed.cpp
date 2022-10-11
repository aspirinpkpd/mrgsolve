$PROB
#one compt model with first order absorption
  
$GLOBAL
#define CP (CENT/V)
  
$PKMODEL ncmt = 1, depot = TRUE
  
$CMT  @annotated
  EV   : Extravascular compartment
  CENT : Central compartment
  
$PARAM @annotated
  CL   :  1 : Clearance (volume/time)
  V    : 20 : Central volume (volume)
  KA   :  1 : Absorption rate constant (1/time)
  
$CAPTURE @annotated
  CP : Plasma concentration (mass/volume)
  