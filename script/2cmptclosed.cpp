$PROB
#two compt model with first order absorption
  
$GLOBAL
#define CP (CENT/V2)

  
$PKMODEL ncmt = 2, depot = TRUE
  
$CMT  @annotated
  EV     : Extravascular compartment
  CENT   : Central compartment
  PERIPH : Peripheral compartment (mass) 
  
$PARAM @annotated
  CL   :  1 : Clearance (volume/time)
  V2   : 20 : Central volume (volume)
  Q    :  2 : Inter-compartmental clearance (volume/time)
  V3   : 10 : Peripheral volume of distribution (volume)
  KA   :  1 : Absorption rate constant (1/time)
  
$CAPTURE @annotated
  CP : Plasma concentration (mass/volume)
  