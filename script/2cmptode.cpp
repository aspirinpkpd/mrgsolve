$PROB
#two compt model with first order absorption
$GLOBAL
#define CP (CENT/V2)
#define CT (PERIPH/V3)

  
$CMT  @annotated
  EV   : Extravascular compartment
  CENT : Central compartment
  PERIPH : Peripheral compartment (mass) 
  
$PARAM @annotated
  CL   :  1 : Clearance (volume/time)
  V2   : 20 : Central volume (volume)
  Q    :  2 : Inter-compartmental clearance (volume/time)
  V3   : 10 : Peripheral volume of distribution (volume)
  KA   :  1 : Absorption rate constant (1/time)
  
  
$ODE
dxdt_EV = -KA*EV;
dxdt_CENT = KA*EV  - CL*CP - Q*CP + Q*CT;
dxdt_PERIPH = Q*CP  - Q*CT;

$CAPTURE @annotated
  CP : Plasma concentration (mass/volume)
  CT : Peripheral concentration (mass/volume)