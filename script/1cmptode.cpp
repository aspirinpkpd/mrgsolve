$PROB
#one compt model with first order absorption
$GLOBAL
#define CP (CENT/V)
  
$CMT  @annotated
  EV   : Extravascular compartment
  CENT : Central compartment
  
$PARAM @annotated
  CL   :  1 : Clearance (volume/time)
  V    : 20 : Central volume (volume)
  KA   :  1 : Absorption rate constant (1/time)
  
$ODE
dxdt_EV = -KA*EV;
dxdt_CENT = KA*EV  - CL*CP;

$CAPTURE @annotated
  CP : Plasma concentration (mass/volume)
  