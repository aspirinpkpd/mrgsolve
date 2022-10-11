$PARAM @annotated
CL   :  1 : Clearance (volume/time)
V    : 20 : Central volume (volume)
Q     : 0.5 : intercompartmental clearance (volume/time)
KA   :  1 : Absorption rate constant (1/time)

$CMT  @annotated
EV   : Extravascular compartment
CENT : Central compartment


$GLOBAL
#define CP (CENT/V)

$PKMODEL ncmt = 1, depot = TRUE

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
