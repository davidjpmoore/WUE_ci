#RUN parameters from SimpleACI_voncaemmerer.R first
library (dplyr)
#initial slope of J vs ppfd is defined as Phi after Farquhar & Wong, 1984
Phi = 0.331 #after Walker 2014

# theta is defined as a demensionless convexity parameter after Farquhar & Wong, 1984
theta = 0.825 #after Walker 2014


PAR1 = HarvardHa1$PAR

PAR = PAR1-(PAR1)


J = jmax +Phi*PAR - ((((jmax+Phi*PAR)^2)-4*theta*Phi*PAR)^0.5)/2*Phi #Equation 1(a) from Walker 2014 modified from Farquhar and Wong 1984



median(J)

