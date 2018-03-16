#RUN parameters from SimpleACI_voncaemmerer.R first
library (dplyr)
#initial slope of J vs ppfd is defined as Phi after Farquhar & Wong, 1984
Phi = 0.331 #after Walker 2014
# theta is defined as a demensionless convexity parameter after Farquhar & Wong, 1984
theta = 0.825 #after Walker 2014

load("./data/Harvard.ameriflux.allsites.L2_data.05Mar2016.RData")
PAR1 = HarvardHa1$PAR
PAR = PAR1[PAR1>-1]


JHarvRec = (jmax +Phi*PAR - sqrt((jmax+Phi*PAR)^2-4*theta*jmax*Phi*PAR))/2*theta
  #Equation 1(a) from Walker 2014 modified from Farquhar and Wong 1984
plot(PAR,JJHarvRec)
jmaxADJ_HarvardRecord = max(JHarvRec)

hist(JHarvRec)


BelowCanopyPAR = read.csv("./data/BelowCanopy_FOSTERhf063-01.csv")



plot(BelowCanopyPAR$ac.par,col="blue2", ylim=c(0,2000), xlim=c(0,8000), ylab="PAR", xlab="Time")
par(new=T)
plot(BelowCanopyPAR$uc.par,col="red", ylim=c(0,2000), xlim=c(0,8000),ylab="", xlab="")
par(new=T)
plot(BelowCanopyPAR$mc.par,col="black", ylim=c(0,2000), xlim=c(0,8000),ylab="", xlab="")
par(new=T)
plot(BelowCanopyPAR$lc.par,col="yellow", ylim=c(0,2000), xlim=c(0,8000),ylab="", xlab="")

PAR = BelowCanopyPAR$ac.par[BelowCanopyPAR$ac.par>0]
Jac =  (jmax +Phi*PAR - sqrt((jmax+Phi*PAR)^2-4*theta*jmax*Phi*PAR))/2*theta
#Equation 1(a) from Walker 2014 modified from Farquhar and Wong 1984
hist(Jac)

PAR = BelowCanopyPAR$uc.par[BelowCanopyPAR$uc.par>0]
Juc =  (jmax +Phi*PAR - sqrt((jmax+Phi*PAR)^2-4*theta*jmax*Phi*PAR))/2*theta
#Equation 1(a) from Walker 2014 modified from Farquhar and Wong 1984


PAR = BelowCanopyPAR$mc.par[BelowCanopyPAR$mc.par>0]
Jmc = (jmax +Phi*PAR - sqrt((jmax+Phi*PAR)^2-4*theta*jmax*Phi*PAR))/2*theta
#Equation 1(a) from Walker 2014 modified from Farquhar and Wong 1984



PAR = BelowCanopyPAR$lc.par[BelowCanopyPAR$lc.par>0]
Jlc =  (jmax +Phi*PAR - sqrt((jmax+Phi*PAR)^2-4*theta*jmax*Phi*PAR))/2*theta
#Equation 1(a) from Walker 2014 modified from Farquhar and Wong 1984

jmaxADJ_FosterAC = median(Jac, na.rm = T)
jmaxADJ_FosterUC = median(Juc, na.rm = T)
jmaxADJ_FosterMC = median(Jmc, na.rm = T)
jmaxADJ_FosterLC = median(Jlc, na.rm = T)

MedJCanopy = c(jmax, jmaxADJ_FosterAC, jmaxADJ_FosterUC, jmaxADJ_FosterMC, jmaxADJ_FosterLC)


PARFosterAC = max(BelowCanopyPAR$ac.par, na.rm = T)
PARFosterUC = max(BelowCanopyPAR$uc.par, na.rm = T)
PARFosterMC = max(BelowCanopyPAR$mc.par, na.rm = T)
PARFosterLC = max(BelowCanopyPAR$lc.par, na.rm = T)

max (BelowCanopyPAR$ac.par)

medPARcanopy = c(2400,PARFosterAC, PARFosterUC, PARFosterMC, PARFosterLC)
plot (medPARcanopy,MedJCanopy,  ylab="Median J calculated", xlab="Maximum hourly PAR"
      ,pch=16, tck = 0.02)
legend(x=1800,y=48, "Vcmax @ saturation", bty="n")
legend(x=350,y=48.5, "Effect of canopy position on Jmax'", bty="n")

plot(Jac ,col="blue2", ylim=c(0,60), xlim=c(0,5000), ylab="J canopy position", xlab="Time")
par(new=T)
plot(Juc,col="red", ylim=c(0,60), xlim=c(0,5000),ylab="", xlab="")
par(new=T)
plot(Jmc,col="black", ylim=c(0,60), xlim=c(0,5000),ylab="", xlab="")
par(new=T)
plot(Jlc,col="yellow", ylim=c(0,60), xlim=c(0,5000),ylab="", xlab="")


