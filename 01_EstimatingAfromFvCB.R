####### Photosynthesis Analysis WUE paper Oct 2018
####### Author: Dave Moore


#####################################################
##### Section 1                                 #####
##### implementing the FVcB model for Tsuga     #####
#####################################################



Kc = 404 # 260 or 404 if ci=cc von Caemmerer 1994
Ko = 248   # 179 or 248 if ci=cc
GammaStar = 37 # 38.6 or 37 if ci=cc von Caemmerer 1994
Vcmax = 18.49  #Griffen un published data
Vomax = Vcmax*0.25
Rd = .5 #0.554 # range 0.01 - 0.02
jmax= 28 #Griffen et al unpublished
#jmax=jmaxADJ_FosterMC
#jmax =jmaxADJ_HarvardRecord
#weighted J

#jmax = 29.1+1.64*Vcmax
#jmax = 1.59*Vcmax # 44.26# #range 1.5 to 2 x vcmax
OXY = 21
ca_mod = 0:1000
ci_mod = ca_mod*.616

Avc = 0
Aj = 0


Avc = ((ci_mod-GammaStar)*Vcmax / (ci_mod + Kc*(1+OXY/Ko))) - Rd   
Aj= ((ci_mod - GammaStar)*jmax)/(4*ci_mod+8*GammaStar) -Rd
Amin = pmin(Avc,Aj)

ACI_TreeRings = read.csv(file="./data/ci_TS_SingleValue.csv")
ca = ACI_TreeRings$CO2atm
ci = ca*0.616
Measci=ACI_TreeRings$NE_Ci

#####Must recalculate for J_real

#calculate rate of carboxy
AvcHaT = ((Measci-GammaStar)*Vcmax / (Measci + Kc*(1+OXY/Ko))) - Rd  
#calculate rate of ruBP regen  
AjHaT = ((Measci - GammaStar)*jmax)/(4*Measci+8*GammaStar) -Rd

Amin_Meas = pmin(AvcHaT, AjHaT)

YearTR=as.numeric(ACI_TreeRings$TIME)

#####################################################
###### Section 2 
###### Accounting for light limitation at Harvard Forest 
###### Estimating a useful value for J given Jmax and 
###### the PAR available at Harvard Forest over the last 20 years
#####################################################

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
#plot(PAR,JHarvRec)
jmaxADJ_HarvardRecord = max(JHarvRec)

hist(JHarvRec)
library(lattice)
#hist( ~ JHarvRec | HarvardHa1$YEAR)
max (JHarvRec)
#Max Value for J is 18.91 give the limitation of light


###### Phase 3 applying model with modified J

jmaxPRIME= 18.91 #Griffen et al unpublished modified to use J calculated from Havard PAR measurements from 1991 onwards

#Reinitialize A
Avc = 0
Aj = 0


Avc = ((ci_mod-GammaStar)*Vcmax / (ci_mod + Kc*(1+OXY/Ko))) - Rd   
Aj= ((ci_mod - GammaStar)*jmaxPRIME)/(4*ci_mod+8*GammaStar) -Rd
Amin = pmin(Avc,Aj)

ACI_TreeRings = read.csv(file="./data/ci_TS_SingleValue.csv")
ca = ACI_TreeRings$CO2atm
ci = ca*0.616
Measci=ACI_TreeRings$NE_Ci
#calculate rate of carboxy
AvcHaT = ((Measci-GammaStar)*Vcmax / (Measci + Kc*(1+OXY/Ko))) - Rd  
#calculate rate of ruBP regen  
AjHaTjmaxPRIME = ((Measci - GammaStar)*jmaxPRIME)/(4*Measci+8*GammaStar) -Rd


#######
Amin_MeasjmaxPRIME = pmin(AvcHaT, AjHaTjmaxPRIME)


plot (YearTR, ca)
plot (YearTR, Amin_MeasjmaxPRIME)
plot (ca, Amin_MeasjmaxPRIME)

Amin_MeasjmaxPRIME

EstimateOfA = data.frame (YearTR,ca,Amin_MeasjmaxPRIME)
EstimateOfA


write.csv(file = "./data/EstimateOfA.csv", EstimateOfA)



#ci/ca pre1976
caPre76 = subset(ca, YearTR<1976)
MeasciPre76 = subset(Measci, YearTR<1976)
Amin_MeasPre76 = subset(Amin_MeasjmaxPRIME, YearTR<1976)

#Ci/ca post1976
caPost76 = subset(ca, YearTR>1976)
MeasciPost76 = subset(Measci, YearTR>1976)
Amin_MeasPost76 = subset(Amin_MeasjmaxPRIME, YearTR>1976)


plot(ci_mod[Avc>0],Avc [Avc>0],lwd=3, col="firebrick1", type="l",ylim=c(0,4), xlim=c(0,500), axes=F,
     ylab="", xlab="", lty=3)
box()
lines(ci_mod[Avc>0],Aj [Avc>0],lwd=3, col="deepskyblue1", type="l", ylim=c(0,4), xlim=c(0,500), lty=3)
lines(ci_mod[Avc>0],Amin [Avc>0],lwd=3, col="black")

axis(1, at=seq(0, 500, by=50),las=1 , labels = T,tck = 0.02,padj = -1, lwd=0.5,cex.lab=0.8)
axis(2, at=seq(0, 4, by=0.5),las=1 , tck = 0.02,hadj=0.5, lwd=0.5, cex.lab=0.8)
mtext(side= 2, text = "Photosythesis (μmol/m^2/s)", line = 1.9,cex=1.2)
mtext(side= 1, text =  substitute(paste("c" [i] , " (ppm)")), line = 2.4,cex=1.2)


#add segments pre76
segments(caPre76,0,MeasciPre76,Amin_MeasPre76,col="dimgrey")
#add segments post76
segments(caPost76,0,MeasciPost76,Amin_MeasPost76,col="blue2")

legend(x=300,y=4.4, "Limited by RubBP
       regeneration", bty="n")
legend(x=5,y=3, "Limited by Rubisco", bty="n")
legend(x=0,y=4, c("Vmax", "Jmax"),lty=c(3,3), col=c("firebrick1","deepskyblue1"), bty="n",
       lwd=c(3,3))
legend(x=300,y=3, 
       legend =expression(paste("c" [i] ,"/", "c" [a], " < 1976" )),
       lty=1, col="dimgrey", bty="n", lwd=1)

legend(x=300,y=2.5, 
       legend =expression(paste("c" [i] ,"/", "c" [a], " > 1976" )),
       lty=1, col="blue2", bty="n",lwd=1)

abline(v=191.907, lty=3)
abline(v=251.907, lty=3)


