#calculate the frequency of days where RuBP vs RUBSICO limits photosynthesis 

Kc = 404 # 260 or 404 if ci=cc von Caemmerer 1994
Ko = 248   # 179 or 248 if ci=cc
GammaStar = 37 # 38.6 or 37 if ci=cc von Caemmerer 1994
Vcmax = 18.49  #Griffen un published data
Vomax = Vcmax*0.25
Rd = 1 #0.554 # range 0.01 - 0.02
jmax= 45.568 #Griffen et al unpublished
jmax=Vcmax*4
#jmax=jmaxADJ_FosterMC
#jmax =jmaxADJ_HarvardRecord
#weighted J

#jmax = 29.1+1.64*Vcmax
#jmax = 1.59*Vcmax # 44.26# #range 1.5 to 2 x vcmax
OXY = 21
ca_mod = 0:1000
ci_mod = ca_mod*.616

library (dplyr)
#initial slope of J vs ppfd is defined as Phi after Farquhar & Wong, 1984
Phi = 0.331 #after Walker 2014
# theta is defined as a demensionless convexity parameter after Farquhar & Wong, 1984
theta = 0.825 #after Walker 2014

load("./data/Harvard.ameriflux.allsites.L2_data.05Mar2016.RData")
PAR1 = HarvardHa1$PAR
PAR2 = PAR1[PAR1>-1]


Measci=ACI_TreeRings$NE_Ci
ciPost96 = median(Measci[YearTR>1976])
ciPre96 = median(Measci[YearTR<1976])

J = (jmax +Phi*PAR - sqrt((jmax+Phi*PAR)^2-4*theta*jmax*Phi*PAR))/2*theta
Aj_pre76= ((ciPre96 - GammaStar)*J)/(4*ciPre96+8*GammaStar) -Rd
Aj_post76= ((ciPost96 - GammaStar)*J)/(4*ciPost96+8*GammaStar) -Rd
index=PAR-PAR+1;

Avcpre76 = (((ciPre96-GammaStar)*Vcmax / (ciPre96 + Kc*(1+OXY/Ko))) - Rd)*index   
Avcpost76 = (((ciPost96-GammaStar)*Vcmax / (ciPost96 + Kc*(1+OXY/Ko))) - Rd)*index   


RuBPlim = as.numeric(1*Aj_pre76<Avcpre76)
plot(RuBPlim)
sum(RuBPlim, na.rm = T)/sum(index, na.rm = T)











Avc = 0
Aj = 0