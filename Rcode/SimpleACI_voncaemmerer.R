

Kc = 404 # 260 or 404 if ci=cc von Caemmerer 1994
Ko = 248   # 179 or 248 if ci=cc
GammaStar = 37 # 38.6 or 37 if ci=cc von Caemmerer 1994
Vcmax = 80 
Vomax = Vcmax*0.25
Rd = 0.02 # range 0.01 - 0.02
jmax = 1.75*Vcmax #range 1.5 to 2 x vcmax
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
#calculate rate of carboxy
AvcHaT = ((Measci-GammaStar)*Vcmax / (Measci + Kc*(1+OXY/Ko))) - Rd  
#calculate rate of ruBP regen  
AjHaT = ((Measci - GammaStar)*jmax)/(4*Measci+8*GammaStar) -Rd

Amin_Meas = pmin(AvcHaT, AjHaT)

YearTR=as.numeric(ACI_TreeRings$TIME)
#ci/ca pre1976
caPre76 = subset(ca, YearTR<1976)
MeasciPre76 = subset(Measci, YearTR<1976)
Amin_MeasPre76 = subset(Amin_Meas, YearTR<1976)

#Ci/ca post1976
caPost76 = subset(ca, YearTR>1976)
MeasciPost76 = subset(Measci, YearTR>1976)
Amin_MeasPost76 = subset(Amin_Meas, YearTR>1976)


#Plot model output PRE/POST1976
plot(ci_mod[Avc>0],Avc [Avc>0],lwd=1.5, col="red", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,30), xlim=c(0,500))
lines(ci_mod[Avc>0],Aj [Avc>0],lwd=1.5, col="blue", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,30), xlim=c(0,500))
lines(ci_mod[Avc>0],Amin [Avc>0],lwd=3, col="black")



par(new=T)
plot(Measci,AvcHaT,type="l", lwd=3, col="blue2", axes=F, ylab="", xlab="",  
     ylim=c(0,30), xlim=c(0,500))
lines(Measci,AjHaT,type="l", lwd=3, col="red", axes=F, ylab="", xlab="",  
      ylim=c(0,30), xlim=c(0,500))


#add segments pre76
segments(caPre76,0,MeasciPre76,Amin_MeasPre76,col="cadetblue1")

#add segments post76
segments(caPost76,0,MeasciPost76,Amin_MeasPost76,col="pink")


# #quantile(Measci/ca)
# 0%       25%       50%       75%      100% 
# 0.5557746 0.6000060 0.6155859 0.6341553 0.6708146 

#ci/ca low 25%
calow = subset(ca, Measci/ca<0.6000060 )
Meascilow = subset(Measci, Measci/ca<0.6000060 )
Amin_Measlow = subset(Amin_Meas, Measci/ca<0.6000060 )

#Ci/ca post1976
caMid = subset(ca, Measci/ca>0.6000060 | Measci/ca<0.6155859)
MeasciMid = subset(Measci, Measci/ca>0.6000060 | Measci/ca<0.6155859)
Amin_MeasMid = subset(Amin_Meas, Measci/ca>0.6000060 | Measci/ca<0.6155859)

#ci/ca high 25%
cahigh = subset(ca, Measci/ca>0.6341553 )
Meascihigh = subset(Measci, Measci/ca>0.6341553 )
Amin_Meashigh = subset(Amin_Meas, Measci/ca>0.6341553 )

#Plot model output LowMedHigh supply function
plot(ci_mod[Avc>0],Avc [Avc>0],lwd=1.5, col="red", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,30), xlim=c(0,500))
lines(ci_mod[Avc>0],Aj [Avc>0],lwd=1.5, col="blue", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,30), xlim=c(0,500))
lines(ci_mod[Avc>0],Amin [Avc>0],lwd=3, col="black")



par(new=T)
plot(Measci,AvcHaT,type="l", lwd=3, col="blue2", axes=F, ylab="", xlab="",  
     ylim=c(0,30), xlim=c(0,500))
lines(Measci,AjHaT,type="l", lwd=3, col="red", axes=F, ylab="", xlab="",  
      ylim=c(0,30), xlim=c(0,500))


#add segments LOW
segments(calow,0,Meascilow,Amin_Measlow,col="black")


#add segments mid
segments(caMid,0,MeasciMid,Amin_MeasMid,col="lightgoldenrod1")

#add segments post76
segments(cahigh,0,Meascihigh,Amin_Meashigh,col="pink")





######### END #############








########################### Tsuga at Harvard
ci=ACI_TreeRings$Ha1_T
AvcHaT = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
AjHaT = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd

plot(ACI_TreeRings$TIME, AvcHaT, col="red" )
points (ACI_TreeRings$TIME, AjHaT, col="blue")


##################### Quercus at Havard
ci=ACI_TreeRings$Ha1_Q
AvcHaQ = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
AjHaQ = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
#########
plot(ACI_TreeRings$TIME, AvcHaQ, col="red" )
points (ACI_TreeRings$TIME, AjHaQ, col="blue")




##################### ABP
ci=ACI_TreeRings$ABP
AvcABP = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
AjABP = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
#########
plot(ACI_TreeRings$TIME, AvcABP, col="red" )
points (ACI_TreeRings$TIME, AjABP , col="blue")
     


##################### Tsuga at Howland
ci=ACI_TreeRings$Ho1_T
AvcHo1_T = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
AjHo1_T = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
#########
plot(ACI_TreeRings$TIME, AvcHo1_T, col="red" )
points (ACI_TreeRings$TIME, AjHo1_T , col="blue")





