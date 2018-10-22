

Kc = 404 # 260 or 404 if ci=cc von Caemmerer 1994
Ko = 248   # 179 or 248 if ci=cc
GammaStar = 37 # 38.6 or 37 if ci=cc von Caemmerer 1994
Vcmax = 18.49  #Griffen un published data
Vomax = Vcmax*0.25
Rd = 1 #0.554 # range 0.01 - 0.02
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
plot(ci_mod[Avc>0],Avc [Avc>0],lwd=1.5, col="red", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,7), xlim=c(0,500))
lines(ci_mod[Avc>0],Aj [Avc>0],lwd=1.5, col="blue", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,7), xlim=c(0,500))
lines(ci_mod[Avc>0],Amin [Avc>0],lwd=3, col="black")



par(new=T)
plot(Measci,AvcHaT,type="l", lwd=3, col="blue2", axes=F, ylab="", xlab="",  
     ylim=c(0,7), xlim=c(0,500))
lines(Measci,AjHaT,type="l", lwd=3, col="red", axes=F, ylab="", xlab="",  
      ylim=c(0,7), xlim=c(0,500))


#add segments pre76
segments(caPre76,0,MeasciPre76,Amin_MeasPre76,col="cadetblue1")

#add segments post76
segments(caPost76,0,MeasciPost76,Amin_MeasPost76,col="pink")


plot(ci_mod[Avc>0],Avc [Avc>0],lwd=3, col="firebrick1", type="l",ylim=c(0,8), xlim=c(0,500), axes=F,
     ylab="", xlab="", lty=3)
box()
lines(ci_mod[Avc>0],Aj [Avc>0],lwd=3, col="deepskyblue1", type="l", ylim=c(0,7), xlim=c(0,500), lty=3)
lines(ci_mod[Avc>0],Amin [Avc>0],lwd=3, col="black")

axis(1, at=seq(0, 500, by=50),las=1 , labels = T,tck = 0.02,padj = -1, lwd=0.5,cex.lab=0.8)
axis(1, at=seq(0, 8, by=1),las=1 , tck = 0.02,hadj=0.5, lwd=0.5, cex.lab=0.8)
mtext(side= 4, text = "Photosythesis (μmol/m/s)", line = 1.9,cex=0.8)
mtext(side= 1, text =  substitute(paste("c" [i] , " (ppm)")), line = 2.4,cex=0.8)

#add segments pre76
segments(caPre76,0,MeasciPre76,Amin_MeasPre76,col="dimgrey")
#add segments post76
segments(caPost76,0,MeasciPost76,Amin_MeasPost76,col="blue2")

legend(x=350,y=6, "Limited by RubBP
       regeneration", bty="n")
legend(x=100,y=2, "Limited
       by Rubisco", bty="n")
legend(x=80,y=7, c("Vmax", "Jmax"),lty=c(3,3), col=c("firebrick1","deepskyblue1"), bty="n",
       lwd=c(3,3))
legend(x=350,y=3, 
       legend =expression(paste("c" [i] ,"/", "c" [a], " < 1976" )),
       lty=1, col="dimgrey", bty="n", lwd=1)

legend(x=350,y=2.5, 
       legend =expression(paste("c" [i] ,"/", "c" [a], " > 1976" )),
       lty=1, col="blue2", bty="n",lwd=1)

abline(v=191.907, lty=3)
abline(v=251.907, lty=3)



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
plot(ci_mod[Avc>0],Avc [Avc>0],lwd=1.5, col="red", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,7), xlim=c(0,500))
lines(ci_mod[Avc>0],Aj [Avc>0],lwd=1.5, col="blue", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(0,7), xlim=c(0,500))
lines(ci_mod[Avc>0],Amin [Avc>0],lwd=3, col="black")

#highlight period of measurements
par(new=T)
plot(Measci,AvcHaT,type="l", lwd=3, col="blue2", axes=F, ylab="", xlab="",  
     ylim=c(0,7), xlim=c(0,500))
lines(Measci,AjHaT,type="l", lwd=3, col="red", axes=F, ylab="", xlab="",  
      ylim=c(0,7), xlim=c(0,500))

#add segments mid
segments(caMid,0,MeasciMid,Amin_MeasMid,col="lightgoldenrod1")

#add segments LOW
segments(calow,0,Meascilow,Amin_Measlow,col="black")

#add segments High
segments(cahigh,0,Meascihigh,Amin_Meashigh,col="pink")

plot (YearTR, ca)
######### END #############




# 
# 
# 
# 
# ########################### Tsuga at Harvard
# ci=ACI_TreeRings$Ha1_T
# AvcHaT = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
# AjHaT = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
# 
# plot(ACI_TreeRings$TIME, AvcHaT, col="red" )
# points (ACI_TreeRings$TIME, AjHaT, col="blue")
# 
# 
# ##################### Quercus at Havard
# ci=ACI_TreeRings$Ha1_Q
# AvcHaQ = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
# AjHaQ = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
# #########
# plot(ACI_TreeRings$TIME, AvcHaQ, col="red" )
# points (ACI_TreeRings$TIME, AjHaQ, col="blue")
# 
# 
# 
# 
# ##################### ABP
# ci=ACI_TreeRings$ABP
# AvcABP = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
# AjABP = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
# #########
# plot(ACI_TreeRings$TIME, AvcABP, col="red" )
# points (ACI_TreeRings$TIME, AjABP , col="blue")
#      
# 
# 
# ##################### Tsuga at Howland
# ci=ACI_TreeRings$Ho1_T
# AvcHo1_T = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
# AjHo1_T = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
# #########
# plot(ACI_TreeRings$TIME, AvcHo1_T, col="red" )
# points (ACI_TreeRings$TIME, AjHo1_T , col="blue")
# 
# 
# 
# 
# 
