
Kc = 404 # 260 or 404 if ci=cc von Caemmerer 1994
Ko = 248   # 179 or 248 if ci=cc
GammaStar = 37 # 38.6 or 37 if ci=cc von Caemmerer 1994
Vcmax = 18.49  #Griffen un published data
Vomax = Vcmax*0.25
Rd = 1 #0.554 # range 0.01 - 0.02
jmax= 45.568 #Griffen et al unpublished
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


ACI_TreeRings = read.csv(file="./data/ci_TS_SingleValue.csv")
data_rt=ACI_TreeRings$NE_Ci/ACI_TreeRings$CO2atm

#calculate rate of carboxy
  
Avc= ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd  
  

#calculate rate of ruBP regen  
  Aj = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
  
# Avc1 = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   

Amin = pmin(Avc,Aj)


########################### DATA

#Redefine Ci 
#ci=ACI_TreeRings$Ha1_T
#assign Ci value
Measci=ACI_TreeRings$NE_Ci

#calculate rate of carboxy
AvcHaT = ((Measci-GammaStar)*Vcmax / (Measci + Kc*(1+OXY/Ko))) - Rd  

#calculate rate of ruBP regen  
AjHaT = ((Measci - GammaStar)*jmax)/(4*Measci+8*GammaStar) -Rd

MinfromObs = pmin(AvcHaT, AjHaT)

#Plot model output
plot(ca[Avc>0],Avc [Avc>0],lwd=1.5, col="red", type="l",ylab = "Photosynthesis", xlab = "Atmospheric CO2", ylim=c(3,7), xlim=c(290,395))
lines(ca[Avc>0],Aj [Avc>0],lwd=1.5, col="blue")
lines(ca[Avc>0],Amin [Avc>0],lwd=3, col="black")

par(new=T)
plot(ACI_TreeRings$CO2atm,AvcHaT,type="l", lwd=3, col="blue2", axes=F, ylab="", xlab="",ylim=c(3,7), 
     xlim=c(290,395))
lines(ACI_TreeRings$CO2atm,AjHaT,type="l", lwd=3, col="red", axes=F, ylab="", xlab="",ylim=c(3,7), 
     xlim=c(290,395))
lines(ACI_TreeRings$CO2atm,MinfromObs,type="l", lwd=1, lty=2, col="white", axes=F, ylab="", xlab="",ylim=c(3,7), 
      xlim=c(290,395))



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
# ##################### Tsuga at Howland
# ci=ACI_TreeRings$Ho1_T
# AvcHo1_T = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
# AjHo1_T = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
# #########
# plot(ACI_TreeRings$TIME, AvcHo1_T, col="red" )
# points (ACI_TreeRings$TIME, AjHo1_T , col="blue")

# 
# 
# ## bla
# newM=as.matrix(cbind(AvcHaT,AjHaT,ci,ACI_TreeRings$CO2atm))
# junk = pmin(newM[,1], newM[,2])
# 
# min <- c()
# for (i in ncol(newM)){
# min=which.min(newM[,i])}

