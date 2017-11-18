

Kc = 404 # 260 or 404 if ci=cc von Caemmerer 1994
Ko = 248   # 179 or 248 if ci=cc
GammaStar = 37 # 38.6 or 37 if ci=cc von Caemmerer 1994
Vcmax = 80 
Vomax = vcmax*0.25
Rd = 0.02 # range 0.01 - 0.02
jmax = 1.75*vcmax #range 1.5 to 2 x vcmax
OXY = 21
ci = (0:400)
Avc = 0
Aj = 0


  Avc[ci] = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   
  Aj[ci] = ((ci - GammaStar)*jmax)/(4*ci+8*GammaStar) -Rd
  

#set up Ca and CI for plotting
ci = (1:400)
ca = ci/0.7

Avc1 = ((ci-GammaStar)*Vcmax / (ci + Kc*(1+OXY/Ko))) - Rd   


#Plot model output
plot(ca[Avc>0],Avc [Avc>0],cex=1.5, col="red")
lines(ca[Avc>0],Aj [Avc>0],cex=1.5, col="blue")


ACI_TreeRings = read.csv(file="./data/ci_TS_SingleValue.csv")


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





