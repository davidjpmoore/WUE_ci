#Read and plot LAI data from Harvard Forest
#install.packages("readr")
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
install.packages(lubridate)
  library(lubridate)

Harv_LAIMOD=read_csv("./data/MODIS_ORNLDAAC_statistics_Lai_500m.csv")
Harv_LAIMOD = Harv_LAIMOD %>%
  mutate (DateLAIMod = as.Date(dt,format='%m/%d/%Y'), LAIModis=value_mean, LAIModis_err = value_standard_deviation)
  
plot (Harv_LAIMOD$DateLAIMod,Harv_LAIMOD$LAIModis)


HarvBADM2018=read_csv("./data/AMF_US-Ha1_BIF_LATEST10162018.csv")

HarvBADM2018 %>% 
  group_by(VARIABLE) %>% 
  summarize(counts = n())


LAI_HarvBADM2018 = HarvBADM2018 %>% 
   filter(HarvBADM2018$VARIABLE == "LAI_O_DEC") %>% 
  rename(LAI_insitu =DATAVALUE) %>% 
  select (GROUP_ID, LAI_insitu)

LAI_err_HarvBADM2018 = HarvBADM2018 %>% 
  filter(HarvBADM2018$VARIABLE == "LAI_O_DEC_SPATIAL_VARIABILITY") %>% 
    rename(LAI_SpErr =DATAVALUE) %>% 
select (GROUP_ID, LAI_SpErr)
  
plot(LAI_err_HarvBADM2018$GROUP_ID, LAI_err_HarvBADM2018$LAI_SpErr)


LAI_Date_HarvBADM2018 = HarvBADM2018 %>% 
  filter(HarvBADM2018$VARIABLE == "LAI_DATE") %>% 
rename(DateMeas =DATAVALUE) %>% 
  select (GROUP_ID, DateMeas)

LAI_insitu_01 = left_join(LAI_Date_HarvBADM2018, LAI_HarvBADM2018, by = "GROUP_ID") 


LAI_insitu = left_join(LAI_insitu_01, LAI_err_HarvBADM2018, by = "GROUP_ID") %>% 
  mutate(Yr=as.numeric(substr(DateMeas, 1, 4)), Mon=as.numeric(substr(DateMeas, 5, 6)), DoM=as.numeric(substr(DateMeas, 7, 8))) %>%
mutate (DateLAI = ISOdate(Yr,Mon,DoM))

#Simple plot of insitu LAI
plot(LAI_insitu$DateLAI,LAI_insitu$LAI_insitu )



plot(Harv_LAIMOD$DateLAIMod,Harv_LAIMOD$LAIModis, type="l",col="red")
points(LAI_insitu$DateLAI,LAI_insitu$LAI_insitu,col="green")