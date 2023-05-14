###Rcode for Climate Data course Project
###Written by Andrew Pavlu
###Spring 2023



#packages for Raster work
install.packages('raster')
install.packages('rgdal')
install.packages('sp')
library(sp)
library(raster)
library(sf)
library(rgdal)
library(terra)
library(tidyverse)
library(stats)
library(lubridate)
library(exactextractr)
.rs.unloadPackage("tidyr")

#_________________________________________________________

fpath <- 'C:/Users/andre/Desktop/Spring 2023 Semester Files/Climate Data_ Geog 701r/Cove/'
setwd(fpath)
MACAPath <- 'C:/Users/andre/Desktop/Spring 2023 Semester Files/Climate Data_ Geog 701r/Cove/MACA/'
ClimEngPath <- 'C:/Users/andre/Desktop/Spring 2023 Semester Files/Climate Data_ Geog 701r/Cove/Climate Engine/'

###Load in the first raster
###Raster name convention: RCP level_Beginning year for interval_Variable. So 2010 for the 2010-2039 range
RCP45_2010_pr <- raster(paste0(MACAPath,'macav2metdata_pr_ANN_20102039_rcp45_20CMIP5ModelMean.tif'))
plot(RCP45_2010_pr)
RCP45_2040_pr<-  raster(paste0(MACAPath,'macav2metdata_pr_ANN_20402069_rcp45_20CMIP5ModelMean.tif'))

RCP45_2070_pr<-  raster(paste0(MACAPath,'macav2metdata_pr_ANN_20702099_rcp45_20CMIP5ModelMean.tif'))

RCP85_2010_pr<-  raster(paste0(MACAPath,'macav2metdata_pr_ANN_20102039_rcp85_20CMIP5ModelMean.tif'))

RCP85_2040_pr<-  raster(paste0(MACAPath,'macav2metdata_pr_ANN_20402069_rcp85_20CMIP5ModelMean.tif'))

RCP85_2070_pr<-  raster(paste0(MACAPath,'macav2metdata_pr_ANN_20702099_rcp85_20CMIP5ModelMean.tif'))

#now the AET rasters
RCP45_2010_pet <- raster(paste0(MACAPath,'macav2metdata_pet_ANN_20102039_rcp45_20CMIP5ModelMean.tif'))

RCP45_2040_pet<-  raster(paste0(MACAPath,'macav2metdata_pet_ANN_20402069_rcp45_20CMIP5ModelMean.tif'))

RCP45_2070_pet<-  raster(paste0(MACAPath,'macav2metdata_pet_ANN_20702099_rcp45_20CMIP5ModelMean.tif'))

RCP85_2010_pet<-  raster(paste0(MACAPath,'macav2metdata_pet_ANN_20102039_rcp85_20CMIP5ModelMean.tif'))

RCP85_2040_pet<-  raster(paste0(MACAPath,'macav2metdata_pet_ANN_20402069_rcp85_20CMIP5ModelMean.tif'))

RCP85_2070_pet<-  raster(paste0(MACAPath,'macav2metdata_pet_ANN_20702099_rcp85_20CMIP5ModelMean.tif'))

#Load in the lake area, Lake Drainage area, and HUC12
CoveLake<- st_read(paste0(fpath,'Cove Lake.kml'))
CoveLake<-CoveLake[,-(2)]#Removes the discription col, which is empty anyways
CoveLake<- st_zm(CoveLake,drop = T)#this drops the Z value to use with raster(no z values)
plot(CoveLake,main = 'Cove Lake',col='transparent')

CoveLakeDrain<- st_read(paste0(fpath, 'Cove Lake Drainage.kml'))
CoveLakeDrain<- CoveLakeDrain[,-(2)]
CoveLakeDrain<- st_zm(CoveLakeDrain,drop=T)
plot(CoveLakeDrain)


#________Extract and sum the raster values for the Lake area

ZS_RCP45_2010_pr <- exact_extract(RCP45_2010_pr,CoveLake,'sum')
ZS_RCP45_2010_pr# this value in  annual inches is the sum of the direct lake projected precip(weighted area value)

ZS_RCP45_2040_pr <- exact_extract(RCP45_2040_pr,CoveLake,'sum')

ZS_RCP45_2070_pr <- exact_extract(RCP45_2070_pr,CoveLake,'sum')

ZS_RCP85_2010_pr <- exact_extract(RCP85_2010_pr,CoveLake,'sum')

ZS_RCP85_2040_pr <- exact_extract(RCP85_2040_pr,CoveLake,'sum')

ZS_RCP85_2070_pr <- exact_extract(RCP85_2070_pr,CoveLake,'sum')
ZS_RCP85_2070_pr
#Now for PET
ZS_RCP45_2010_pet <- exact_extract(RCP45_2010_pet,CoveLake,'sum')

ZS_RCP45_2040_pet <- exact_extract(RCP45_2040_pet,CoveLake,'sum')

ZS_RCP45_2070_pet <- exact_extract(RCP45_2070_pet,CoveLake,'sum')

ZS_RCP85_2010_pet <- exact_extract(RCP85_2010_pet,CoveLake,'sum')

ZS_RCP85_2040_pet <- exact_extract(RCP85_2040_pet,CoveLake,'sum')

ZS_RCP85_2070_pet <- exact_extract(RCP85_2070_pet,CoveLake,'sum')
#Now extract precip for immediate lake drainage area
ZS_RCP45_2010_pr_dr <- exact_extract(RCP45_2010_pr,CoveLakeDrain,'sum')

ZS_RCP45_2040_pr_dr <- exact_extract(RCP45_2040_pr,CoveLakeDrain,'sum')

ZS_RCP45_2070_pr_dr <- exact_extract(RCP45_2070_pr,CoveLakeDrain,'sum')

ZS_RCP85_2010_pr_dr <- exact_extract(RCP85_2010_pr,CoveLakeDrain,'sum')

ZS_RCP85_2040_pr_dr <- exact_extract(RCP85_2040_pr,CoveLakeDrain,'sum')

ZS_RCP85_2070_pr_dr <- exact_extract(RCP85_2070_pr,CoveLakeDrain,'sum')
ZS_RCP45_2010_pr <- exact_extract(RCP45_2010_pr,CoveLake,'sum')
ZS_RCP45_2010_pr# this value in  annual inches is the sum of the direct lake projected precip(weighted area value)
#________________________________________________________
ZS_RCP45_2010_pr_max <- exact_extract(RCP45_2010_pr,CoveLake,'max')
ZS_RCP45_2040_pr_max <- exact_extract(RCP45_2040_pr,CoveLake,'max')
ZS_RCP45_2070_pr_max <- exact_extract(RCP45_2070_pr,CoveLake,'max')
ZS_RCP85_2010_pr_max <- exact_extract(RCP85_2010_pr,CoveLake,'max')
ZS_RCP85_2040_pr_max <- exact_extract(RCP85_2040_pr,CoveLake,'max')
ZS_RCP85_2070_pr_max <- exact_extract(RCP85_2070_pr,CoveLake,'max')
#Now for PET
ZS_RCP45_2010_pet_max <- exact_extract(RCP45_2010_pet,CoveLake,'max')
ZS_RCP45_2040_pet_max <- exact_extract(RCP45_2040_pet,CoveLake,'max')
ZS_RCP45_2070_pet_max <- exact_extract(RCP45_2070_pet,CoveLake,'max')
ZS_RCP85_2010_pet_max <- exact_extract(RCP85_2010_pet,CoveLake,'max')
ZS_RCP85_2040_pet_max <- exact_extract(RCP85_2040_pet,CoveLake,'max')
ZS_RCP85_2070_pet_max <- exact_extract(RCP85_2070_pet,CoveLake,'max')
#Now extract precip for immediate lake drainage area
ZS_RCP45_2010_pr_dr_max <- exact_extract(RCP45_2010_pr,CoveLakeDrain,'max')
ZS_RCP45_2040_pr_dr_max <- exact_extract(RCP45_2040_pr,CoveLakeDrain,'max')
ZS_RCP45_2070_pr_dr_max <- exact_extract(RCP45_2070_pr,CoveLakeDrain,'max')
ZS_RCP85_2010_pr_dr_max <- exact_extract(RCP85_2010_pr,CoveLakeDrain,'max')
ZS_RCP85_2040_pr_dr_max <- exact_extract(RCP85_2040_pr,CoveLakeDrain,'max')
ZS_RCP85_2070_pr_dr_max <- exact_extract(RCP85_2070_pr,CoveLakeDrain,'max')

#establish dataframe for MACA data
RCP <- c('4.5','4.5','4.5','8.5','8.5','8.5','4.5','4.5','4.5','8.5','8.5','8.5','4.5','4.5','4.5','8.5','8.5','8.5')
YearIntBeg<- c('2010','2040', '2070','2010','2040', '2070','2010','2040', '2070','2010','2040', '2070','2010','2040', '2070','2010','2040', '2070')
Var<- c('Precip','Precip','Precip','Precip','Precip','Precip', 'PET', 'PET', 'PET', 'PET', 'PET', 'PET','Runoff','Runoff','Runoff','Runoff','Runoff','Runoff')
Value<- c(ZS_RCP45_2010_pr,ZS_RCP45_2040_pr,ZS_RCP45_2070_pr,ZS_RCP85_2010_pr,ZS_RCP85_2040_pr,ZS_RCP85_2070_pr,ZS_RCP45_2010_pet,ZS_RCP45_2040_pet,ZS_RCP45_2070_pet,ZS_RCP85_2010_pet,ZS_RCP85_2040_pet,ZS_RCP85_2070_pet,ZS_RCP45_2010_pr_dr*0.3,ZS_RCP45_2040_pr_dr*0.3,ZS_RCP45_2070_pr_dr*0.3,ZS_RCP85_2010_pr_dr*0.3,ZS_RCP85_2040_pr_dr*0.3,ZS_RCP85_2070_pr_dr*0.3)
Value_max<- c(ZS_RCP45_2010_pr_max,ZS_RCP45_2040_pr_max,ZS_RCP45_2070_pr_max,ZS_RCP85_2010_pr_max,ZS_RCP85_2040_pr_max,ZS_RCP85_2070_pr_max,ZS_RCP45_2010_pet_max,ZS_RCP45_2040_pet_max,ZS_RCP45_2070_pet_max,ZS_RCP85_2010_pet_max,ZS_RCP85_2040_pet_max,ZS_RCP85_2070_pet_max,ZS_RCP45_2010_pr_dr_max*0.3,ZS_RCP45_2040_pr_dr_max*0.3,ZS_RCP45_2070_pr_dr_max*0.3,ZS_RCP85_2010_pr_dr_max*0.3,ZS_RCP85_2040_pr_dr_max*0.3,ZS_RCP85_2070_pr_dr_max*0.3)

MACASUM<- data.frame(RCP,YearIntBeg,Var,Value,Value_max)


#Bring in Lake Level Data
LakeLevel<- data.frame(readxl::read_xlsx(paste0(fpath,'Cove Lake Levels.xlsx')))
str(LakeLevel)


#correlate and model lake levels to precip
# cor(LakeLevel$Lake_Level_ft_amsl,LakeLevel$Pr_Gridmet_in_5yr_rollingave)

#plotting lake levels and 5yr cumulative precip
ggplot(LakeLevel, aes(x=LakeLevel$Lake_Level_ft_amsl, y=LakeLevel$Pr_Gridmet_in_5yr_rollingsum))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(LakeLevel, aes(x=LakeLevel$Lake_Level_ft_amsl, y=LakeLevel$Pr_Gridmet_in_yr_sum))+
  geom_point()+
  geom_smooth(method=lm)

ggplot(LakeLevel, aes(x=Lake_Level_ft_amsl, y=PET_Gridmet_in_5yr_rollingsum))+
  geom_point()+
  geom_smooth(method=lm)
#Lets look at the rolliing ave instead since our MACA raster is in average
ggplot(Recent_LakeLevel, aes(x=Lake_Level_ft_amsl, y=PET_Gridmet_in_rollingave))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(Recent_LakeLevel, aes(x=Lake_Level_ft_amsl, y=Pr_Gridmet_in_rollingave))+
  geom_point()+
  geom_smooth(method=lm)

#Simple Multiple linear regression with all entries
# LmLake<- lm(Lake_Level_ft_amsl~Pr_Gridmet_in_yr_sum+PET_Gridmet_in_yr_sum,data=LakeLevel)
# plot(LmLake)
# qqnorm(LmLake)
# summary(LmLake)#does this mean its not normally distributed? Very weird oscellation 

#subset DF to only entries with a corresponging lake level of 4600 or higher and pairs plot
Recent_LakeLevel<-subset.data.frame(LakeLevel,Lake_Level_ft_amsl>4600)
pairs(Recent_LakeLevel[c('Lake_Level_ft_amsl','Pr_Gridmet_in_yr_sum','PET_Gridmet_in_yr_sum', 'PET_Gridmet_in_5yr_rollingsum','PET_Gridmet_in_5yr_rollingsum','Pr_Gridmet_in_rollingave',"PET_Gridmet_in_rollingave","Run_Gridmet_in_rollingave","Rolling_Wtr_Bal")])
#Recent_LakeLevel$WatBal_in_rollingave<-Recent_LakeLevel$Pr_Gridmet_in_rollingave+Recent_LakeLevel$PET_Gridmet_in_rollingave*(-1)+Recent_LakeLevel$Run_Gridmet_in_rollingave


#Now lets try the linear regression and test for correlation
# Lm_PET<- lm(Lake_Level_ft_amsl~PET_Gridmet_in_yr_sum,data=Recent_LakeLevel)
# summary(Lm_PET)
# plot(Lm_PET)
# cor.test(Recent_LakeLevel$Lake_Level_ft_amsl,Recent_LakeLevel$PET_Gridmet_in_yr_sum)
# Lm_Pr<- lm(Lake_Level_ft_amsl~Pr_Gridmet_in_yr_sum,data=Recent_LakeLevel)
# summary(Lm_Pr)
# plot(Lm_Pr)
# cor.test(Recent_LakeLevel$Lake_Level_ft_amsl,Recent_LakeLevel$Pr_Gridmet_in_yr_sum)

# Lm_Pr<- lm(Lake_Level_ft_amsl~Pr_Gridmet_in_rollingave,data=Recent_LakeLevel)
# summary(Lm_Pr)
# plot(Lm_Pr)
# cor.test(Recent_LakeLevel$Lake_Level_ft_amsl,Recent_LakeLevel$Pr_Gridmet_in_yr_sum)
# 
# Lm_PET<- lm(Lake_Level_ft_amsl~PET_Gridmet_in_rollingave,data=Recent_LakeLevel)
# summary(Lm_PET)
# plot(Lm_PET)
# cor.test(Recent_LakeLevel$Lake_Level_ft_amsl,Recent_LakeLevel$PET_Gridmet_in_yr_sum)
# 
# Lm_Run<-lm(Lake_Level_ft_amsl~Run_Gridmet_in_rollingave,data=Recent_LakeLevel)
# summary(Lm_Run)
# plot(Lm_Run)
#Lets test precip and runoff together
Mod_1<- lm(Lake_Level_ft_amsl~WatBal_in_rollingave,data=Recent_LakeLevel)
summary(Mod_1)

Mod_2<- lm(Lake_Level_ft_amsl~Rolling_Wtr_Bal,data=Recent_LakeLevel)
summary(Mod_2)
cor(Recent_LakeLevel$Lake_Level_ft_amsl,Recent_LakeLevel$Rolling_Wtr_Bal)
plot (Mod_2)

#Strong correlation betweet PET and Precip, should not analyze together?
# MLM<- lm(Lake_Level_ft_amsl~PET_Gridmet_in_yr_sum+Pr_Gridmet_in_yr_sum,Recent_LakeLevel)
# summary(MLM)
# cor(Recent_LakeLevel[,4:9])
# cor(Recent_LakeLevel$PET_Gridmet_in_yr_sum,Recent_LakeLevel$Pr_Gridmet_in_yr_sum)
#But with the a yearly average
cor(Recent_LakeLevel[,4:9])
cor(Recent_LakeLevel$PET_Gridmet_in_rollingave,Recent_LakeLevel$Pr_Gridmet_in_rollingave+Recent_LakeLevel$Run_Gridmet_in_rollingave)
#-0.715 compared to -0.72 correlation of the 5 year sum


#Separately, they are well fitting linear regressions
ggplot(data=Recent_LakeLevel, aes(x=PET_Gridmet_in_rollingave,y=Lake_Level_ft_amsl))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(data=Recent_LakeLevel, aes(x=Pr_Gridmet_in_rollingave,y=Lake_Level_ft_amsl))+
  geom_point()+
  geom_smooth(method=lm)
ggplot(data=Recent_LakeLevel, aes(x=Rolling_Wtr_Bal,y=Lake_Level_ft_amsl))+
  geom_point()+
  geom_smooth(method=lm)


#plotting Lake levels with time
ggplot(LakeLevel, aes(x= Date,y= Lake_Level_ft_amsl))+
  geom_point()+
  geom_smooth()

#Model Predictions
 MACAPredict_PET<- subset.data.frame(MACASUM,Var =='PET')
# MACAPredict_PET$Predict<- MACAPredict_PET$Value*26.88 + 4527.51
# 
 MACAPredict_Pr<- subset.data.frame(MACASUM,Var =='Precip')
# MACASUM$Predict<- MACAPredict_Pr$Value*(-555.857) + 4634.725
# 
 MACAPredict_Run<- subset.data.frame(MACASUM,Var =='Runoff')
# MACAPredict_Run$Predict<- MACAPredict_Run$Value*(-1778.962)+4634.65
# MACAPredict<-rbind(MACAPredict_Run,MACAPredict_PET,MACAPredict_Pr)


#Make a new Dataframe with the predicted water levels


YearIntBeg<- c('2010','2040', '2070','2010','2040', '2070')
YearIntBeg<-lubridate::ymd(YearIntBeg,truncated=2L)#Need dates to actually be dates
RCP <- c('4.5','4.5','4.5','8.5','8.5','8.5')
ProjLvl<-c(4627,4627,4627,4627,4627,4627)
Value<-MACAPredict_Pr$Value+MACAPredict_Run$Value-MACAPredict_PET$Value
Value_max<-MACAPredict_Pr$Value_max+MACAPredict_Run$Value_max-MACAPredict_PET$Value_max
Predict_Mod_1<- Value*(-27.71)+4525.55
Predict_Mod_2<- Value*(-28.347)+4529.689
Predict_Mod_2_max<- Value_max*(-28.347)+4529.689
ModelResid<- 4627-Predict_Mod_2
ModelResid_max<- 4627-Predict_Mod_2_max
MACAPredict<-data.frame(RCP,YearIntBeg,Predict_Mod_1,Predict_Mod_2,Predict_Mod_2_max,ProjLvl,ModelResid,ModelResid_max)
str(MACAPredict)

MACASUM

write.csv(MACAPredict,file=paste0(fpath,"MACAPredict.csv"))
write.csv(MACASUM,file=paste0(fpath,"MACASummary.csv"))


#MACA data plotted as points
ggplot(MACASUM, aes(x=YearIntBeg, y=Value, color=Var,shape=RCP))+
         geom_point()+
  labs(x="Projected Year Interval",y="Annual Inches/Year")



ggplot(MACAPredict)+
  geom_col(aes(x=YearIntBeg, y=Predict_Mod_2, group=RCP))
###
###
###
###
# Mod_2<- lm(Lake_Level_ft_amsl~Rolling_Wtr_Bal,data=Recent_LakeLevel)
# summary(Mod_2)
ggplot(data=Recent_LakeLevel,aes(x=Rolling_Wtr_Bal,y=Lake_Level_ft_amsl))+
  geom_point()+
  geom_smooth(method=lm)


Mod_2<- lm(Lake_Level_ft_amsl~Rolling_Wtr_Bal,data=Recent_LakeLevel)
summary(Mod_2)

#Drainarea_sqin<- 38174506*144

citation('sp')
citation('raster')
citation('sf')
citation('rgdal')
citation('terra')
citation('tidyverse')
citation('stats')
citation('lubridate')










