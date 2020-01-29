rm(list=ls())
##### Data Import and transformation ####
Data_rep = read.csv("Repeatability_long_P_R.csv",header=T, sep=",", na.strings="NA", dec=".", strip.white=T)
str(Data_rep)
Data_rep$Pardosa_ID = as.factor(Data_rep$Pardosa_ID)

Data_community_wide=read.csv("Community_effects_wide_P_R.csv",header=T, sep=",", na.strings="NA", dec=".", strip.white=T)
str(Data_community_wide)
Data_community_wide$Trt_Phase=paste(Data_community_wide$Treatment,Data_community_wide$Phase,sep="_")
Data_community_wide$Trt_Phase=factor(Data_community_wide$Trt_Phase)
str(Data_community_wide)
Data_community_wide$Replicate.ID=factor(Data_community_wide$Replicate.ID)

# Make Control the first level of the factor Treatment
f <- c("Control","Inactive","Mixed","Active")
Data_community_wide <- within(Data_community_wide, 
                              Treatment_reorder <- factor(Data_community_wide$Treatment,levels=f))
str(Data_community_wide)

# Make Control_Initial the first level of the factor Trt_Phase
f2 <- c("Control_Initial","Inactive_Initial","Mixed_Initial","Active_Initial",
        "Control_Post-Treatment","Inactive_Post-Treatment","Mixed_Post-Treatment","Active_Post-Treatment")
Data_community_wide <- within(Data_community_wide_PT, 
                              Trt_Phase_reorder <- factor(Data_community_wide_PT$Trt_Phase,levels=f2))
str(Data_community_wide)


# Post-Treatment dataset
Data_community_wide_PT=subset(Data_community_wide,Phase=="Post-Treatment")

