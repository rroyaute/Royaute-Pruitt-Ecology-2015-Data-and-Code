###### Analysis Community ####
library(effects)
library(coefplot2)
library(lattice)
library(multcomp)


###### barplots #####
bwplot(Blister_beetle~Treatment_reorder|Phase,Data_community_wide)
bwplot(Leaf_hoppers~Treatment_reorder|Phase,Data_community_wide)
bwplot(Beet_armyworm~Treatment_reorder|Phase,Data_community_wide)
bwplot(Pea_aphid~Treatment_reorder|Phase,Data_community_wide)
bwplot(Sharpshooter~Treatment_reorder|Phase,Data_community_wide)
bwplot(Alfalfa_weevil~Treatment_reorder|Phase,Data_community_wide)

#### Activity per treatment ####
Activity_comm=lm(mean_activity~Treatment_reorder,data=na.omit(Data_community_wide_PT))
summary(Activity_comm);plot(allEffects(Activity_comm))

##### Herbivore abundance vs mean Pardosa activity ####
# Simple linear models
Blister_b=lm(sqrt(Blister_beetle)~Treatment_reorder,data=Data_community_wide_PT)
summary(Blister_b);anova(Blister_b);plot(allEffects(Blister_b))
shapiro.test(residuals(Blister_b));hist(residuals(Blister_b))

Leaf_h=lm(sqrt(Leaf_hoppers)~Treatment_reorder,data=Data_community_wide_PT)
summary(Leaf_h);anova(Leaf_h);plot(allEffects(Leaf_h))
shapiro.test(residuals(Leaf_h));hist(residuals(Leaf_h))

Beet_a=lm(sqrt(Beet_armyworm)~Treatment_reorder,data=Data_community_wide_PT)
summary(Beet_a);anova(Beet_a);plot(allEffects(Beet_a))
shapiro.test(residuals(Beet_a));hist(residuals(Beet_a))

Pea_a=lm(sqrt(Pea_aphid)~Treatment_reorder,data=Data_community_wide_PT)
summary(Pea_a);anova(Pea_a);plot(allEffects(Pea_a))
shapiro.test(residuals(Pea_a));hist(residuals(Pea_a))

Sharpshooter=lm(sqrt(Sharpshooter)~Treatment_reorder,data=Data_community_wide_PT)
summary(Sharpshooter);anova(Sharpshooter);plot(allEffects(Sharpshooter))
shapiro.test(residuals(Sharpshooter));hist(residuals(Sharpshooter))

Alfalfa_w=lm(sqrt(Alfalfa_weevil)~Treatment_reorder,data=Data_community_wide_PT)
summary(Alfalfa_w);anova(Alfalfa_w);plot(allEffects(Alfalfa_w))
shapiro.test(residuals(Alfalfa_w));hist(residuals(Alfalfa_w))


# Residuals are not always-normal, retry with GLM and Poisson error
Blister_b=glm(Blister_beetle~Treatment_reorder,data=Data_community_wide_PT,family="poisson")
summary(Blister_b);anova(Blister_b);plot(allEffects(Blister_b))
drop1(Blister_b,test="Chisq");hist(residuals(Blister_b))
summary(glht(Blister_b,mcp(Treatment_reorder="Tukey")))
confint(Blister_b)
coefplot2(Blister_b)

Leaf_h=glm(Leaf_hoppers~Treatment_reorder,data=Data_community_wide_PT,family="poisson")
summary(Leaf_h);anova(Leaf_h);plot(allEffects(Leaf_h))
drop1(Leaf_h,test="Chisq");hist(residuals(Leaf_h))
summary(glht(Leaf_h,mcp(Treatment_reorder="Tukey")))
coefplot2(Leaf_h)

Beet_a=glm(Beet_armyworm~Treatment_reorder,data=Data_community_wide_PT,family="poisson")
summary(Beet_a);anova(Beet_a);plot(allEffects(Beet_a))
drop1(Beet_a,test="Chisq");hist(residuals(Beet_a))
summary(glht(Beet_a,mcp(Treatment_reorder="Tukey")))
coefplot2(Beet_a)

Pea_a=glm(Pea_aphid~Treatment_reorder,data=Data_community_wide_PT,family="poisson")
summary(Pea_a);anova(Pea_a);plot(allEffects(Pea_a))
drop1(Pea_a,test="Chisq");hist(residuals(Pea_a))
summary(glht(Pea_a,mcp(Treatment_reorder="Tukey")))
coefplot2(Pea_a)

Sharpshooter=glm(Sharpshooter~Treatment_reorder,data=Data_community_wide_PT,family="poisson")
summary(Sharpshooter);anova(Sharpshooter);plot(allEffects(Sharpshooter))
drop1(Sharpshooter,test="Chisq");hist(residuals(Sharpshooter))
summary(glht(Sharpshooter,mcp(Treatment_reorder="Tukey")))
coefplot2(Sharpshooter)

Alfalfa_w=glm(Alfalfa_weevil~Treatment_reorder,data=Data_community_wide_PT,family="poisson")
summary(Alfalfa_w);anova(Alfalfa_w);plot(allEffects(Alfalfa_w))
drop1(Alfalfa_w,test="Chisq");hist(residuals(Alfalfa_w))
summary(glht(Alfalfa_w,mcp(Treatment_reorder="Tukey")))
coefplot2(Alfalfa_w)

predict(Alfalfa_w)

#extract predicted values
Blister_b_effects=allEffects(Blister_b)
Leaf_h_effects=allEffects(Leaf_h)
Beet_a_effects=allEffects(Beet_a)
Pea_a_effects=allEffects(Pea_a)
Sharpshooter_effects=allEffects(Sharpshooter)
Alfalfa_w_effects=allEffects(Alfalfa_w)

summary(Blister_b_effects)
summary(Leaf_h_effects)
summary(Beet_a_effects)
summary(Pea_a_effects)
summary(Sharpshooter_effects)
summary(Alfalfa_w_effects)

##### Departure from expected preformance ####
Blister_beetle=Data_community_wide_PT[,c("Replicate.ID","Treatment","Treatment_reorder","Blister_beetle")]
Leaf_hoppers=Data_community_wide_PT[,c("Replicate.ID","Treatment","Treatment_reorder","Leaf_hoppers")]
Beet_armyworm=Data_community_wide_PT[,c("Replicate.ID","Treatment","Treatment_reorder","Beet_armyworm")]
Pea_aphid=Data_community_wide_PT[,c("Replicate.ID","Treatment","Treatment_reorder","Pea_aphid")]
Sharpshooter=Data_community_wide_PT[,c("Replicate.ID","Treatment","Treatment_reorder","Sharpshooter")]
Alfalfa_weevil=Data_community_wide_PT[,c("Replicate.ID","Treatment","Treatment_reorder","Alfalfa_weevil")]

# Blister beetles
mixed_bb=subset(Blister_beetle,Treatment_reorder=="Mixed")
active_bb=subset(Blister_beetle,Treatment_reorder=="Active")
inactive_bb=subset(Blister_beetle,Treatment_reorder=="Inactive")
mean_bb=mean(c(active_bb$Blister_beetle,inactive_bb$Blister_beetle))
mean(mixed_bb$Blister_beetle-mean_bb) # -3.866667 * Significant

# Leaf hoppers
mixed_lh=subset(Leaf_hoppers,Treatment_reorder=="Mixed")
active_lh=subset(Leaf_hoppers,Treatment_reorder=="Active")
inactive_lh=subset(Leaf_hoppers,Treatment_reorder=="Inactive")
mean_lh=mean(c(active_lh$Leaf_hoppers,inactive_lh$Leaf_hoppers))
mean(mixed_lh$Leaf_hoppers-mean_lh) # -1.266667 NS

# Beet armyworms
mixed_ba=subset(Beet_armyworm,Treatment_reorder=="Mixed")
active_ba=subset(Beet_armyworm,Treatment_reorder=="Active")
inactive_ba=subset(Beet_armyworm,Treatment_reorder=="Inactive")
mean_ba=mean(c(active_ba$Beet_armyworm,inactive_ba$Beet_armyworm))
mean(mixed_ba$Beet_armyworm-mean_ba) # -0.3333333 NS

# Pea aphids
mixed_pa=subset(Pea_aphid,Treatment_reorder=="Mixed")
active_pa=subset(Pea_aphid,Treatment_reorder=="Active")
inactive_pa=subset(Pea_aphid,Treatment_reorder=="Inactive")
mean_pa=mean(c(active_pa$Pea_aphid,inactive_pa$Pea_aphid))
mean(mixed_pa$Pea_aphid-mean_pa) # 1.233333 NS

# Sharpshooter
mixed_sh=subset(Sharpshooter,Treatment_reorder=="Mixed")
active_sh=subset(Sharpshooter,Treatment_reorder=="Active")
inactive_sh=subset(Sharpshooter,Treatment_reorder=="Inactive")
mean_sh=mean(c(active_sh$Sharpshooter,inactive_sh$Sharpshooter))
mean(mixed_sh$Sharpshooter-mean_sh) # -1.3 * Significant

# Alfalfa_weevil
mixed_aw=subset(Alfalfa_weevil,Treatment_reorder=="Mixed")
active_aw=subset(Alfalfa_weevil,Treatment_reorder=="Active")
inactive_aw=subset(Alfalfa_weevil,Treatment_reorder=="Inactive")
mean_aw=mean(c(active_aw$Alfalfa_weevil,inactive_aw$Alfalfa_weevil))
mean(mixed_aw$Alfalfa_weevil-mean_aw) # -2.6 * Significant


