library(ggplot2)
library(Rmisc)

Data_Summary_LMs = read.csv("Summary_LMs.csv",header=T, sep=",", na.strings="NA", dec=".", strip.white=T)
str(Data_Summary_LMs)

# Make Control the first level of the factor Treatment
f <- c("Control","Inactive","Mixed","Active")
Data_Summary_LMs <- within(Data_Summary_LMs, 
                              Treatment_reorder <- factor(Data_Summary_LMs$Treatment,levels=f))
str(Data_Summary_LMs)

######## Quick Graph #####
ggplot(Data_Summary_LMs, aes(x=Treatment_reorder, y=Mean)) + theme_bw() +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), colour="black", width=.1) +
  geom_line() + facet_wrap(~Species) +
  geom_point(size=3)


Summary_Blister_b=subset(Data_Summary_LMs,Species=="Blister beetles")
Summary_Leaf_h=subset(Data_Summary_LMs,Species=="Leaf hoppers")
Summary_Beet_a=subset(Data_Summary_LMs,Species=="Beet armyworms")
Summary_Pea_a=subset(Data_Summary_LMs,Species=="Pea aphids")
Summary_Sharpshooters=subset(Data_Summary_LMs,Species=="Sharpshooters")
Summary_Alfalfa_w=subset(Data_Summary_LMs,Species=="Alfalfa weevils")

a=ggplot(Summary_Blister_b, aes(x=Treatment_reorder, y=Mean)) + theme_bw() +
  ylab("Blister beetle abundance") + xlab("") + scale_y_continuous(limits=c(0,16)) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), colour="black", width=.1, size=0.75) +
  geom_point(size=3, shape=21, fill="white") + 
  geom_hline(aes(yintercept=15), colour="black",size=1) + 
  geom_hline(aes(yintercept=mean_bb), colour="black", linetype="dashed",size=1) +
  scale_x_discrete(breaks=c("Control","Inactive","Mixed","Active"),
                   labels=c("C.", "I.","M.","A.")) +
  theme(axis.title.x  = element_text(size=9), axis.title.y  = element_text(size=12))
  
a


b=ggplot(Summary_Leaf_h, aes(x=Treatment_reorder, y=Mean)) + theme_bw() +
  ylab("Potato leafhopper abundance") + xlab("") + scale_y_continuous(limits=c(0,16)) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), colour="black", width=.1, size=0.75) +
  geom_point(size=3, shape=21, fill="white") + 
  geom_hline(aes(yintercept=15), colour="black", size=1) +
  geom_hline(aes(yintercept=mean_lh), colour="black", linetype="dashed",size=1) +
  scale_x_discrete(breaks=c("Control","Inactive","Mixed","Active"),
                   labels=c("C.", "I.","M.","A.")) +
  theme(axis.title.x  = element_text(size=9), axis.title.y  = element_text(size=12))
b

c=ggplot(Summary_Pea_a, aes(x=Treatment_reorder, y=Mean)) + theme_bw() +
  ylab("Pea aphid abundance") + xlab("")  + scale_y_continuous(limits=c(0,75)) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), colour="black", width=.1, size=0.75) +
  geom_point(size=3, shape=21, fill="white") + 
  geom_hline(aes(yintercept=9), colour="black",size=1) +
  geom_hline(aes(yintercept=mean_pa), colour="black", linetype="dashed",size=1) +
  scale_x_discrete(breaks=c("Control","Inactive","Mixed","Active"),
                   labels=c("C.", "I.","M.","A.")) +
  theme(axis.title.x  = element_text(size=9), axis.title.y  = element_text(size=12))
c

d=ggplot(Summary_Sharpshooters, aes(x=Treatment_reorder, y=Mean)) + theme_bw() +
  ylab("Sharpshooter abundance") + xlab("") + scale_y_continuous(limits=c(0,6)) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), colour="black", width=.1, size=0.75) +
  geom_point(size=3, shape=21, fill="white") + 
  geom_hline(aes(yintercept=5), colour="black", size=1) +
  geom_hline(aes(yintercept=mean_sh), colour="black", linetype="dashed",size=1) +
  scale_x_discrete(breaks=c("Control","Inactive","Mixed","Active"),
                   labels=c("C.", "I.","M.","A."))  +
  theme(axis.title.x  = element_text(size=9), axis.title.y  = element_text(size=12))
d

e=ggplot(Summary_Alfalfa_w, aes(x=Treatment_reorder, y=Mean)) + theme_bw() +
  ylab("Alfalfa weevil abundance") + xlab("") + scale_y_continuous(limits=c(0,10)) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), colour="black", width=.1, size=0.75) +
  geom_point(size=3, shape=21, fill="white") + 
  geom_hline(aes(yintercept=8), colour="black", size=1) +
  geom_hline(aes(yintercept=mean_aw), colour="black", linetype="dashed",size=1) +
  scale_x_discrete(breaks=c("Control","Inactive","Mixed","Active"),
                   labels=c("C.", "I.","M.","A.")) +
  theme(axis.title.x  = element_text(size=9), axis.title.y  = element_text(size=12))
e

f=ggplot(Summary_Beet_a, aes(x=Treatment_reorder, y=Mean)) + theme_bw() +
  ylab("Beet armyworm abundance") + xlab("") + scale_y_continuous(limits=c(0,12)) +
  geom_errorbar(aes(ymin=low_ci, ymax=up_ci), colour="black", width=.1, size=0.75) +
  geom_point(size=3, shape=21, fill="white") + 
  geom_hline(aes(yintercept=10), colour="black", size=1) +
  geom_hline(aes(yintercept=mean_ba), colour="black", linetype="dashed",size=1) +
  scale_x_discrete(breaks=c("Control","Inactive","Mixed","Active"),
                   labels=c("C.", "I.","M.","A.")) +
  theme(axis.title.x  = element_text(size=9), axis.title.y  = element_text(size=12))
f

pdf("Fig1.pdf",width=17.3/2.54,height=17.3/2.54)
multiplot(a,b,c,d,e,f, layout=matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE))
dev.off()

pdf("Fig1.pdf",width=6)
multiplot(a,b,c,d,e,f, layout=matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE))
dev.off()


######## Single prey #####
Data_single_prey_summary = read.csv("Predation_single_prey_summary.csv",header=T, sep=",", na.strings="NA", dec=".", strip.white=T)
str(Data_single_prey_summary)

ggplot(Data_single_prey_summary, aes(x=Prey, y=mean)) + theme_bw() +
  geom_errorbar(aes(ymin=low, ymax=up), colour="black", width=.1) +
  geom_point(size=3)

ggplot(Data_single_prey_summary[Data_single_prey_summary$Model=="Single",], aes(x=Prey, y=mean)) +
  theme_bw() +ylab("Probability of capture") + xlab("Prey species") + 
  scale_y_continuous(limits=c(0,1)) + geom_errorbar(aes(ymin=low, ymax=up), 
                                                    colour="black", width=.1, size=0.75) +
  geom_point(size=3, shape=21, fill="white") + 
  theme(axis.text.x  = element_text(size=8),axis.text.x  = element_text(size=10),
        axis.title.x  = element_text(size=12),axis.title.y  = element_text(size=12))

pdf("Fig1_R2.pdf",width=3,height=3)
ggplot(Data_single_prey_summary[Data_single_prey_summary$Model=="Single",], aes(x=Prey, y=mean)) +
  theme_bw() +ylab("Probability of capture") + xlab("Prey species") + 
  scale_y_continuous(limits=c(0,1)) + geom_errorbar(aes(ymin=low, ymax=up), 
                                                    colour="black", width=.1, size=0.75) +
  scale_x_discrete(breaks=c("Alflalfa weevil","Beet armyworm","Blister beetle","Pea aphid","Potato leafhopper","Sharpshooter"),
                   labels=c("Aw.", "Ba.","Bb.","Pa.","Pl.","Sh.")) +
  geom_point(size=3, shape=21, fill="white") + 
  theme(axis.text.x  = element_text(size=10),axis.text.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),axis.title.y  = element_text(size=12))
dev.off()


##### Supplementary Figures #####
library(doBy)
library(dplyr)
Data_rep_sorted=summaryBy(Activity ~ Pardosa_ID, data = Data_rep, FUN = median)
Data_rep_sorted = arrange(Data_rep_sorted,desc(Activity.median))
f_2 <- c("2","5","18","1","6","16","11","13","4","10","3","7",
         "15","12","9","14","8","17","19")
Data_rep_2=Data_rep
Data_rep_2=within(Data_rep_2, 
                  Pardosa_ID_reorder <- factor(Data_rep_2$Pardosa_ID,levels=f_2))

FigS1a=ggplot(Data_rep_2, aes(x=Pardosa_ID_reorder, y=Activity)) + theme_bw() +
  geom_boxplot() + scale_y_continuous(limits=c(0,30)) +
  xlab("") + ylab("Spider activity (Line crossed)")

FigS1b=ggplot(Data_rep_2, aes(x=Pardosa_ID_reorder, y=Size)) + theme_bw() +
  geom_point() + scale_y_continuous(limits=c(0,2)) +
  xlab("Spider ID") + ylab("Spider body-size (cm)")

multiplot(FigS1a,FigS1b)

Pardosa_mesocosm_activity = read.csv("Pardosa_mesocosm_activity_P_R.csv",header=T, sep=",", na.strings="NA", dec=".", strip.white=T)
str(Pardosa_mesocosm_activity)
#f <- c("Control","Inactive","Mixed","Active")
Pardosa_mesocosm_activity <- within(Pardosa_mesocosm_activity, 
                                    Treatment_reorder <- factor(Pardosa_mesocosm_activity$Treatment,levels=f))
str(Pardosa_mesocosm_activity)

library(reshape2)
Pardosa_mesocosm_activity_full=melt(Pardosa_mesocosm_activity, id.vars=c("Replicate_ID","Treatment","Treatment_reorder",
                                                                         "mean_activity"))
str(Pardosa_mesocosm_activity_full)
FigS2=ggplot(Pardosa_mesocosm_activity, aes(x=Treatment_reorder, y=mean_activity)) + theme_bw() +
  geom_boxplot() + xlab("Mescosm Treatment") + ylab("Average predator activity")

FigS2_2=ggplot(Pardosa_mesocosm_activity_full, aes(x=Treatment_reorder, y=value)) + theme_bw() +
  geom_boxplot() + xlab("") + ylab("Predator activity") +
  scale_y_continuous(limits=c(0,35))

multiplot(FigS2_2,FigS2)

library(lme4)
library(multcomp)
library(effects)
Activity_mesocosm=lmer(value~Treatment_reorder+(1|Replicate_ID),Pardosa_mesocosm_activity_full)
summary(Activity_mesocosm)
anova(Activity_mesocosm)
#drop1(Activity_mesocosm,test = "Chisq")
summary(glht(Activity_mesocosm,mcp(Treatment_reorder="Tukey")))
plot(allEffects(Activity_mesocosm))

Pardosa_cannibalism = read.csv("Pardosa_cannibalism_P_R.csv",header=T, sep=",", na.strings="NA", dec=".", strip.white=T)
str(Pardosa_cannibalism)
f_3 <- c("Inactive","Mixed","Active")
Pardosa_cannibalism_post <- within(Pardosa_cannibalism_post, 
                                    Treatment_reorder <- factor(Pardosa_cannibalism_post$Treatment,levels=f_3))
str(Pardosa_cannibalism_post)


Pardosa_cannibalism_post=Pardosa_cannibalism[Pardosa_cannibalism$Phase=="Post-Treatment",]

Cannibalism_mesocosm=glmer(Pardosa~Treatment*Phase+(1|Replicate_ID),Pardosa_cannibalism,family="poisson")
summary(Cannibalism_mesocosm)
drop1(Cannibalism_mesocosm,test = "Chisq")
summary(glht(Cannibalism_mesocosm,mcp(Treatment_reorder="Tukey")))
plot(allEffects(Cannibalism_mesocosm))

Cannibalism_mesocosm_post=glm(Pardosa~Treatment,Pardosa_cannibalism_post,family="poisson")
summary(Cannibalism_mesocosm_post)
drop1(Cannibalism_mesocosm_post,test = "Chisq")
anova(Cannibalism_mesocosm_post,test = "Chisq")
summary(glht(Cannibalism_mesocosm_post,mcp(Treatment="Tukey")))
plot(allEffects(Cannibalism_mesocosm_post))

FigS3=ggplot(Pardosa_cannibalism_post, aes(x=Treatment_reorder, y=Pardosa)) + theme_bw() +
  geom_boxplot() + xlab("Mescosm Treatment") + ylab("Number of spiders recovered") +
  scale_y_continuous(limits=c(0,10)) + geom_hline(aes(yintercept=8),linetype="dashed",size=1)
