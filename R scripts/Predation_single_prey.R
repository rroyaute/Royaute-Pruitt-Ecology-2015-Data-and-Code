Data_p_single = read.csv("Pardosa_predation_single.csv",header=T, sep=",", na.strings="NA", dec=".", strip.white=T)
str(Data_p_single)

library(effects)
library(multcomp)
Predation_single=glm(Capture~Activity*Prey,Data_p_single,family="binomial")
summary(Predation_single)
anova(Predation_single)
step(Predation_single)
drop1(Predation_single,test="Chisq")
plot(allEffects(Predation_single))
Predation_single.1=glm(Capture~Activity+Prey,Data_p_single,family="binomial")
summary(Predation_single.1)
drop1(Predation_single.1,test="Chisq")
plot(allEffects(Predation_single.1))
summary(glht(Predation_single,mcp(Prey="Tukey")))
summary(glht(Predation_single.1,mcp(Prey="Tukey")))

summary(effect("Prey",Predation_single))
summary(effect("Prey",Predation_single.1))



summary(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Alfalfa_weevil",],family="binomial"))
plot(allEffects(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Alfalfa_weevil",],family="binomial")))

summary(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Beet_armyworm",],family="binomial"))
plot(allEffects(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Beet_armyworm",],family="binomial")))

summary(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Blister_beetle",],family="binomial"))
plot(allEffects(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Blister_beetle",],family="binomial")))

summary(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Leaf_hopper",],family="binomial"))
plot(allEffects(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Leaf_hopper",],family="binomial")))

summary(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Pea_aphid",],family="binomial"))
plot(allEffects(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Pea_aphid",],family="binomial")))

summary(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Sharpshooter",],family="binomial"))
plot(allEffects(glm(Capture~Activity,Data_p_single[Data_p_single$Prey=="Sharpshooter",],family="binomial")))


library(ggplot2)
ggplot(Data_p_single, aes(x=Prey, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method=glm,family=binomial,se=T,color="black")


ggplot(Data_p_single, aes(x=Activity, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method=glm,family=binomial,se=T,color="black") + facet_wrap(~Prey)


ggplot(Data_p_single[Data_p_single$Prey=="Alfalfa_weevil",], 
       aes(x=Activity, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method="lm")

ggplot(Data_p_single[Data_p_single$Prey=="Beet_armyworm",], 
       aes(x=Activity, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method="lm")

ggplot(Data_p_single[Data_p_single$Prey=="Blister_beetle",], 
       aes(x=Activity, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method="lm")

ggplot(Data_p_single[Data_p_single$Prey=="Leaf_hopper",], 
       aes(x=Activity, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method="lm")

ggplot(Data_p_single[Data_p_single$Prey=="Pea_aphid",], 
       aes(x=Activity, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method="lm")

ggplot(Data_p_single[Data_p_single$Prey=="Sharpshooter",], 
       aes(x=Activity, y=Capture)) + theme_bw() +
  geom_point(size=3) + geom_smooth(method="lm")
