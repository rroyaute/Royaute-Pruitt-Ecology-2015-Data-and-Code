library(lme4)
library(lmerTest)


Activity_rep=lmer(scale(Activity)~Measurement+Size+(1|Pardosa_ID),data=Data_rep) 
Activity_rep_rdslope=lmer(scale(Activity)~Measurement+Size+(Measurement|Pardosa_ID),data=Data_rep)
summary(Activity_rep);summary(Activity_rep_rdslope)
anova(Activity_rep,Activity_rep_rdslope)
drop1(Activity_rep,test="Chisq")
confint_Act=confint(Activity_rep,method = "boot",nsim=1000)
confint(Activity_rep,method = "profile")
confint(Activity_rep,nsim=1000)
shapiro.test(residuals(Activity_rep))
# R = 0.3773/(0.3773+0.6638)=0.3624051
# CI_low = 0.36660499/(0.36660499+0.83922394) = 0.3040274
# CI_up = 0.71137219/(0.71137219+0.91267111) = 0.4380254

Activity_rep.poisson=glmer(Activity~Measurement+(1|Pardosa_ID),
                           data=Data_rep,family=poisson,control=glmerControl(optimizer="bobyqa")) 
summary(Activity_rep.poisson)
confint(Activity_rep.poisson,method = "boot",nsim=1000)
# R = 0.07463/(0.07463+log(1/(2.575168))+1)=0.5798071


###### Repeatability Analysis MCMC ####
detach(package:lme4)
library(MCMCglmm)

univ.1v.1rd<- list(R=list(V=1, nu= 0.002),
                   G=list(G1=list(V=1, nu=0.002)))
#univ.1v.1rd.2<- list(R=list(V=1/2, nu= 0.002),
#                        G=list(G1=list(V=1/2, nu=0.002)))


Activity_rep_MCMC=MCMCglmm(sqrt(Activity)~Measurement,random=~Pardosa_ID,
                   data=Data_rep,nitt=1300000, thin=1000, burnin=300000,
                   verbose=F,prior=univ.1v.1rd)
summary(Activity_rep_MCMC);diag(autocorr(Activity_rep_MCMC$Sol)[2, , ]);diag(autocorr(Activity_rep_MCMC$VCV)[2, , ])
posterior.mode(Activity_rep_MCMC$VCV)
posterior.mode(Activity_rep_MCMC$VCV[,1]/(Activity_rep_MCMC$VCV[,1]+Activity_rep_MCMC$VCV[,2]))
HPDinterval(Activity_rep_MCMC$VCV[,1]/(Activity_rep_MCMC$VCV[,1]+Activity_rep_MCMC$VCV[,2]))
plot(Activity_rep_MCMC$VCV[,1]/(Activity_rep_MCMC$VCV[,1]+Activity_rep_MCMC$VCV[,2]))
# R = 0.349524 [0.1791415; 0.5514155]

Activity_rep_MCMC.2=MCMCglmm(scale(Activity)~Measurement,random=~Pardosa_ID,
                           data=Data_rep,nitt=1300000, thin=1000, burnin=300000,
                           verbose=F,prior=univ.1v.1rd)
summary(Activity_rep_MCMC.2);diag(autocorr(Activity_rep_MCMC.2$Sol)[2, , ]);diag(autocorr(Activity_rep_MCMC.2$VCV)[2, , ])
posterior.mode(Activity_rep_MCMC.2$VCV[,1]/(Activity_rep_MCMC.2$VCV[,1]+Activity_rep_MCMC.2$VCV[,2]))
HPDinterval(Activity_rep_MCMC.2$VCV[,1]/(Activity_rep_MCMC.2$VCV[,1]+Activity_rep_MCMC.2$VCV[,2]))
plot(Activity_rep_MCMC.2$VCV[,1]/(Activity_rep_MCMC.2$VCV[,1]+Activity_rep_MCMC.2$VCV[,2]))


detach(package:MCMCglmm)

