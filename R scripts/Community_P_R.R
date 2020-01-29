######## Ordination #####
library(vegan)

######## Ordination no initial phase #####
Data_community_herb=Data_community_wide_PT[,14:19]
str(Data_community_wide_PT)
trt=droplevels(Data_community_wide_PT[,2])
trt_reorder=droplevels(Data_community_wide_PT[,21])

Data_community_herb_log=log10(Data_community_herb+1)
community_bc=vegdist(Data_community_herb_log)
community.MDS <- metaMDS(Data_community_herb_log, trymax=50, trace=FALSE)
community.MDS
community.MDS.6d <- metaMDS(Data_community_herb_log, trymax=50, trace=FALSE,k=6)
community.MDS.6d
community.MDS.fit=envfit(community.MDS,Data_community_herb_log,permute=1000)

plot(community.MDS,display=c("sites"),type="t")
plot(community.MDS.fit)


plot(community.MDS,display="species",type="t")
plot(community.MDS_post,display="sites")
plot(community.MDS_post,display="species")

NMDS_results=data.frame(community.MDS$points)
NMDS_results$Treatment=trt_reorder
str(NMDS_results)

NMDS_results_6d=data.frame(community.MDS.6d$points)
NMDS_results_6d$Treatment=trt_reorder
str(NMDS_results_6d)

library(ggplot2)
library(devtools)
library(digest)
source_url("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R")    

Fig3=ggplot(NMDS_results, aes(x=MDS1, y=MDS2,shape=Treatment)) + geom_point(size=2)+
  theme_bw() + scale_shape_manual(values=c(1,2,15,16)) + 
  stat_ellipse(fill="grey") + xlab("NMDS1") + ylab("NMDS2") +
  theme(legend.title=element_blank(),legend.position=c(.9, .8),
        legend.text = element_text(size = 12))
Fig3

#+stat_ellipse(fill="grey")

ggplot(NMDS_results_6d, aes(x=MDS1, y=MDS2,shape=Treatment)) + geom_point(size=5)+
  theme_bw() + scale_shape_manual(values=c(1,2,15,16)) + stat_ellipse(fill="grey")


community.ad <- adonis(Data_community_herb_log~trt_reorder)
community.ad

community.bd <- betadisper(community_bc,trt_reorder)
anova(community.bd)
permutest(community.bd)
TukeyHSD(community.bd)
plot(community.bd)

#Fig2b
boxplot(community.bd)

# Posthoc test
TukeyHSD(community.bd)

