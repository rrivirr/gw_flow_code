library(lmerTest)

library(performance)

extr_lmer<-function(x){
lmer_formula<-as.character(x@call)[2]
pvalue<-anova(x)[[6]]
numDF<-anova(x)[[3]]
denDF<-anova(x)[[4]]
Fvalue<-anova(x)[[5]]
cond_r2<-r2_nakagawa(x)[[1]]
marg_r2<-r2_nakagawa(x)[[2]]

statsret<-data.frame(lmer_formula=lmer_formula,pvalue=pvalue,numDF=numDF,denDF=denDF,Fvalue=Fvalue,cond_r2=cond_r2,marg_r2=marg_r2)
return(statsret)
}




lmer_doc_wetland_surfacearea <- lmer(log10(mgL_C.mean_na)~log10(surface_area_ha)+(1|Site_2), data=ldu)
summary(lmer_doc_wetland_surfacearea)
plot(lmer_doc_wetland_surfacearea)
anova(lmer_doc_wetland_surfacearea)
lmer_doc_surfacearea_extr<-extr_lmer(lmer_doc_wetland_surfacearea)