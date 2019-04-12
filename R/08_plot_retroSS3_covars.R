################################################################################
#  IBpil: look at retrospective estimates from ss32flbeia                     # 
#------------------------------------------------------------------------------#
#   Leire Citores (AZTI-Tecnalia)                                              #
#   created:  12/04/2019                                                       #
#                                                                              #
################################################################################


wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
setwd(wd)

# directory with results
res.dir  <- file.path("./temporal_output")

setwd(res.dir)
load("scenario_list.RData")
setwd(paste0(res.dir,"/output_iters"))


ni<-100
files<-list.files()

#select scenario j
j<-1
sc<-scenario_list[j]
#sc<-"ASSss3_HCR1d_BH04_RECmed_INNvar_OERnaq"

##read invidial runs
files_sc<-files[grep(sc, files)]
ni<-min(ni,length(files_sc))
load(files_sc[1])
covars<-get(sc)$covars
covars_all<-lapply(covars, expand,iter=1:ni)
covars_all$real_ssb<-covars_all$ssb*NA
covars_all$real_rec<-covars_all$rec*NA
covars_all$indices<-covars_all$qs*NA


for(i in 1:ni){
  print(i)
  #load(paste0(paste("out2018",sc,i,sep="_"),".RData"))
  load(files_sc[i])
  covars<-get(sc)$covars
  covars_all$ssb[,,,,,i]<-covars$ssb[,,,,,1]
  covars_all$rec[,,,,,i]<-covars$rec[,,,,,1]
  covars_all$fbar[,,,,,i]<-covars$fbar[,,,,,1]
  covars_all$sel[,,,,,i]<-covars$sel[,,,,,1]
  covars_all$qs[,,,,,i]<-covars$qs[,,,,,1]
  covars_all$conv[,,,,,i]<-covars$conv[,,,,,1]
  covars_all$real_ssb[,,,,,i]<-ssb(get(sc)$biols$PIL)
  covars_all$real_rec[,,,,,i]<-get(sc)$biols$PIL@n[1,]
  covars_all$ssb[ac(2048),,,,,i]<-ssb(get(sc)$biols$PIL)
  covars_all$rec[ac(2048),,,,,i]<-get(sc)$biols$PIL@n[1,]
  covars_all$indices[1,,,,,i]<-quantSums(get(sc)$indices$PIL$AcousticNumberAtAge@index)
  covars_all$indices[2,,,,,i]<-get(sc)$indices$PIL$DEPM@index
}


pdf(paste0(res.dir,"/",sc,ni,"its.pdf"),paper="a4r")
ggplot(as.data.frame(covars_all$ssb[,,,,,]/covars_all$real_ssb[,,,,,]),aes(year,data))+facet_wrap(~eval.year)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.2,col=NA) +
  stat_summary(fun.y = median,
               geom = "line",size=1)+
  geom_hline(yintercept = 1)+geom_vline(xintercept = 2018)+
  ggtitle(sc)+ylim(0,2.5)+ylab("estim ssb/real ssb")

ggplot(as.data.frame(covars_all$ssb[,,,,,]/covars_all$real_ssb[,,,,,]),aes(year,data))+facet_wrap(~eval.year)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.2,col=NA) +
  stat_summary(fun.y = median,
               geom = "line",size=1)+
  geom_hline(yintercept = 1)+geom_vline(xintercept = 2018)+
  ggtitle(sc)+ylim(0.7,1.3)+ylab("estim ssb/real ssb")


ggplot(as.data.frame(covars_all$rec[,,,,,]/covars_all$real_rec[,,,,,]),aes(year,data))+facet_wrap(~eval.year)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.2,col=NA) +
  stat_summary(fun.y = median,
               geom = "line",size=1)+
  geom_hline(yintercept = 1)+geom_vline(xintercept = 2018)+ylim(0,2)+
  ggtitle(sc)+ylab("estim rec/real rec")

ggplot(as.data.frame(covars_all$rec[,,,,,]/covars_all$real_rec[,,,,,]),aes(year,data))+facet_wrap(~eval.year)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.2,col=NA) +
  stat_summary(fun.y = median,
               geom = "line",size=1)+
  geom_hline(yintercept = 1)+geom_vline(xintercept = 2018)+ylim(0.7,1.3)+
  ggtitle(sc)+ylab("estim rec/real rec")

ggplot(as.data.frame(covars_all$qs[,,,,,]),aes(year,data,col=factor(qs),fill=factor(qs)))+#facet_wrap(~eval.year)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.1,col=NA) +
  stat_summary(fun.y = median,
               geom = "line",size=1)+
  geom_hline(yintercept = 1)+geom_vline(xintercept = 2018)+
  ggtitle(sc)+ylab("qs")

ggplot(as.data.frame((covars_all$indices[,,,,,])),aes(year,data,col=factor(qs),fill=factor(qs)))+facet_wrap(~qs,scales="free")+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.1,col=NA) +
  stat_summary(fun.y = median,
               geom = "line",size=1)+
  geom_hline(yintercept = 1)+geom_vline(xintercept = 2018)+ggtitle(sc)+
  ylab("generated indices")


ggplot(as.data.frame(covars_all$sel[,,,,,]),aes(age,data,col=factor(year),fill=factor(year)))+facet_wrap(~unit)+
  stat_summary(fun.y = median,
               fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.1,col=NA) +
  stat_summary(fun.y = median,
               geom = "line",size=1)+  ggtitle(sc)+ylab("selectivities by block")

dev.off()
