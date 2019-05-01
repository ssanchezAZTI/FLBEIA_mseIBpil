#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "~/Documents/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/" # main directory
setwd(wd)

# directory with results
res.dir  <- file.path("./output")
# directory with plots
plot.dir <- file.path(res.dir,"plots")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

library(r4ss)
library(reshape2)

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios

load(file.path(res.dir, "scenario_list.RData"))
length(scenario_list)



theme_set(
  theme_light()+
    theme(
      axis.text=element_text(size=10),
      axis.text.x = element_text(angle=90,vjust=1),
      axis.title=element_text(size=14,face="bold"),
      legend.text=element_text(size=14),
      text = element_text(size = 14),
      plot.background =	element_rect(colour = NA, fill = NA),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position="bottom"
    )
)

# read data

dfyr <- read.table(file.path(res.dir,"stats_byyr2018.csv"), header=T, sep=";")
#select only variables of interest
dfyr <- dfyr[,c(1:7,32:37,23:25,11:13)]
#change units of variables of interest
dfyr[,c(8:10)] <- dfyr[,c(8:10)]/1000000
dfyr[,c(11:13)] <- dfyr[,c(11:13)]/1000
dfyr[,c(17:19)] <- dfyr[,c(17:19)]/1000

# reshape data 
dfyr%>%
  gather("var_q", "value", -c(1:7)) %>%
  separate("var_q", into = c("indicator", "quantile"), sep = "_") %>%
  spread("quantile", "value") %>% as.data.frame -> dfyr


# data.frame for differences between runs with assessment and observation error with
# runs without assessment and no observation error

dfyr %>%
  dplyr::select(-scenario, -q05, -q95,-obsErr) %>%
  spread(assessment, q50) %>%
  mutate(ss3_relbias = (ss3 - none) / none) %>%
  as.data.frame -> relbias

# subset data.frame for variables of interest
relbias <- subset(relbias, indicator %in% c("ssb","f"))

##PLOTS for ASSss3 comparison with ASSnone
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+",'1'="HCR1",'2'="HCR2",'3'="HCR3",'4'="HCR4",'5'="HCR5",'6'="HCR6",'7'="HCR7")
#make sure order of variables is ssb and then f
relbias$indicator2 <- factor(relbias$indicator, levels=c("ssb","f"))

###rule 1 and 2
aux.rule <- subset(relbias, rule %in% c(1:2))
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

p <- ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = rec)) +
        geom_line() + ylab("ASSss3 relative to ASSnone")+
        scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
        facet_grid(indicator2~rule,scales="free",labeller = as_labeller(facet_names))+
        scale_colour_discrete(name="Recruitment",breaks=c("med", "low", "lowmed","mix"),
                      labels=c("Med", "Low", "LowMed","Mix"))
ggsave(paste0("HCR12rel.pdf"))

###rule 3 and 4
aux.rule <- subset(relbias, rule %in% c(3:4))
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

p <- ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
  facet_grid(indicator2~rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("med", "low", "lowmed","mix"),
                        labels=c("Med", "Low", "LowMed","Mix"))
ggsave(paste0("HCR34rel.pdf"))

###rule 5 and 6
aux.rule <- subset(relbias, rule %in% c(5:6))
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

p <- ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
  facet_grid(indicator2~rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("med", "low", "lowmed","mix"),
                        labels=c("Med", "Low", "LowMed","Mix"))
ggsave(paste0("HCR56rel.pdf"))

# ############
# # Estimate 90% confidence interval for historical assessment
# ############
# 
# dir2018 <- c("~/Documents/IPMA/SARDINE/WGHANSA2018/SS3/retrospective/retro_2018/")
# 
# 
# #read ouputs and create database for ggplots
#   mydir <- dir2018
#   
#   setwd(mydir)
#   
#   out.ss3<-SS_output(mydir,forecast=FALSE,ncols=45)
#   dados<-SS_readdat(paste(mydir,"data.ss_new",sep=""), verbose = TRUE, echoall = FALSE, section = NULL,version = "3.30")
#   
#   selectivity<-subset(out.ss3$ageselex[out.ss3$ageselex$Fleet==1 & out.ss3$ageselex$Factor=="Asel2" & out.ss3$ageselex$Yr %in% c(out.ss3$startyr:(out.ss3$endyr)),c("Yr","0","1","2","3","4","5","6") ])
#   
#   colnames(selectivity)[1]<-"year"
#   
#   sel<-melt(selectivity,id.vars='year')
#   sel_bar<-rowMeans(selectivity[,4:7])
#   
#   startyr <- out.ss3$startyr
#   endyr <- out.ss3$endyr
#   aux <- out.ss3$derived_quants
#   idx <- match(paste("F_",startyr:(endyr-1),sep=""), aux[,1])
#   aux <- aux[idx, ]
# 
#   Fbar.dat <- data.frame(Year=startyr:(endyr-1),
#                          Value=aux$Value*sel_bar[-length(sel_bar)],
#                          StdDev=aux$StdDev,
#                          Lower=(aux$Value-1.645*aux$StdDev)*sel_bar[-length(sel_bar)],
#                          Upper=(aux$Value+1.645*aux$StdDev)*sel_bar[-length(sel_bar)],
#                          sel_bar=sel_bar[-length(sel_bar)],
#                          mean=mean(aux$Value),
#                          param="f")
#   
# 
#   aux <- out.ss3$derived_quants
#   idx <- match(paste("Recr_",startyr:(endyr-1),sep=""), aux[,1])
#   aux <- aux[idx, ] 
#   rec.dat <- data.frame(Year=startyr:(endyr-1), 
#                         Value=aux$Value/1000000,
#                         StdDev=aux$StdDev/1000000,
#                         Lower=(aux$Value-1.645*aux$StdDev)/1000000,
#                         Upper=(aux$Value+1.645*aux$StdDev)/1000000,
#                         sel_bar=sel_bar[-length(sel_bar)],
#                         mean=mean(aux$Value/1000000),
#                         param="rec"
#   )
# 
#   #In our case we use Biomass1+ and not SSB.
#   #So we need to get values from the timeseries data.frame
#   #And use the StDev from the SSB since the model does not calculate the StDev from the Biomass1+
#   #aux <- out.ss3$derived_quants
#   #idx <- match(paste("SSB_",startyr:endyr,sep=""), aux[,1])
#   #aux <- aux[idx, ] 
#   aux <- subset(out.ss3$timeseries, Era=="TIME",c("Yr","Bio_smry"))
#   idx <- grep("SSB_\\d",out.ss3$derived_quants$Label)
#   cv <- data.frame(cv=out.ss3$derived_quants[idx,"StdDev"]/out.ss3$derived_quants[idx,"Value"])
#   cv$Yr <- out.ss3$startyr:out.ss3$endyr
#   aux <- merge(aux,cv,by="Yr")
#   bio.dat <- data.frame(Year=startyr:endyr, 
#                         #Value=aux$Value,
#                         Value=aux$Bio_smry/1000,
#                         StdDev=(aux$cv*aux$Bio_smry)/1000,
#                         Lower=(aux$Bio_smry-1.645*aux$cv*aux$Bio_smry)/1000,
#                         Upper=(aux$Bio_smry+1.645*aux$cv*aux$Bio_smry)/1000,
#                         #StdDev=aux$StdDev,
#                         #Lower=aux$Value-2*aux$StdDev,
#                         #Upper=aux$Value+2*aux$StdDev,
#                         sel_bar=sel_bar,
#                         mean=mean(aux$Bio_smry/1000),
#                         param="ssb"
#   )
# 
#   landings <- dados$catch[dados$catch$year %in% c(startyr:(endyr-1)),"purse_seine"]/1000
#   
#   catch.dat <- data.frame(Year=startyr:(endyr-1), 
#                           Value=landings,
#                           StdDev=landings,
#                           Lower=landings,
#                           Upper=landings,
#                           sel_bar=sel_bar[-length(sel_bar)],
#                           mean=mean(landings),
#                           param="catch"
# )
# 
#   all<-rbind(rec.dat,bio.dat,Fbar.dat,catch.dat)
#   all <- all[,-c(3,6:7)]
#  names(all) <- c("year","q50","q95","q05","indicator")
#  all <- subset(all,year %in% c(1978:2017))
# 
# save(all,file="ASSss3_historicalData.RData")
# rm(aux,bio.dat,cv,Fbar.dat,rec.dat,catch.dat,sel,selectivity,endyr,idx,mydir,out.ss3,sel_bar,startyr,dir2018,dados)

#get data from historical assessment
load("~/Documents/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/data/ASSss3_historicalData.RData")

##make data.frame joining historical data with projection data 
dfyr %>%
  filter(year == 2018) %>%
  dplyr::select(-year, -q05, -q50, -q95) %>%
  left_join(all) %>% as.data.frame -> tt

dfyr %>%
  bind_rows(
    tt
  )%>% as.data.frame -> ww

ww <- subset(ww,assessment=="ss3")
ww$indicator2 <- factor(ww$indicator, levels=c("rec","ssb","f","catch"))

#Data from two iters
out.all <- NULL
for (cs in scenario_list){
  
  file.dat <- file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  aux <- subset(aux, iter %in% c(45,235))
  aux <- subset(aux, year>2017)
  
  out.all <- rbind(out.all, aux)
  
}

scenario   <- as.character(out.all$scenario)
assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
rec_sc        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))

out.iters <- cbind( assessment, rule, rec_sc, initNage, obsErr, out.all)
#change units of variables of interest
out.iters[,15] <- out.iters[,15]/1000000
out.iters[,11] <- out.iters[,11]/1000
out.iters[,16] <- out.iters[,16]/1000

# reshape data 
out.iters <- melt(out.iters,id=1:9,variable.name = "indicator")
out.iters <- subset(out.iters,indicator %in% c("catch","f","rec","ssb"))
out.iters$indicator2 <- factor(out.iters$indicator,levels=c("rec","ssb","f","catch"))
out.iters <- subset(out.iters,assessment=="ss3")

##Now for the plots
###For HCR1 and HCR2
gg <- subset(ww, rule %in% c(1:2))
aa <- subset(out.iters, rule %in% c(1:2))

for (rr in c("med","low","lowmed","mix")){
  aux <- subset(gg,rec==rr)
  aux.iters <- subset(aa,rec_sc==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2018, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
    
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  
  ggsave(paste0(rr,"_HCR12.pdf"))
}

###For HCR5 and HCR6
gg <- subset(ww, rule %in% c(5:6))
aa <- subset(out.iters, rule %in% c(5:6))

for (rr in c("med","low","lowmed","mix")){
  aux <- subset(gg,rec==rr)
  aux.iters <- subset(aa,rec_sc==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2018, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  
  ggsave(paste0(rr,"_HCR56.pdf"))
}

###For HCR3 and HCR4
gg <- subset(ww, rule %in% c(3:4))
aa <- subset(out.iters, rule %in% c(3:4))

for (rr in c("med","low","lowmed","mix")){
  aux <- subset(gg,rec==rr)
  aux.iters <- subset(aa,rec_sc==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2018, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  
  ggsave(paste0(rr,"_HCR34.pdf"))
}

#==============================================================================
# Plots to see whether the conditions are met
#==============================================================================

# read data and compute pblim and p80blim by year

successyr <- NULL
for (scenario in scenario_list){
  load(file.path(res.dir,"output_scenarios",paste("results2018_",scenario,".RData",sep="")))
  aux <- out.bio %>% 
    separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_", keep = TRUE)%>%
    filter(year>2018)%>%
    group_by(year) %>% 
    summarize(pblim=sum(biomass>=337448)/length(biomass),
              p80blim=sum(biomass>= 0.8 * 337448)/length(biomass),
              pblow=sum(biomass>=196334)/length(biomass),
              p80blow=sum(biomass>= 0.8 * 196334)/length(biomass))
  aux$scenario <- rep(scenario,length(aux))
  successyr <- rbind(successyr, as.data.frame(aux))
  }

df <- successyr%>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_", keep = TRUE)%>%
  filter(Ass=="ASSss3")

library(patchwork)
df$Rec2 <- factor(df$Rec, levels=c("RECmed","REClow","REClowmed","RECmix"))

ss <- subset(df, Rule %in% c("HCR1","HCR2"))

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=pblim))+
  facet_grid(Rule~Rec2)+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=pblow))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("pblim_HCR12.pdf")

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=p80blim))+
  facet_grid(Rule~Rec2)+
  geom_hline(yintercept = 0.90, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=p80blow))+
  ylab("P(B1+>=0.8*Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("p80blim_HCR12.pdf")

ss <- subset(df, Rule %in% c("HCR5","HCR6"))

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=pblim))+
  facet_grid(Rule~Rec2)+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=pblow))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("pblim_HCR56.pdf")

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=p80blim))+
  facet_grid(Rule~Rec2)+
  geom_hline(yintercept = 0.90, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=p80blow))+
  ylab("P(B1+>=0.8*Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("p80blim_HCR56.pdf")

ss <- subset(df, Rule %in% c("HCR3","HCR4"))

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=pblim))+
  facet_grid(Rule~Rec2)+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=pblow))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("pblim_HCR34.pdf")

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=p80blim))+
  facet_grid(Rule~Rec2)+
  geom_hline(yintercept = 0.90, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=p80blow))+
  ylab("P(B1+>=0.8*Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("p80blim_HCR34.pdf")

