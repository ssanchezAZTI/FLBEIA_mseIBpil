#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "D:/LWise/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil" # main directory
setwd(wd)

# directory with results
res.dir  <- file.path("./output_modHCR4")
# directory with plots
plot.dir <- file.path(res.dir,"plots")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(R.utils)

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
dfyr <- dfyr[,c(1:8,33:38,24:26,12:14)]
#change units of variables of interest
dfyr[,c(9:11)] <- dfyr[,c(9:11)]/1000000
dfyr[,c(12:14)] <- dfyr[,c(12:14)]/1000
dfyr[,c(18:20)] <- dfyr[,c(18:20)]/1000

# reshape data 
dfyr%>%
  gather("var_q", "value", -c(1:8)) %>%
  separate("var_q", into = c("indicator", "quantile"), sep = "_") %>%
  tidyr::spread("quantile", "value") %>% as.data.frame -> dfyr

# data.frame for differences between runs with assessment and observation error with
# runs without assessment and no observation error

dfyr %>%
  dplyr::select(-scenario, -q05, -q95,-OER) %>%
  spread(Ass, q50) %>%
  mutate(ss3_relbias = (ASSss3 - ASSnone) / ASSnone) %>%
  as.data.frame -> relbias

# subset data.frame for variables of interest
relbias <- subset(relbias, indicator %in% c("ssb","f"))

##PLOTS for ASSss3 comparison with ASSnone
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+",'Ftgt0.08'='HCR8','Ftgt0.065'='HCR9','Ftgt0.064' ='HCR12')
#make sure order of variables is ssb and then f
relbias$indicator2 <- factor(relbias$indicator, levels=c("ssb","f"))

###rule HCR8
aux.rule <- subset(relbias, Ftgt %in% c('Ftgt0.08','Ftgt0.065','Ftgt0.064'))
droplevels(aux.rule$Ftgt)
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

ggplot(data=aux.rule,aes(x = year, y = ss3_relbias,colour=Ftgt)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
  facet_grid(indicator2~.,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Rule",breaks=c('Ftgt0.08', 'Ftgt0.065', 'Ftgt0.064'),
                        labels=c("HCR8", "HCR9", "HCR12"))
  
ggsave(paste0(plot.dir,'/',"HCRsrel.png"))


#get data from historical assessment
load("D:/LWise/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/data/ASSss3_historicalData.RData")

##make data.frame joining historical data with projection data 
dfyr %>%
  filter(year == 2018) %>%
  dplyr::select(-year, -q05, -q50, -q95) %>%
  left_join(all) %>% as.data.frame -> tt

dfyr %>%
  bind_rows(
    tt
  )%>% as.data.frame -> ww

ww <- subset(ww,Ass=="ASSss3")
ww$indicator2 <- factor(ww$indicator, levels=c("rec","ssb","f","catch"))

#Data from two iters
out.all <- NULL
for (cs in scenario_list){
  
  file.dat <- file.path(res.dir,paste("output_scenarios/results2018_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  aux$scenario <- as.character(cs)
  aux <- subset(aux, iter %in% c(45,235))
  aux <- subset(aux, year>2017)
  
  out.all <- rbind(out.all, aux)
  
}

out.iters <-
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","Ftgt"), sep = "_",  remove=FALSE)

#change units of variables of interest
out.iters[,16] <- out.iters[,16]/1000000
out.iters[,12] <- out.iters[,12]/1000
out.iters[,17] <- out.iters[,17]/1000

# reshape data 
out.iters <- melt(out.iters,id=1:10,variable.name = "indicator")
names(out.iters)[11] <- "indicator"
out.iters <- subset(out.iters,indicator %in% c("catch","f","rec","ssb"))
out.iters$indicator2 <- factor(out.iters$indicator,levels=c("rec","ssb","f","catch"))
out.iters <- subset(out.iters,Ass=="ASSss3")

##Now for the plots
###For HCR4
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+")

for (rr in unique(out.iters$Ftgt)){
  aux <- subset(ww, Ftgt==rr)
  aux.iters <- subset(out.iters,Ftgt==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~.,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2018, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2048,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  p +# geom_hline(aes(yintercept = 337.448), data = subset(ww, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(ww,indicator=="ssb"),linetype="dashed",color="#00A08A")
  
  ggsave(paste0(plot.dir,'/',rr,"_HCR4.png"),width = 3.92,height=6.65)
}

#==============================================================================
# Plots to see whether the conditions are met
#==============================================================================

# read data and compute pblim by year

successyr <- NULL
for (scenario in scenario_list){
  load(file.path(res.dir,paste("output_scenarios/results2018_",scenario,".RData",sep="")))
  out.bio$scenario <- scenario
  aux <- out.bio %>% 
    separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","Ftgt"), sep = "_",  remove=FALSE)%>%
    filter(year>2018)%>%
    group_by(year) %>% 
    summarise(pblim=sum(biomass>=337448)/length(biomass),
              pblow=sum(biomass>=196334)/length(biomass),
              pzero=sum(catch<= 1e-6)/length(catch))
  aux$scenario <- rep(scenario,dim(aux)[1])
  successyr <- rbind(successyr, as.data.frame(aux))
  }

df <- successyr%>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","Ftgt"), sep = "_",  remove=FALSE)%>%
  filter(Ass=="ASSss3")

df$Rec2 <- factor(df$Rec, levels=c("RECmed","REClow","REClowmed","RECmix"))

ss <- subset(df, Rule=="HCR4" & Ftgt %in% c('Ftgt0.064','Ftgt0.065','Ftgt0.08'))
facet_names <- c('Ftgt0.064'='HCR12','Ftgt0.065'= 'HCR9', 'Ftgt0.08' = 'HCR8')
ss$Ftgt <- as.factor(ss$Ftgt)

ss %>% 
  arrange(desc(Ftgt)) %>% # sort
  mutate_at(vars(Ftgt), funs(factor(., levels=unique(.)))) %>% 
  ggplot()+
  geom_line(aes(x=year,y=pblow))+
  facet_grid(.~Ftgt,labeller = as_labeller(facet_names))+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave(paste0(plot.dir,'/',"pblim_HCR8912.png"))


maxP <- max(ss$pzero)

ss %>% 
  arrange(desc(Ftgt)) %>% # sort
  mutate_at(vars(Ftgt), funs(factor(., levels=unique(.)))) %>% 
  ggplot()+
  geom_line(aes(x=year,y=pzero))+
  facet_grid(Ftgt~.)+
  #geom_hline(yintercept = 0.90, linetype = "longdash")+
  #ylim(c(0,maxP))+
  ylim(c(0,1))+
  ylab("P(TAC = 0)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave(paste0(plot.dir,'/',"pzero_HCR4.pdf"))

####################
bloss <- 112943 #b1+ of 2015 according to 2018 assessment 
blim <- 196334
b1plus <- seq(0,400000,1000)
Ftgt12 <- 0.064
Flow12 <- 0.046

fs2 <- ifelse(b1plus<=bloss,0,ifelse(b1plus > bloss & b1plus<=blim,((Ftgt12-Flow12)/(blim-bloss))*b1plus + (-(Ftgt12-Flow12)*bloss/(blim-bloss)+Flow12),0.064))


tab.hcr2 <- data.frame(Bio=b1plus,Fbar = fs2)

ggplot(tab.hcr2,aes(x=Bio,y=Fbar)) + 
  geom_point(size=1,color="blue",) +
  expand_limits(y=c(0,0.15)) +
  ylab("Fbar (2-5)") +
  xlab("B1+ (tonnes)") +
  #theme_bw()+
  geom_vline(xintercept = c(bloss,blim),linetype="dashed",color="blue") + 
  scale_x_continuous(breaks=c(bloss,blim),labels = c(expression(B[low]),expression(B[lim])))+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="HCR12")
ggsave(paste0(plot.dir,'/',"HCR12.png"))

Ftgt8 <- 0.08
Flow8 <- 0.06

fs2 <- ifelse(b1plus<=bloss,0,ifelse(b1plus > bloss & b1plus<=blim,((Ftgt8-Flow8)/(blim-bloss))*b1plus + (-(Ftgt8-Flow8)*bloss/(blim-bloss)+Flow8),0.08))


tab.hcr2 <- data.frame(Bio=b1plus,Fbar = fs2)

ggplot(tab.hcr2,aes(x=Bio,y=Fbar)) + 
  geom_point(size=1,color="blue",) +
  expand_limits(y=c(0,0.15)) +
  ylab("Fbar (2-5)") +
  xlab("B1+ (tonnes)") +
  #theme_bw()+
  geom_vline(xintercept = c(bloss,blim),linetype="dashed",color="blue") + 
  scale_x_continuous(breaks=c(bloss,blim),labels = c(expression(B[low]),expression(B[lim])))+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="HCR8")
ggsave(paste0(plot.dir,'/',"HCR8.png"))

Ftgt9 <- 0.065
Flow9 <- 0.047

fs2 <- ifelse(b1plus<=bloss,0,ifelse(b1plus > bloss & b1plus<=blim,((Ftgt9-Flow9)/(blim-bloss))*b1plus + (-(Ftgt9-Flow9)*bloss/(blim-bloss)+Flow9),0.065))


tab.hcr2 <- data.frame(Bio=b1plus,Fbar = fs2)

ggplot(tab.hcr2,aes(x=Bio,y=Fbar)) + 
  geom_point(size=1,color="blue",) +
  expand_limits(y=c(0,0.15)) +
  ylab("Fbar (2-5)") +
  xlab("B1+ (tonnes)") +
  #theme_bw()+
  geom_vline(xintercept = c(bloss,blim),linetype="dashed",color="blue") + 
  scale_x_continuous(breaks=c(bloss,blim),labels = c(expression(B[low]),expression(B[lim])))+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="HCR9")
ggsave(paste0(plot.dir,'/',"HCR9.png"))
