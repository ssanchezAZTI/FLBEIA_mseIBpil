################################################################################
#  IBpil results - plots of performance statistics                             # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                         #
#   created:  04/04/2019                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2019
# Author: AZTI (<libaibarriaga@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
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

theme_set(theme_bw())

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios

load(file.path(res.dir, "scenario_list.RData"))
length(scenario_list)

#==============================================================================
# Load the reference points
#==============================================================================

# load the reference points

load(file.path(wd, "input", "PIL_refpts2018.RData"))
PIL_ref.pts

#==============================================================================
# Read performance statistics
#==============================================================================

# load performance statistics for all the scenarios

df <- read.table(file.path(res.dir,"stats2018.csv"), header=T, sep=";")

# reshape to the long format for ggplot

df <- reshape(df, idvar=c("period","scenario","Ass","Rule","Rec","INN","OER"), varying=list(8:ncol(df)), 
              times=names(df)[8:ncol(df)], timevar="indicator", v.names="value", direction="long")

#period as an ordered factor for the figures

df$period <- factor(df$period, levels=c("initial","short","last"))

#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- unique(df$indicator)
perflabels <- c( "P5th_B1plus","P10th_B1plus","Median_B1plus","P90th_B1plus","P95th_B1plus","Mean_B1plus","Median_lastB1plus",
                 "IAV1_B1plus","IAV2_B1plus",
                 "P(B1+>=0.8Blim)","P(B1+>=0.8Blow)","Nb years to get B1plus>=0.8Blim","Nb years to get B1plus>=0.8Blow",
                 "P(B1+<Blim)","P(B1+<Blow)","P(once B1+<Blim)","P(once B1+<Blow)",
                 "max P(B1+<Blim)","max P(B1p+<Blow)",
                 "Nb years B1+<Blim", "Nb years to get B1plus>Blim",
                 "Nb years B1+<Blow", "Nb years to get B1plus>Blow",
                 "P(closure)", "P(closure once)", "Nb yearsclosure",
                 "P5th_Catch","Median_Catch","P95th_Catch",
                 "Mean_Catch", "StDev_Catch",
                 "IAV1_Catch","IAV2_Catch")

# effect of fixed or variable initial population

pdf(file.path(plot.dir,"plot_compare_by_periods.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind)
  p <- ggplot(aux, aes(x=period, y=value, fill=period))+
    geom_bar(stat="identity")+
    facet_grid(Ass + Rule ~ Rec)+
    ylab(perflabels[i])
  if(length(grep("Risk", ind))>0){
    p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
    p <- ylim(c(0,1)) 
  }
  if(ind %in% c("P_B1plus_0.8Blim","P_B1plus_0.8Blow")){
    p <- p + geom_hline(yintercept = 0.9, linetype = "longdash")
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("closure","closure_once")){
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("P5th_B1plus","P10th_B1plus","Median_B1plus","P90th_B1plus","P95th_B1plus","Mean_B1plus","Median_lastB1plus")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

# comparison of rules for each SR case

pdf(file.path(plot.dir,"plot_compare_rule_initial.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind & period=="initial")
  aux$rule <- as.factor(aux$Rule)
  p <- ggplot(aux, aes(x=factor(Rule), y=value, fill=Rule))+
    geom_bar(stat="identity")+
    facet_grid(Ass ~ Rec)+
    ylab(perflabels[i])
  if(length(grep("Risk", ind))>0){
    p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
    p <- ylim(c(0,1)) 
  }
  if(ind %in% c("MP_Success","MP_Success_Low")){
    p <- p + geom_hline(yintercept = 0.9, linetype = "longdash")
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("closure","closure_once")){
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("p025_SSB","p05_SSB","Median_SSB","p95_SSB","p975_SSB","Mean_SSB","Median_lastSSB")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

pdf(file.path(plot.dir,"plot_compare_rule_short.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind & period=="short")
  aux$rule <- as.factor(aux$Rule)
  p <- ggplot(aux, aes(x=factor(Rule), y=value, fill=Rule))+
    geom_bar(stat="identity")+
    facet_grid(Ass ~ Rec)+
    ylab(perflabels[i])
  if(length(grep("Risk", ind))>0){
    p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
    p <- ylim(c(0,1)) 
  }
  if(ind %in% c("MP_Success","MP_Success_Low")){
    p <- p + geom_hline(yintercept = 0.9, linetype = "longdash")
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("closure","closure_once")){
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("p025_SSB","p05_SSB","Median_SSB","p95_SSB","p975_SSB","Mean_SSB","Median_lastSSB")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

pdf(file.path(plot.dir,"plot_compare_rule_last.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind & period=="last")
  aux$rule <- as.factor(aux$Rule)
  p <- ggplot(aux, aes(x=factor(Rule), y=value, fill=Rule))+
    geom_bar(stat="identity")+
    facet_grid(Ass ~ Rec)+
    ylab(perflabels[i])
  if(length(grep("Risk", ind))>0){
    p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
    p <- ylim(c(0,1)) 
  }
  if(ind %in% c("MP_Success","MP_Success_Low")){
    p <- p + geom_hline(yintercept = 0.9, linetype = "longdash")
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("closure","closure_once")){
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("p025_SSB","p05_SSB","Median_SSB","p95_SSB","p975_SSB","Mean_SSB","Median_lastSSB")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

# comparison of SRs for each rule 

pdf(file.path(plot.dir,"plot_compare_sr_initial.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind & period=="initial")
  aux$rule <- as.factor(aux$Rule)
  p <- ggplot(aux, aes(x=Rec, y=value, fill=Rec))+
    geom_bar(stat="identity")+
    facet_grid(Ass ~ Rule)+
    ylab(perflabels[i])
  if(length(grep("Risk", ind))>0){
    p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
    p <- ylim(c(0,1)) 
  }
  if(ind %in% c("MP_Success","MP_Success_Low")){
    p <- p + geom_hline(yintercept = 0.9, linetype = "longdash")
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("closure","closure_once")){
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("p025_SSB","p05_SSB","Median_SSB","p95_SSB","p975_SSB","Mean_SSB","Median_lastSSB")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

pdf(file.path(plot.dir,"plot_compare_sr_short.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind & period=="short")
  aux$rule <- as.factor(aux$Rule)
  p <- ggplot(aux, aes(x=Rec, y=value, fill=Rec))+
    geom_bar(stat="identity")+
    facet_grid(Ass ~ Rule)+
    ylab(perflabels[i])
  if(length(grep("Risk", ind))>0){
    p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
    p <- ylim(c(0,1)) 
  }
  if(ind %in% c("MP_Success","MP_Success_Low")){
    p <- p + geom_hline(yintercept = 0.9, linetype = "longdash")
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("closure","closure_once")){
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("p025_SSB","p05_SSB","Median_SSB","p95_SSB","p975_SSB","Mean_SSB","Median_lastSSB")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

pdf(file.path(plot.dir,"plot_compare_sr_last.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind & period=="last")
  aux$rule <- as.factor(aux$Rule)
  p <- ggplot(aux, aes(x=Rec, y=value, fill=Rec))+
    geom_bar(stat="identity")+
    facet_grid(Ass ~ Rule)+
    ylab(perflabels[i])
  if(length(grep("Risk", ind))>0){
    p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
    p <- ylim(c(0,1)) 
  }
  if(ind %in% c("MP_Success","MP_Success_Low")){
    p <- p + geom_hline(yintercept = 0.9, linetype = "longdash")
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("closure","closure_once")){
    p <- p + ylim(c(0,1)) 
  }  
  if(ind %in% c("p025_SSB","p05_SSB","Median_SSB","p95_SSB","p975_SSB","Mean_SSB","Median_lastSSB")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

# To increase sizes, we should add in the code:
# theme(text=element_text(size=10),
#       title=element_text(size=10,face="bold"),
#       strip.text=element_text(size=10),
#       plot.title = element_text(hjust = 0.5))+

#==============================================================================
# radar plots to compare several performance statistics at the same time
#==============================================================================

# function to plot radar plot
# http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# rescale each indicator from 0 to 1

library(dplyr)
library(scales)

df.scaled <- df %>% group_by(indicator) %>% 
  mutate( value2=rescale(value))

df.scaled <- as.data.frame(df.scaled)

# radar plot 

for (rr in c("low","med","mix")){
  scnms <- scenario_list # names of scenarios to be compared
  scnms <- scenario_list[grep(paste("REC",rr,sep=""),scenario_list)] # names of scenarios to be compared
  for (cs in c("initial","short","last")){
    tiff(file.path(plot.dir,paste("radar_REC_",rr,"_period_",cs,".tif",sep="")), width=900, height=700)
    aux <- subset(df.scaled, period==cs & scenario %in% scnms & 
                    indicator %in% c("Mean_B1plus","Mean_Catch","IAV1_Catch","P_B1plus_0.8Blim","max_P_B1plus_Blim","max_P_B1plus_Blow"))
    aux <- aux[order(aux$indicator), ] 
    p <- ggplot(data=aux, aes(x=indicator, y=value2, col=scenario, fill=scenario, group=scenario))+
      #  geom_polygon(alpha=0.2, lwd=1)+
      geom_polygon(fill=NA, lwd=1)+
      geom_point(cex=1.5)+
      coord_radar()+
      theme_bw()+
      theme(text=element_text(size=14),
            strip.text=element_text(size=14),
            title=element_text(size=18,face="bold"))+
      ylab("")+
      ylim(c(0,1))
    print(p)
    dev.off()
  }
}

# example of radar plot to compare several performance statistics at the same time
# for several scenarios

scnms <- scenario_list # names of scenarios to be compared

tiff(file.path(plot.dir,"example_radar.tif"), width=900, height=700)
aux <- subset(df.scaled, period=="initial" & scenario %in% scnms & 
                indicator %in% c("Mean_B1plus","Mean_Catch","IAV1_Catch","P_B1plus_0.8Blim","max_P_B1plus_Blim","max_P_B1plus_Blow"))
aux <- aux[order(aux$indicator), ] 
p <- ggplot(data=aux, aes(x=indicator, y=value2, col=scenario, fill=scenario, group=scenario))+
#  geom_polygon(alpha=0.2, lwd=1)+
  geom_polygon(fill=NA, lwd=1)+
  geom_point(cex=1.5)+
  coord_radar()+
  theme_bw()+
  theme(text=element_text(size=14),
        strip.text=element_text(size=14),
        title=element_text(size=18,face="bold"))+
  ylab("")+
  ylim(c(0,1))
print(p)
dev.off()

#==============================================================================
# Read quantiles of the indicators per year
# Similar plots to the ones used in FLBEIAShiny
#==============================================================================

# read data

dfyr <- read.table(file.path(res.dir,"stats_byyr2018.csv"), header=T, sep=";")

# reshape dat to the long format to use ggplot2

tmp <- sapply(names(dfyr)[-c(1:7)], function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
indnms <- unique(tmp[1,])
dfyr <- reshape(dfyr, direction="long", varying=names(dfyr)[-c(1:7)], v.names=c("q95","q50","q05"), 
                idvar=names(dfyr)[c(1:7)], timevar="indicator", times=indnms, sep="_")
rownames(dfyr) <- NULL

# external pdf for all the indicators for the scenarios to be compared
# indicators are: biomass, catch, discards, f, landings, rec, ssb, catch.iyv, land.iyv, effort and tac

scnms <- scenario_list # names of scenarios to be compared
pdf(file.path(plot.dir,paste("plots_series_indicators.pdf",sep="")), width=10)
for (ind in unique(dfyr$indicator)){
  aux <- subset(dfyr, indicator==ind & scenario %in% scnms)
  aux$year <- as.numeric(aux$year)
  p <- ggplot(data=aux, aes(x=year, y=q50, color=scenario))+
    geom_line()+
    geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=scenario), alpha=0.2)+
    geom_vline(xintercept = 2018, linetype = "longdash")+
    theme_bw()+
    theme(text=element_text(size=10),
          title=element_text(size=10,face="bold"),
          strip.text=element_text(size=10),
          plot.title = element_text(hjust = 0.5))+
    ylab(ind)+
    expand_limits(y=0)+
    ggtitle("")
  if(ind %in% c("ssb")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

# compare scenarios with assessment error

scnms <- scenario_list[grep("ASSss3",scenario_list)] # names of scenarios to be compared
pdf(file.path(plot.dir,paste("plots_series_indicators_ASSss3.pdf",sep="")), width=10)
for (ind in unique(dfyr$indicator)){
  aux <- subset(dfyr, indicator==ind & scenario %in% scnms)
  aux$year <- as.numeric(aux$year)
  p <- ggplot(data=aux, aes(x=year, y=q50, color=scenario))+
    geom_line()+
    geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=scenario), alpha=0.2)+
    geom_vline(xintercept = 2018, linetype = "longdash")+
    theme_bw()+
    theme(text=element_text(size=10),
          title=element_text(size=10,face="bold"),
          strip.text=element_text(size=10),
          plot.title = element_text(hjust = 0.5))+
    ylab(ind)+
    expand_limits(y=0)+
    ggtitle("")
  if(ind %in% c("ssb")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

# compare scenarios without assessment error

scnms <- scenario_list[grep("ASSnone",scenario_list)] # names of scenarios to be compared
pdf(file.path(plot.dir,paste("plots_series_indicators_ASSnone.pdf",sep="")), width=10)
for (ind in unique(dfyr$indicator)){
  aux <- subset(dfyr, indicator==ind & scenario %in% scnms)
  aux$year <- as.numeric(aux$year)
  p <- ggplot(data=aux, aes(x=year, y=q50, color=scenario))+
    geom_line()+
    geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=scenario), alpha=0.2)+
    geom_vline(xintercept = 2018, linetype = "longdash")+
    theme_bw()+
    theme(text=element_text(size=10),
          title=element_text(size=10,face="bold"),
          strip.text=element_text(size=10),
          plot.title = element_text(hjust = 0.5))+
    ylab(ind)+
    expand_limits(y=0)+
    ggtitle("")
  if(ind %in% c("ssb")){
    p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
  }
  print(p)
}
dev.off()

# compare scenarios with assessment error for each SR 

for (cs in c("low","med","mix")){
  scnms <- scenario_list[grep("ASSss3",scenario_list)] # names of scenarios to be compared
  scnms <- scenario_list[grep(paste("REC",cs,sep=""),scenario_list)] # names of scenarios to be compared
  pdf(file.path(plot.dir,paste("plots_series_indicators_ASSss3_REC",cs,".pdf",sep="")), width=10)
  for (ind in unique(dfyr$indicator)){
    aux <- subset(dfyr, indicator==ind & scenario %in% scnms)
    aux$year <- as.numeric(aux$year)
    p <- ggplot(data=aux, aes(x=year, y=q50, color=scenario))+
      geom_line()+
      geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=scenario), alpha=0.2)+
      geom_vline(xintercept = 2018, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=10),
            title=element_text(size=10,face="bold"),
            strip.text=element_text(size=10),
            plot.title = element_text(hjust = 0.5))+
      ylab(ind)+
      expand_limits(y=0)+
      ggtitle("")
    if(ind %in% c("ssb")){
      p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
    }
    print(p)
  }
  dev.off()
}

# compare scenarios with assessment error for each SR 

for (cs in c(1,2,5,6)){
  scnms <- scenario_list[grep("ASSss3",scenario_list)] # names of scenarios to be compared
  scnms <- scenario_list[grep(paste("HCR",cs,sep=""),scenario_list)] # names of scenarios to be compared
  pdf(file.path(plot.dir,paste("plots_series_indicators_ASSss3_HCR",cs,".pdf",sep="")), width=10)
  for (ind in unique(dfyr$indicator)){
    aux <- subset(dfyr, indicator==ind & scenario %in% scnms)
    aux$year <- as.numeric(aux$year)
    p <- ggplot(data=aux, aes(x=year, y=q50, color=scenario))+
      geom_line()+
      geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=scenario), alpha=0.2)+
      geom_vline(xintercept = 2018, linetype = "longdash")+
      theme_bw()+
      theme(text=element_text(size=10),
            title=element_text(size=10,face="bold"),
            strip.text=element_text(size=10),
            plot.title = element_text(hjust = 0.5))+
      ylab(ind)+
      expand_limits(y=0)+
      ggtitle("")
    if(ind %in% c("ssb")){
      p <- p + geom_hline(yintercept = c(196334, 337448), linetype = "longdash")
    }
    print(p)
  }
  dev.off()
}


# specific plot: time series biomass 1+ with ref points

scnms <- scenario_list # names of scenarios to be compared
tiff(file.path(plot.dir,paste("plots_series_biomass.tif",sep="")), , width=900, height=700)
  aux <- subset(dfyr, indicator=="biomass" & scenario %in% scnms)
  aux$year <- as.numeric(aux$year)
  p <- ggplot(data=aux, aes(x=year, y=q50, color=scenario))+
    geom_line()+
    geom_ribbon(aes(x=year, ymin=q05, ymax=q95, fill=scenario), alpha=0.2)+
    geom_hline(yintercept = PIL_ref.pts["Blim"], linetype = "longdash")+
    geom_hline(yintercept = 0.8*PIL_ref.pts["Blim"], linetype = "longdash")+
    geom_hline(yintercept = PIL_ref.pts["Bloss"], linetype = "longdash")+
    geom_vline(xintercept = 2018, linetype = "longdash")+
    theme_bw()+
    theme(text=element_text(size=14),
          title=element_text(size=14,face="bold"),
          strip.text=element_text(size=14),
          plot.title = element_text(hjust = 0.5))+
    ylab("Biomass")+
    expand_limits(y=0)+  # to expand lower limit of the y-axis to 0
    ggtitle("")
  print(p)
dev.off()

# relative changes of q50 in dfyr wrt to a base case scenario

scref <- scnms[1] # reference scenario

dfyr.rel <- NULL
for (scnm in scenario_list){
  aux.base <- subset(dfyr, scenario==scref, 
                     select=c("indicator","year","q50"))
  aux.other <- subset(dfyr, scenario==scnm, 
                      select=c("scenario","year","assessment","rule","rec","initNage","obsErr","indicator","q50"))
  aux <- merge(aux.other, aux.base, by=c("indicator","year"), suffixes=c(".other",".base"))
  aux$ratio <- (aux$q50.other-aux$q50.base)/aux$q50.base
  dfyr.rel <- rbind(dfyr.rel, aux)
}

pdf(file.path(plot.dir,paste("plots_series_indicators_relative.pdf",sep="")), width=10)

scnms <- scenario_list[-1] # names of scenarios to be compared wrt to the base case

for (ind in unique(dfyr.rel$indicator)){
  aux <- subset(dfyr.rel, indicator==ind & scenario %in% scnms)
  aux$year <- as.numeric(aux$year)
  p <- ggplot(data=aux, aes(x=year, y=ratio, color=scenario))+
    geom_line()+
    geom_vline(xintercept = 2018, linetype = "longdash")+
    theme_bw()+
    theme(text=element_text(size=10),
          title=element_text(size=10,face="bold"),
          strip.text=element_text(size=10),
          plot.title = element_text(hjust = 0.5))+
    ylab(paste("Relative",ind))+
    ggtitle("")
  print(p)
}
dev.off()

#==============================================================================
# Plots to see whether the conditions are met
#==============================================================================

# load library

library(dplyr)

# read data and compute pblim and p80blim by year

successyr <- NULL
for (scenario in scenario_list){
  load(file.path(res.dir,"output_scenarios",paste("results2018_",scenario,".RData",sep="")))
  aux <- out.bio %>% 
          group_by(scenario, year) %>% 
          summarize(pblim=sum(biomass>=337448)/length(biomass),
                    p80blim=sum(biomass>= 0.8 * 337448)/length(biomass))
  successyr <- rbind(successyr, as.data.frame(aux))
}
successyr <- subset(successyr, year>2018)

pdf(file.path(plot.dir,paste("pblim_by_yr.pdf",sep="")), width=10)
for (sc in scenario_list){
  p <- ggplot(subset(successyr, scenario==sc), aes(x=year,y=pblim))+
    geom_line()+
    geom_hline(yintercept = 0.95, linetype = "longdash")+
    ylim(c(0,1))+
    theme(text=element_text(size=10),
          title=element_text(size=10,face="bold"),
          strip.text=element_text(size=10),
          plot.title = element_text(hjust = 0.5))+
    ylab("P(SSB>=Blim)")+
    ggtitle(sc)
  print(p)
}
p <- ggplot(successyr, aes(x=year,y=pblim, col=scenario))+
  geom_line()+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10),
        plot.title = element_text(hjust = 0.5))+
  ylab("P(SSB>=Blim)")+
  ggtitle("")
print(p)
dev.off()


pdf(file.path(plot.dir,paste("p80blim_by_yr.pdf",sep="")), width=10)
for (sc in scenario_list){
  p <- ggplot(subset(successyr, scenario==sc), aes(x=year,y=p80blim))+
    geom_line()+
    geom_hline(yintercept = 0.9, linetype = "longdash")+
    ylim(c(0,1))+
    theme(text=element_text(size=10),
          title=element_text(size=10,face="bold"),
          strip.text=element_text(size=10),
          plot.title = element_text(hjust = 0.5))+
    ylab("P(SSB>=0.8Blim)")+
    ggtitle(sc)
  print(p)
}
p <- ggplot(successyr, aes(x=year,y=p80blim, col=scenario))+
  geom_line()+
  geom_hline(yintercept = 0.90, linetype = "longdash")+
  ylim(c(0,1))+
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10),
        plot.title = element_text(hjust = 0.5))+
  ylab("P(SSB>=0.8Blim)")+
  ggtitle("")
print(p)
dev.off()

#==============================================================================
# Calculate nbr pop that reach Blim and 'would stay in med regime' by yr                                                               ----
#==============================================================================

#subset for RECmix
scenarios <- scenario_list[grep("RECmix",scenario_list)]

#auxiliary function
ff <- function(pop, threshold) {
  cut_year <- pop$year[which(pop$ssb >= threshold)[1]]
  return(pop[pop$year >= cut_year, ])
}

nbrBlim <- NULL

for (scenario in scenarios){
  load(file.path(res.dir,"output_scenarios",paste("results2018_",scenario,".RData",sep="")))
  aux <- subset(out.bio,year>2018)
  yy <- lapply(split(aux, aux$iter), ff,threshold=337448)
  tt <-plyr::rbind.fill(yy)
  tt <- subset(tt,scenario == scenario)
  rr <- tt %>%
    group_by(scenario, year) %>% 
    summarize(nbrPop=length(unique(iter)))
  nbrBlim <- rbind(nbrBlim,as.data.frame(rr))
}

pdf(file.path(plot.dir,paste("nbrPopBlim_by_yr.pdf",sep="")), width=10)
p <- ggplot(nbrBlim, aes(x=year,y=nbrPop, col=scenario))+
  geom_line()+
  theme(text=element_text(size=10),
        title=element_text(size=10,face="bold"),
        strip.text=element_text(size=10),
        plot.title = element_text(hjust = 0.5))+
  ylab("Nbr pop med regime")+
  ggtitle("")
print(p)
dev.off()

