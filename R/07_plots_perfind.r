################################################################################
#  IBpil results - plots of performance statistics                             # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                         #
#   created:  02/04/2019                                                       #
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
#res.dir  <- file.path("./output_med_innvar_indiceserror_1000it")
# directory with plots
plot.dir <- file.path("./plots")

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
# Read performance statistics
#==============================================================================

# load performance statistics for all the scenarios

df <- read.table(file.path(res.dir,"stats2018.csv"), header=T, sep=";")

# reshape to the long format for ggplot

df <- reshape(df, idvar=c("scenario","assessment","rule","rec","initNage","obsErr"), varying=list(7:ncol(df)), 
              times=names(df)[7:ncol(df)], timevar="indicator", v.names="value", direction="long")

#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names of performance statistics

perfnms <- unique(df$indicator)

# effect of fixed or variable initial population

pdf(file.path(plot.dir,"plot_compare_initNage.pdf"), onefile=T)
for (ind in perfnms){
  aux <- subset(df, indicator %in% ind)
  p <- ggplot(aux, aes(x=initNage, y=value, fill=initNage))+
    geom_bar(stat="identity")+
    facet_grid(interaction(assessment, rule) ~ rec)+
    ylab(ind)
  print(p)
}
dev.off()

# comparison of rules for each SR case with variable initial population

pdf(file.path(plot.dir,"plot_compare_rule_for_initNage_var.pdf"), onefile=T)
for (ind in perfnms){
  aux <- subset(df, initNage=="var" & indicator %in% ind)
  aux$rule <- as.factor(aux$rule)
  p <- ggplot(aux, aes(x=factor(rule), y=value, fill=rule))+
    geom_bar(stat="identity")+
    facet_grid(assessment ~ rec)+
    ylab(ind)
  print(p)
}
dev.off()

# comparison of SRs for each rule with variable initial population

pdf(file.path(plot.dir,"plot_compare_sr_for_initNage_var.pdf"), onefile=T)
for (ind in perfnms){
  aux <- subset(df, initNage=="var" & indicator %in% ind)
  aux$rule <- as.factor(aux$rule)
  p <- ggplot(aux, aes(x=rec, y=value, fill=rec))+
    geom_bar(stat="identity")+
    facet_grid(assessment ~ rule)+
    ylab(ind)
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

# example of radar plot to compare several performance statistics at the same time
# for several scenarios

scnms <- scenario_list[1:4] # names of scenarios to be compared

tiff(file.path(plot.dir,"example_radar.tif"), width=900, height=700)
aux <- subset(df.scaled, scenario %in% scnms & indicator %in% c("Median_SSB","Median_lastSSB","average_catch","Risk3","Risk3_Low"))
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

dfyr <- read.table(file.path(res.dir,"stats_byyr2018.csv"), header=T, sep=";")

# reshape dat to the long format to use ggplot2

tmp <- sapply(names(dfyr)[-c(1:7)], function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
indnms <- unique(tmp[1,])
dfyr <- reshape(dfyr, direction="long", varying=names(dfyr)[-c(1:7)], v.names=c("q95","q50","q05"), 
                idvar=names(dfyr)[c(1:7)], timevar="indicator", times=indnms, sep="_")
rownames(dfyr) <- NULL

# external pdf for all the indicators for the scenarios to be compared
# indicators are: biomass, catch, discards, f, landings, rec, ssb, catch.iyv, land.iyv, effort and tac

scnms <- scenario_list[1:4] # names of scenarios to be compared

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
    ggtitle("")
  print(p)
}
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

scnms <- scenario_list[2:4] # names of scenarios to be compared wrt to the base case

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

