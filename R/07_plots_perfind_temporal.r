################################################################################
#  IBpil results - plots of performance statistics                             # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                         #
#   created:  02/04/2019                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2019
# Author: Leire Ibaibarriaga (AZTI) (<libaibarriaga@azti.es>)
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
res.dir  <- file.path("./temporal_output")
#res.dir  <- file.path("./output_med_innvar_indiceserror_1000it")
# directory with plots
plot.dir <- file.path("./plots")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(ggplot2)

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

df$sc <- 
  
  
  
  
  
#==============================================================================
# Read quantiles of the indicators per year
# Similar to the ones used in FLBEIAShiny
#==============================================================================

ind.dat <- read.table(file.path(res.dir,"stats_byyr2018.csv"), header=T, sep=";")

# reshape dat to the long format to use ggplot2

tmp <- sapply(names(ind.dat)[-c(1:7)], function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
indnms <- unique(tmp[1,])
dat <- reshape(ind.dat, direction="long", varying=names(ind.dat)[-c(1:7)], v.names=c("q95","q50","q05"), 
                idvar=names(ind.dat)[c(1:7)], timevar="indicator", times=indnms, sep="_")
rownames(dat) <- NULL
bioQ$stock <- "PIL"
bioQ <- subset(bioQ, select=c("stock","indicator","year","scenario","q95","q50","q05"))

