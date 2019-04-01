################################################################################
#  IBpil results - plots in shiny                                              # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  29/03/2019                                                       #
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
res.dir  <- file.path("./output")
# directory with plots
plot.dir <- file.path("./plots")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(ggplot2)
library(FLBEIAShiny)

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios

load(file.path("output", "scenario_list.RData"))
length(scenario_list)

#==============================================================================
# Prepare objects for FLBEIAShiny
#==============================================================================

# load Qsummaries for all the scenarios

load(file.path("output","res_bio_all2018.RData"))
load(file.path("output","res_eco_all2018.RData"))
load(file.path("output","res_adv_all2018.RData"))

# reshape dat.bio.q to the long format
# It would have been easier to compute the summary in wide, reshape to long and then compute que summaryQ

tmp <- sapply(names(dat.bio.q)[-c(1:7)], function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
indnms <- unique(tmp[1,])
bioQ <- reshape(dat.bio.q, direction="long", varying=names(dat.bio.q)[-c(1:7)], v.names=c("q95","q50","q05"), 
        idvar=names(dat.bio.q)[c(1:7)], timevar="indicator", times=indnms, sep="_")
rownames(bioQ) <- NULL
bioQ$stock <- "PIL"
bioQ <- subset(bioQ, select=c("stock","indicator","year","scenario","q95","q50","q05"))

# reshape dat.eco.q to the long format

tmp <- sapply(names(dat.eco.q)[-c(1:7)], function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
indnms <- unique(tmp[1,])
ecoQ <- reshape(dat.eco.q, direction="long", varying=names(dat.eco.q)[-c(1:7)], v.names=c("q95","q50","q05"), 
                idvar=names(dat.eco.q)[c(1:7)], timevar="indicator", times=indnms, sep="_")
rownames(ecoQ) <- NULL
ecoQ$fleet <- "INT"
ecoQ <- subset(ecoQ, select=c("fleet","indicator","year","scenario","q95","q50","q05"))

# reshape dat.adv.q to the long format

tmp <- sapply(names(dat.adv.q)[-c(1:7)], function(x) strsplit(x, "_")[[1]], USE.NAMES=FALSE)
indnms <- unique(tmp[1,])
advQ <- reshape(dat.adv.q, direction="long", varying=names(dat.adv.q)[-c(1:7)], v.names=c("q95","q50","q05"), 
                idvar=names(dat.adv.q)[c(1:7)], timevar="indicator", times=indnms, sep="_")
rownames(advQ) <- NULL
advQ$stock <- "PIL"
advQ <- subset(advQ, select=c("stock","indicator","year","scenario","q95","q50","q05"))

# load the reference points

stknms <- "PIL"

load(file.path("input", "PIL_refpts2018.RData"))
RefPts <- expand.grid(indicator=c("Bmsy", "Fmsy", "Bpa", "Blim", "Fpa", "Flim"), scenario=scenario_list, stock=stknms, value=NA)[,c(3,2,1,4)]
RefPts$value <- rep(PIL_ref.pts[c("Bmsy", "Fmsy", "Bpa", "Blim", "Fpa", "Flim")], length(scenario_list))

# launch FLBEIA

flbeiaApp(RefPts = RefPts, bio = bioQ, flt = ecoQ, adv = advQ, 
          fltStk = NULL, mt = NULL, mtStk = NULL, risk = NULL,
          years = as.character(2018:2068), 
          calculate_npv = FALSE, npv =  NULL, npv.y0 = NULL, npv.yrs = NULL) 


