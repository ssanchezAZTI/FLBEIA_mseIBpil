################################################################################
#  IBpil results - summary tables                                             # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  11/10/2018                                                       #
#   modified:                                                                  #
################################################################################

# ibpil_results_tables_2018.r - summary plots for all scenarios
# msePIL8c9a/ibpil_results_tables_2018.r

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
# setwd(wd)


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
# library(ggplot2)
# library(FLBEIA)#, lib.loc="./Rlibs/")
library(R.utils)


#==============================================================================
# FUNCTIONS                                                               ----
#==============================================================================

source("./R/fun/ibpil_perfInd.R")


#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios
load(file.path("output", "scenario_list.RData"))

length(scenario_list)


#==============================================================================
#  Summary tables for all scenarios:                                       ----
#==============================================================================


# - year by year
#----------------------------------------------------------------------

proj.yr <- 2018
qs <- c("q95","q50","q05")

load( "./output/res_bio_all2018.RData")    # dat.bio.q
# dat.bio.q$stock <- NULL

load( "./output/res_eco_all2018.RData")    # dat.eco.q
dat.eco.q <- dat.eco.q[,c("year","scenario",paste("effort",qs,sep="_"))]

load( "./output/res_adv_all2018.RData")    # dat.adv.q
dat.adv.q <- dat.adv.q[,c("year","scenario",paste("tac",qs,sep="_"))]  #,paste("quotaUpt",qs,sep="_")

out.byr <- merge(dat.bio.q, dat.eco.q, by=c("scenario","year"))
out.byr <- merge(out.byr, dat.adv.q, by=c("scenario","year"))

# Separate scenario into different columns
# LEIRE: these columns are already included

# scenario   <- as.character(out.byr$scenario)
# assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
# rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
# rec        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
# initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
# obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))

#out <- cbind( assessment, rule, rec, initNage, obsErr, out.byr)
out <- out.byr

out <- subset(out, year>=proj.yr)


write.table( out, file=file.path("output","stats_byyr2018.csv"), dec = ".", sep = ";",
             row.names = FALSE)
rm( qs, dat.bio.q, dat.eco.q, dat.adv.q)


# Projection years
proj.yrs <- unique(out$year)


# - global
#-----------

out.all <- NULL

#! NOTE: different Blim for each scenario!!!!!

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio", , obj.adv="out.adv", 
                      scenario=cs, file.dat=file.path("output/output_scenarios",paste("results2018_",cs,".RData",sep="")),
                      proj.yrs=proj.yrs, Blim=337448)
  
  out.all <- rbind(out.all, obj)
  
}

# Separate scenario into different columns
scenario   <- as.character(out.all$scenario)
assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
rec        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))

out <- cbind( scenario, assessment, rule, rec, initNage, obsErr, out.all[, -1])


# Save data
write.table( out, file="./output/stats2018.csv", dec = ".", sep = ";",
             row.names = FALSE)
rm( cs, obj, out.all, scenario, assessment, rule, rec, initNage, obsErr)
rm( perfInd.pil, auxiliary.f, tacdif)

