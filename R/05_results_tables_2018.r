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

wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
setwd(wd)

# directory with results
res.dir  <- file.path("./output")

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
load(file.path(res.dir, "scenario_list.RData"))

length(scenario_list)


#==============================================================================
#  Summary tables for all scenarios:                                       ----
#==============================================================================


# - year by year
#----------------------------------------------------------------------

proj.yr <- 2018
qs <- c("q95","q50","q05")

load(file.path(res.dir,"res_bio_all2018.RData"))    # dat.bio.q
# dat.bio.q$stock <- NULL

load( file.path(res.dir,"res_eco_all2018.RData"))    # dat.eco.q
dat.eco.q <- dat.eco.q[,c("year","scenario",paste("effort",qs,sep="_"))]

load( file.path(res.dir,"res_adv_all2018.RData"))    # dat.adv.q
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


write.table( out, file=file.path(res.dir, "stats_byyr2018.csv"), dec = ".", sep = ";",
             row.names = FALSE)
rm( qs, dat.bio.q, dat.eco.q, dat.adv.q)


# Projection years
# proj.yrs <- unique(out$year)

#Run the function perfInd.pil the number of times needed to estimate stats for all the different periods

# - global
#-----------

#! NOTE: different Blim for each scenario!!!!!

#initial years of projection 2019:2023

out.all5 <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio", obj.adv="out.adv", 
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep="")),
                      proj.yrs=2019:2023, Blim=337448, Blow=196334)
  
  out.all5 <- rbind(out.all5, obj)
  
}

out.all5 <- cbind(period=rep("initial",dim(out.all5)[1]),out.all5)

#first 10 years of projection 2019:2028

out.all10 <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio", obj.adv="out.adv", 
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep="")),
                      proj.yrs=2019:2028, Blim=337448, Blow=196334)
  
  out.all10 <- rbind(out.all10, obj)
  
}

out.all10 <- cbind(period=rep("short",dim(out.all10)[1]),out.all10)

#last 10 years of projection 2039:2048

out.all.last <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio", obj.adv="out.adv", 
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep="")),
                      proj.yrs=2039:2048, Blim=337448, Blow=196334)
  
  out.all.last <- rbind(out.all.last, obj)
  
}

out.all.last <- cbind(period=rep("last",dim(out.all.last)[1]),out.all.last)

#initial years of projection 2019:2048

out.all <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio", obj.adv="out.adv", 
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep="")),
                      proj.yrs=2019:2048, Blim=337448, Blow=196334)
  
  out.all <- rbind(out.all, obj)
  
}

out.all <- cbind(period=rep("all",dim(out.all)[1]),out.all)

out.all <- rbind(out.all5,out.all10,out.all.last,out.all)

# Separate scenario into different columns
library(tidyr)
out.final <-
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_", keep = TRUE, remove=FALSE)


# Save data
write.table( out.final, file=file.path(res.dir,"stats2018.csv"), dec = ".", sep = ";",
             row.names = FALSE)
rm( cs, obj, out.all5,out.all10,out.all.last, out.all, out.final)
rm( perfInd.pil, auxiliary.f, tacdif)

