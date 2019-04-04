################################################################################
#  IBpil results - summary statistics (joining scenarios)                     # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  03/10/2018                                                       #
#   modified: Laura Wise assess 2018                                           #
################################################################################

# ibpil_results_sumStats_2018.r - summary statistics for all scenarios
# msePIL8abd/ibpil_results_sumStats_2018.r

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

t1 <- Sys.time()


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
setwd(wd)

# directory with results
res.dir <- file.path("./temporal_output")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

library(FLBEIA)
library(R.utils)
library(data.table)


#==============================================================================
# SCENARIOS                                                                ----
#==============================================================================

load(file.path(res.dir, "scenario_list.RData"))

# names of the scenarios
scenario_list


#==============================================================================
#  Results:                                                                ----
#==============================================================================

# join results of all scenarios

# - bio.q
###########

dat.bio.q <- data.frame()

for (cs in scenario_list){
  
  # load file
  obj <- loadToEnv( file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep="")))[["out.bio"]]
  
  # combine all cases
  dat.bio.q <- rbind( dat.bio.q, bioSumQ(obj))
  rm(obj)
  
}

# Convert year in numeric
dat.bio.q$year    <- as.numeric(as.character(dat.bio.q$year))

# Separate scenario into different columns
scenario   <- as.character(dat.bio.q$scenario)
assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
rec        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))

dat.bio.q <- cbind( scenario, assessment, rule, rec, initNage, obsErr, dat.bio.q[,-c(1,3)])

# Save data
save( dat.bio.q, file=file.path(res.dir,"res_bio_all2018.RData"))
rm( cs, dat.bio.q)


# - eco.q
###########

dat.eco.q <- data.frame()

for (cs in scenario_list){
  
  # load file
  obj <- loadToEnv( file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep="")))[["out.flt"]]
  
  # combine all cases
  dat.eco.q <- rbind( dat.eco.q, fltSumQ(obj))
  rm(obj)
  
}

# Convert year in numeric
dat.eco.q$year    <- as.numeric(as.character(dat.eco.q$year))

# Separate scenario into different columns
scenario   <- as.character(dat.eco.q$scenario)
assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
rec        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))

dat.eco.q <- cbind( scenario, assessment, rule, rec, initNage, obsErr, dat.eco.q[,-c(1,3)])

# Save data
save( dat.eco.q, file=file.path(res.dir,"res_eco_all2018.RData"))
rm( cs, assessment, rule, rec, initNage, obsErr, dat.eco.q)


# - adv.q
###########

dat.adv.q <- data.frame()

for (cs in scenario_list){
  
  # load file
  obj <- loadToEnv( file.path(res.dir,"output_scenarios",paste("results2018_",cs,".RData",sep="")))[["out.adv"]]
  
  # combine all cases
  dat.adv.q <- rbind( dat.adv.q, advSumQ(obj))
  rm(obj)
  
}

# Convert year in numeric
dat.adv.q$year    <- as.numeric(as.character(dat.adv.q$year))

# Separate scenario into different columns
scenario   <- as.character(dat.adv.q$scenario)
assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
rec        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))

dat.adv.q <- cbind( scenario, assessment, rule, rec, initNage, obsErr, dat.adv.q[,-c(1,3)])

# Save data
save( dat.adv.q, file=file.path(res.dir,"res_adv_all2018.RData"))
rm( cs, assessment, rule, rec, initNage, obsErr, dat.adv.q)


# # - risks
# ###########
# 
# risks <- data.frame()
# 
# for (cs in scenario_list){
#   
#   # load file
#   obj <- loadToEnv( file.path("output/output_scenarios",paste("results2018_",cs,".RData",sep="")))[["out.risk"]]
#   
#   # estimate all risk types
#   dat <- subset(obj, indicator=="pBlim")
#   dat$indicator <- NULL
#   
#   risk1 <- aggregate( value ~ scenario + unit, mean, data=dat)
#   risk2 <- aggregate( value ~ scenario + unit + iter, function(x) ifelse(sum(x)==0,0,1), data=dat)
#   risk2 <- aggregate( value ~ scenario + unit, mean, data=risk2)
#   risk3 <- aggregate( value ~ scenario + unit + year, mean, data=dat)
#   risk3 <- aggregate( value ~ scenario + unit, max, data=risk3)
#   
#   # combine all cases
#   risks <- rbind( risks, cbind(risk1[,c("scenario","unit")], risk1=risk1$value, risk2=risk2$value, risk3=risk3$value))
#   rm(obj, risk1, risk2, risk3)
#   
# }
# 
# # Separate scenario into different columns
# scenario   <- as.character(dat.bio.q$scenario)
# assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
# rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
# rec        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
# initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
# obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))
# 
# risks <- cbind( scenario, assessment, rule, rec, initNage, obsErr, risks[,-c(1:2)])
# 
# # Save data
# save( risks, file="output/res_risks_all2018.RData")
# rm( cs, assessment, rule, rec, initNage, obsErr, risks)

