################################################################################
#  IBpil postsims (joining iterations - all scenarios)                        # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  03/10/2018                                                       #
#   modified: 2019-01-09 Laura Wise (new scenario - assess2018)  #                                                                #
################################################################################

# ibpil_postsims_all_2018.r - joining iterations by scenario
# msePIL8abd/ibpil_postsims_all_2018.r

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

t1 <- Sys.time()

# get the job id from the environment
# it <- as.numeric(Sys.getenv("SGE_TASK_ID"))
# cat("Starting run i = ",it,"\n")

# length(scenario_list)
# [1] 256

ni <- 1000


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
library(ggplot2)
library(FLBEIA)
library(R.utils)


#==============================================================================
# SCENARIOS                                                                ----
#==============================================================================

load(file.path(res.dir, "scenario_list.RData"))

length(scenario_list)

# names of the scenarios
# scenario_list


#==============================================================================
#  Join the results (summary statistics):                                  ----
#==============================================================================
# code adapted from Leire Ibaibarriaga's version

# loop to join iterations for each case
# be patient! it takes time!

# cs <- scenario_list[it]
for (cs in scenario_list){
  
  # print case
  
  print(paste("Working on case", cs))
  print(Sys.time())
  
  
  # join iterations from cluster
  
  out.bio    <- NULL
  out.flt    <- NULL
  # out.fltstk <- NULL
  out.adv    <- NULL
  out.risk   <- NULL
  
  for (i in 1:ni){ #1:ni
    print(paste("     iter",i))
    if (file.exists(file.path(res.dir, "output_iters", paste("out2018_",cs,"_",i,".RData",sep="")))){
      load(file.path(res.dir, "output_iters", paste("out2018_",cs,"_",i,".RData",sep="")))
      
      rm(list=cs)
      # steps:
      # get the objects for each iteration
      # remove loaded objects (not anymore needed)
      # set the correct name for the iterations
      # and join the objects
      
      d.bio    <- get(paste(cs,"bio",sep="_"))
      rm(list=paste(cs,"bio",sep="_"))
      d.bio$iter <- i
      out.bio    <- rbind(out.bio, d.bio)
      rm(d.bio)
      
      d.flt    <- get(paste(cs,"flt",sep="_"))
      rm(list=paste(cs,"flt",sep="_"))
      d.flt$iter <- i
      out.flt    <- rbind(out.flt, d.flt)
      rm(d.flt)
      
      # d.fltstk <- get(paste(cs,"fltstk",sep="_"))
      # rm(list=paste(cs,"fltstk",sep="_"))
      # d.fltstk$iter <- i
      # out.fltstk <- rbind(out.fltstk, d.fltstk)
      # rm(d.fltstk)
      
      d.adv    <- get(paste(cs,"adv",sep="_"))
      rm(list=paste(cs,"adv",sep="_"))
      d.adv$iter <- i
      out.adv    <- rbind(out.adv, d.adv)
      rm(d.adv)
      
      d.risk   <- get(paste(cs,"risk",sep="_"))
      rm(list=paste(cs,"risk",sep="_"))
      d.risk$iter <- i
      out.risk   <- rbind(out.risk, d.risk)
      rm(d.risk)
      
      rm(list=ls()[grep( cs, ls(), fixed=TRUE)])
      
    } else stop("Missing file:", file.path(res.dir, "output_iters", paste("out2018_",cs,"_",i,".RData",sep="")))
  }
  
  
  # save results in external files
  
  save( out.bio, out.flt, out.adv, out.risk, #out.fltstk, 
        file=file.path(res.dir, "output_scenarios", paste("results2018_",cs,".RData",sep="")))
}

print(Sys.time())

t2 <- Sys.time()

t2 - t1

rm( list=ls()[!ls() %in% c("scenario_list","res.dir","ni")])


