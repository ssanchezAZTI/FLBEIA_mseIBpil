################################################################################
#  IBpil MSE simulations                                                       # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  12/07/2018                                                       #
#   modified: Laura Wise                                                       #
################################################################################

# ibpil_simulations.r - MSE simulations
# msePIL8c9a/02_pil_simulations.r

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


rm(list=ls())


# #==============================================================================
# # WORKING DIRECTORY                                                        ----
# #==============================================================================
# 
 wd <- "C:/use/GitHub/FLBEIA_mseIBpil/"
 setwd(wd)


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

library(FLBEIA)
library(FLash)
library(r4ss)
library(reshape2)


#==============================================================================
# LOAD DATA                                                                ----
#==============================================================================

load(file.path("input","PIL_input2018.RData"))

#==============================================================================

#==============================================================================


t1 <- Sys.time()

 for(rec.sc in c("low","mix","med")){
   for(it in 1:100){
 
# get the job id from the environment
#it <- as.numeric(Sys.getenv("SGE_TASK_ID")) #it <- 1
cat("Starting run i = ",it,"\n")

# Set seed for reproducible results:
set.seed(it)

nit <- 1
  

  
#==============================================================================
# SCENARIO                                                                ----
#==============================================================================

# Cases: combination of HCRs (rules), assessment

# ASSESSMENT
# - ASSnone : no assessment + perfect observation
# - ASSemul : emulator of the assessment (observation error in numbers at age)
# - ASSss3  : assess with SS3 (with observation errors in population, harvest and indices)

  ass.sc <- "none" # "emul", "ss3"

# HCRs (pilHCRs variants)
# - HCR0: ICES HCR (F-based HCR in absolute terms)
# - HCR1: Preferred F
# - HCR2: Increasing F

  rule.sc <- 5 #1

# RECRUITMENT
# - REClow: low recruitment regime
# - RECmed: medium recruitment regime
# - RECmix: changing recruitment regime

  #rec.sc <- "mix" # "med", "low"

# INITIAL NUMBERS AT AGE
# - INNfix: same initial numbers at age for all iterations
# - INNvar: different initial numbers at age for all iterations
  
  inn.sc <- "fix" # "var"
  
# OBSERVATION ERROR
# - OERnone  : perfect observation
# - OERnaq   : observation error in numbers at age and survey catchabilities
  
  oer.sc <- "none" # "naq"

  
# List of all the scenarios:
  
  # scenario_list <- c()
  # for (ass in c("none","emul","ss3")) for (rule in c(1:2)) for (rec in c("low","med","mix"))
  #   for (inn in c("none","var")) for (oer in c("none","naq"))
  #     scenario_list <- c( scenario_list, paste("ASS",ass,"_HCR",rule,"_REC",rec,"_INN",inn,"_OER",oer,sep=""))


#==============================================================================
# FLBEIA input objects (for selected scenario)                             ----
#==============================================================================

  ages     <- dimnames(biols[[1]]@n)$age
  yrs      <- dimnames(biols[[1]]@n)$year
  proj.yrs <- ac(main.ctrl$sim.years[1]:main.ctrl$sim.years[2])
  hist.yrs <- yrs[!yrs %in% proj.yrs]
    
  
  scenario <- paste("ASS",ass.sc,"_HCR",rule.sc,"_REC",rec.sc,"_INN",inn.sc,"_OER",oer.sc,sep="")
  
  cat("\n \n")
  cat("#######################################################\n")
  cat("####  SCENARIO: ", scenario, " ####\n")
  cat("#######################################################\n")
  
  
  # ASSESSMENT
  
  if (ass.sc=="none") {
    
    obs.ctrl    <- obs.ctrl_perf
    assess.ctrl <- assess.ctrl_perf
    
    indices <- indices_none
    
  } else if (ass.sc=="emul") {
    
    obs.ctrl    <- obs.ctrl_perf
    assess.ctrl <- assess.ctrl_emul
    
    indices <- indices_none
    
  } else if (ass.sc=="ss3") {
    
    obs.ctrl    <- obs.ctrl_SS3
    
    assess.ctrl <- assess.ctrl_SS3
    
    assess.ctrl[["PIL"]]$control$ref_name <- "ss3R"
    
    indices <- list(PIL = indices_ss3)
    
    
  } else
    stop("Check values in ass.sc")
  
  
  # HCRs: trigger points required for HCR1 and HCR2 (i.e. Bloss, Blim, Floss, Flow and Fmsy)
  
  advice.ctrl[["PIL"]]$rule    <- rule.sc
  
  if (rule.sc==1 | rule.sc==3 | rule.sc==5) {
    
    if(any(!c("Bloss","Blim","Floss","Fmsy") %in% rownames(advice.ctrl$PIL$ref.pts)))
      stop("Required reference points missing: Bloss, Blim, Floss, Fmsy")
    
  } else if (rule.sc==2 | rule.sc==4 | rule.sc==6) {
    
    if(any(!c("Bloss","Blim","Flow","Fmsy") %in% rownames(advice.ctrl$PIL$ref.pts)))
      stop("Required reference points missing: Bloss, Blim, Flow, Fmsy")
    
  } #else
    
    #stop("Check values in rule.sc")
  
  
  # RECRUITMENT
  
  if (rec.sc=="med"){
    
    SRs <- SRs_MED
    # - uncertainties in SR
    # Simulate from a lognormal distribution (mean=0, var=same as the estimated in SR model fitting)
    SRs$PIL@uncertainty[,proj.yrs,,,] <-
      exp(rnorm(length(proj.yrs), 0, residsd_med))

  } else if (rec.sc=="low"){
    
    SRs <- SRs_LOW
    #advice.ctrl$PIL$ref.pts["Blim",] <- Blow # different Blim LAST CHANGE
    # Simulate from a lognormal distribution (mean=0, var=same as the estimated in SR model fitting)
    SRs$PIL@uncertainty[,proj.yrs,,,] <-
      exp(rnorm(length(proj.yrs), 0, residsd_low))
    
    
  } else if (rec.sc=="mix"){

    SRs <- SRs_MIX
    # Simulate from a lognormal distribution (mean=0, var=same as the estimated in SR model fitting)
    SRs$PIL@uncertainty[,proj.yrs,,,] <-
      exp(rnorm(length(proj.yrs), 0, residsd_low))
    SRs[["PIL"]]@covar$uncAdd[,ac(proj.yrs),] <- SRs[["PIL"]]@uncertainty[,ac(proj.yrs),] ^(residsd_med/residsd_low-1)
    
  } else
    
    stop("Check values in rec.sc")
  

  # INITIAL NUMBERS AT AGE
  
  if (inn.sc == "var") {
  
    initpop.yrn <- ac(main.ctrl$sim.years["initial"]-1)
    
    # Simulate from a lognormal distribution (mean=0, CV=assessment CV in 2018 log-numbers-at-age)
    cv_logN <- c(0.29,0.19,0.20,0.22,0.21,0.26,0.26)
    biols$PIL@n[,initpop.yrn,] <- biols$PIL@n[,initpop.yrn,] * 
      rlnorm( length(ages), meanlog = 0, sdlog = sqrt(log(cv_logN^2+1)))
    
  } else if (inn.sc != "fix")
    stop("Not valid value for inn.sc")


# OBSERVATION ERROR
if (oer.sc=="naq") {
  
  # obsevation errors in numbers at age
  # obsevation errors in survey catchabilities
  # obsevation errors in catches at age or any other???
  
} else if (oer.sc != "none")
  stop("Not valid value for oer.sc")


#==============================================================================
# FLBEIA input objects (uncertainty)                                       ----
#==============================================================================

# -  biols
  
# - SRs

# -  BDs

# -  advice:TAC/TAE/quota.share

# -  covars

# -  indices
if (ass.sc == "ss3") {
  
  # # Uncertainty in the XXX index for the projection period (different for each iteration)
  # nit <- dim(biols[[1]]@n)[6]
  # indices[["PIL"]]$ANE.B0@index.var[] <- rlnorm(2*length(proj.yrs)*nit, 0, sqrt(log(1+cv.cbbm^2)))
  
}

# -  main.ctrl

# -  biols.ctrl

# -  fleets.ctrl

# -  advice.ctrl

# -  assess.ctrl

# -  obs.ctrl

# -  covars.ctrl


#==============================================================================
# FLBEIA RUN                                                               ----
#==============================================================================

source("./R/fun/MP_HCR_IBpil.R")
source("./R/fun/segregmix.R")

# source("./R/fun/PILassess.R")
# source("./R/fun/ss32flbeia.R")

proj.obj <- FLBEIA( biols = biols, SRs = SRs, fleets = fleets, covars = covars, indices = indices, advice = advice, 
                    main.ctrl = main.ctrl, biols.ctrl = biols.ctrl,  fleets.ctrl = fleets.ctrl, 
                    covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)


assign(scenario, proj.obj)


#==============================================================================
#  Indicators
#==============================================================================

assign( paste(scenario,"bio",sep="_"), bioSum(proj.obj, long=F, scenario=scenario))
assign( paste(scenario,"flt",sep="_"), fltSum(proj.obj, long=F, scenario=scenario))
assign( paste(scenario,"fltstk",sep="_"), fltStkSum(proj.obj, long=F, scenario=scenario))
assign( paste(scenario,"adv",sep="_"), advSum(proj.obj, long=F, scenario=scenario))

# Get reference points for risks calculation:
assign( paste(scenario,"risk",sep="_"), riskSum(proj.obj, Bpa=c(PIL=advice.ctrl$PIL$ref.pts["Bpa",]), 
                                                Blim=c(PIL=advice.ctrl$PIL$ref.pts["Blim",]), 
                                                Prflim=c(INT=NA), scenario=scenario))


#==============================================================================
#  Save outputs
#==============================================================================

out.obj <- c( scenario, paste( scenario, c("bio","flt","fltstk","adv","risk"), sep="_"))

out.name      <- paste("out2018_",scenario,"_",it,".RData",sep="")
out.file      <- file.path("temporal_output/output_iters",out.name)

cat("Saving objects in: ",out.file,"\n")
save(list=out.obj, file=out.file)

    
# Saving the names of the scenarios runned
if (it==1) {
  if (!file.exists(file.path("temporal_output","scenario_list.RData"))) {
    scenario_list <- scenario
    save(scenario_list, file=file.path("temporal_output","scenario_list.RData"))
  } else {
    load(file.path("temporal_output","scenario_list.RData"))
    scenario_list <- unique(c(scenario_list, scenario))
    save(scenario_list, file=file.path("temporal_output","scenario_list.RData"))
  }
  
}


t2 <- Sys.time()

t2 - t1
}
  }

