################################################################################
#  IBpil MSE simulations                                                       # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  12/07/2018                                                       #
#   modified: Laura Wise
#   modified: Leire Citores (to run with ss32flbeia)
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
# wd <- "C:/use/GitHub/FLBEIA_mseIBpil/"
# setwd(wd)


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


# get the job id from the environment
it <- as.numeric(Sys.getenv("SGE_TASK_ID")) #it <- 1
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

  ass.sc <- "ss3" #"none" # "emul", "ss3"

# HCRs (pilHCRs variants)
# - HCR0: ICES HCR (F-based HCR in absolute terms)
# - HCR1: Preferred F
# - HCR2: Increasing F

  rule.sc <- 1 #2

# RECRUITMENT
# - REClow: low recruitment regime
# - RECmed: medium recruitment regime
# - RECmix: changing recruitment regime

  rec.sc <- "mix" #"mix" # "med", "low"

# INITIAL NUMBERS AT AGE
# - INNfix: same initial numbers at age for all iterations
# - INNvar: different initial numbers at age for all iterations
  
  inn.sc <- "var" # "fix"
  
# OBSERVATION ERROR
# - OERnone  : perfect observation
# - OERnaq   : observation error in numbers at age and survey catchabilities
  
  oer.sc <- "naq" # "naq" none

  
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
    
    #assess.ctrl[["PIL"]]$control$ref_name <- "ss3R"
    
    indices <- list(PIL = indices_ss3)
    
    assess.ctrl[["PIL"]]$control$run_it<-paste0(it,scenario)
    assess.ctrl[["PIL"]]$control$ref_name <- "assess_ref"
    assess.ctrl[["PIL"]]$control$assess_dir <- "/home/lcitores/FLBEIA_mseIBpil/ss3R/"
    k<-1000
    
    
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
  
  conv_ss3<-1
  while(conv_ss3!=0){
    cat("k is ",k)
  
  if (rec.sc=="med"){
    
    SRs <- SRs_MED
    # - uncertainties in SR
    # Simulate from a lognormal distribution (mean=0, var=same as the estimated in SR model fitting)
    SRs$PIL@uncertainty[,proj.yrs,,,] <-
      exp(rnorm(length(proj.yrs), 0, residsd_med))

  } else if (rec.sc=="low"){
    
    SRs <- SRs_LOW

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
  #! A SER POSIBLE CALCULAR LOS ERRORES EN EL FICHERO 01 Y GUARDARLOS EN EL FICHERO DE INPUTS PARA ESTE CODIGO
  #! ASI TENDREMOS TRAZABILIDAD DE LOS DATOS
  
  
  
  # obsevation errors in survey catchabilities
  
  #! LO MISMO QUE ANTES PARA LA TRAZABILIDAD: intentar calcularlo dentro del 01 si es posible (hecho)
  
  #CATCH NUMBERS obs errors
  obs.ctrl$PIL$stkObs$land.nage.error[,]<-
    rlnorm(prod(dim(obs.ctrl_SS3$PIL$stkObs$land.nage.error)[1:2]), 
           meanlog = catch_resid_logmeans, 
           sdlog = catch_resid_logsds)
  
  
  #SURVEYS obs errors
  # Simulate from a lognormal distribution
    ##AcouSticN
  # > apply(log(AcousticN/natage),2,mean)
  # a0          a1          a2          a3          a4          a5          a6 
  # -Inf  0.03363665 -0.06660685 -0.03524329  0.18057145  0.35486293 -0.16620986 
  # > apply(log(AcousticN/natage),2,sd)
  # a0        a1        a2        a3        a4        a5        a6 
  # NaN 0.4836377 0.5184707 0.4399987 0.5134851 0.6282970 0.7162383 
  
  AcousticN.errors<-indices$PIL$AcousticNumberAtAge@index.q
  AcousticN.errors[,]<-rlnorm(length(ages)*dim(AcousticN.errors)[2], 
                              meanlog = (c(0,0.03363665, -0.06660685, -0.03524329,  0.18057145,  0.35486293, -0.16620986 )), 
                              sdlog = c(0,0.4836377, 0.5184707, 0.4399987, 0.5134851, 0.6282970, 0.7162383))
  indices$PIL$AcousticNumberAtAge@index.q[,] <- AcousticN.errors[,]
  indices$PIL$AcousticNumberAtAge@index.q[1,]<-0
  
  ##DEPM cv=0.25 -> sd= sqrt(log(cv^2+1)) =
  sd_DEPM<-sqrt(log(0.25^2+1))
  DEPM.errors<-indices$PIL$DEPM@index.q
  DEPM.errors[,]<-rlnorm(dim(DEPM.errors)[2], meanlog = 0, sdlog = sd_DEPM)
  indices$PIL$DEPM@index.q[,] <- indices$PIL$DEPM@index.q[,]  * DEPM.errors[,]
  
  #update historical indices values to incorporate errors
  
  #AcousticNumberAtAge = stock.n * index.q (*index*0+1) too keep the NAs in the historical data
  indices_ss3$AcousticNumberAtAge@index[,ac(hist.yrs)]<-biols$PIL@n[,ac(hist.yrs)]*indices_ss3$AcousticNumberAtAge@index.q[,ac(hist.yrs)]*(indices_ss3$AcousticNumberAtAge@index[,ac(hist.yrs)]*0+1)
  
  #biomas * index.q (weight in the stock for age 0 is 0)
  indices_ss3$DEPM@index[,ac(hist.yrs)]<-ssb(biols$PIL)[,ac(hist.yrs)]*indices_ss3$DEPM@index.q[,ac(hist.yrs)]*(indices_ss3$DEPM@index[,ac(hist.yrs)]*0+1)
  
  
  
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
 source("./R/fun/ss32flbeia.R")
    
    ##initialize covars
    covars$ssb<-covars$fbar<-covars$rec<-FLQuant(NA, dimnames=list(eval.year=c("2017",proj.yrs), year=yrs))
    covars$ssb[ac(2017),]<-ssb(biols$PIL)[,]
    covars$rec[ac(2017),]<-(biols$PIL@n)[1,]
    covars$qs<-FLQuant(NA, dimnames=list(qs=c("acoustic","depm"), year=yrs))
    covars$sel<-FLQuant(NA, dimnames=list(age=0:6, year=yrs,unit=1:3))
    covars$conv<-FLQuant(NA, dimnames=list(conv="conv",year=yrs))
    

proj.obj <- FLBEIA( biols = biols, SRs = SRs, fleets = fleets, covars = covars, indices = indices, advice = advice, 
                    main.ctrl = main.ctrl, biols.ctrl = biols.ctrl,  fleets.ctrl = fleets.ctrl, 
                    covars.ctrl = covars.ctrl, obs.ctrl = obs.ctrl, assess.ctrl = assess.ctrl, advice.ctrl = advice.ctrl)

if (ass.sc!="ss3") {conv_ss3<-0}
if (ass.sc=="ss3") {
  conv_ss3<-sum(proj.obj$covars$conv[,]>0.001,na.rm=T)
  k<-k+1
  set.seed(it*k)
}
}

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
out.file      <- file.path("output/output_iters",out.name)

cat("Saving objects in: ",out.file,"\n")
save(list=out.obj, file=out.file)

    
# Saving the names of the scenarios runned
if (it==1) {
  if (!file.exists(file.path("output","scenario_list.RData"))) {
    scenario_list <- scenario
    save(scenario_list, file=file.path("output","scenario_list.RData"))
  } else {
    load(file.path("output","scenario_list.RData"))
    scenario_list <- unique(c(scenario_list, scenario))
    save(scenario_list, file=file.path("output","scenario_list.RData"))
  }
  
}


t2 <- Sys.time()

t2 - t1

