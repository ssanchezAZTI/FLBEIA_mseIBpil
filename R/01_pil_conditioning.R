################################################################################
#  IBpil MSE conditioning                                                     # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  03/07/2018                                                       #
#   modified: Laura Wise (assessment WGHANSA2018)                            #
################################################################################

# ibpil_conditioning_2018.r - MSE conditioning and settings
# msePIL8c9a/ibpil_conditioning_2018.r

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
# ASSUMPTIONS - COMPARISON TO WGHANSA2018 (i.e. the BC)                     ----
#==============================================================================

# - REFERENCE POINTS: taken WGHANSA2018 ones
# - HCR trigger points: recalculated based on new reference points
# - PROJECTION PERIOD values:
#   ~ @mat     : mean 2012-2017 (0 for age 0 and 1 for the rest)
#   ~ @stock.wt: mean 2012-2017 
#   ~ @catch.wt: mean 2012-2017 
#   ~ @catch.q : mean 2012-2017 

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

rm(list=ls())

wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory

setwd(wd)

wd_dat  <- paste( wd, "data/", sep="")
wd_plot <- paste( wd, "plots/", sep="")
wd_in   <- paste( wd, "input/", sep="")
wd_out  <- paste( wd, "output/", sep="")


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================
library(FLBEIA)
# library(openxlsx)
# library(dplyr)
#library(plyr)
#library(R.utils)
#library(ggplot2)

source("./R/fun/segregmix.R")


#==============================================================================
# SIMULATION PARAMETERS                                                    ----
#==============================================================================

first.yr <- 1978  # First year of the historic data.
proj.yr  <- 2019  # first year of projection
proj.nyr <- 50    # Number of years in the projection period

#---- periods ----

hist.yrs <- first.yr:(proj.yr-1)   # historical period
last.yr  <- proj.yr + (proj.nyr-1) # Last year of projection
proj.yrs <- proj.yr:last.yr        # projection period
ass.yr <- proj.yr-1                # assessment year

# information for FLBEIA

yrs <- c(first.yr=first.yr,proj.yr=proj.yr,last.yr=last.yr) 

# seasons and iterations

ni <- 1 #! coded for 1 iteration
ns <- 1

#==============================================================================
# LOAD HISTORICAL DATA (assessment + indices)                              ----
#==============================================================================

load(paste(wd_dat,"pil.stock.RData",sep=""))

#change stock.n at age zero/rec in year 2018 to the geometric mean of the last five years as in the stock annex
stock.n(pil.stock)[1,ac(ass.yr)] <- exp(mean(log(rec(pil.stock)[,ac((ass.yr-5):(ass.yr-1))])))

#indices needed for SS3
#import abundances indices data
 
# indices01 <- read.table(file.path(wd_dat, "acousticSurveyAgeData.txt"))
# indices01 <- t(indices01[,c(10:16)])
# 
# #Not to use like this
# #indices02 <- read.table(file.path(wd_dat, "acousticSurveyTotalNumber.txt"))
# #indices02 <- t(indices02[,4])
# 
# indices02 <- read.table(file.path(wd_dat, "depmSurveyBiomassData.txt"))
# indices02 <- t(indices02[,4])
# 
# # transform into FLQuant
# #indices02 <- FLQuant(as.matrix(indices02), dimnames=list(number='oneplus', year = 1996:2018))
# indices01 <- FLQuant(as.matrix(indices01), dimnames=list(age=0:6, year = 1996:2018))
# indices02 <- FLQuant(as.matrix(indices02), dimnames=list(biomass='ssb', year = 1997:2018))
# 
# # and into FLIndex
# # indices02 <- FLIndex(index = indices02,name="AcousticTotalNumber")
# # units(index(indices02)) <- 'number'
# # range(indices02)[c("startf","endf")] <- c(0.20,0.25) # Time of survey relative to year
# 
# indices01 <- FLIndex(index = indices01,name="AcousticNumberAtAge")
# units(index(indices01)) <- 'number'
# range(indices01)[c("startf","endf")] <- c(0.20,0.25) # Time of survey relative to year
# 
# indices02 <- FLIndex(index = indices02,name="DEPM")
# units(index(indices02)) <- 'tonnes'
# range(indices02)[c("startf","endf")] <- c(0.20,0.25) # Time of survey relative to year
# 
# # and then into FLIndices
# indices <- FLIndices(indices01, indices02)#,indices03)
# 
# 
# save(indices,file = paste(wd_in,'pilFLindices.RData',sep=""))

load(paste(wd_dat,"pilFLindices.RData",sep=""))
indices <- window(indices, start=first.yr, end=last.yr )

#==============================================================================
# BIOLOGICAL DATA                                                          ----
#==============================================================================

#---- stocks ----

stks <- "PIL"

# stock PIL
PIL.age.min  <- 0
PIL.age.max  <- 6
PIL.unit     <- 1
PIL_range.plusgroup <- PIL.age.max
PIL_range.minyear   <- first.yr
PIL_range.minfbar   <- 2
PIL_range.maxfbar   <- 5


# PIL - Iberian sardine
# - directly from FLStock (afterwards needs to extend up to last.yr, for example with stf function)

pil <- FLBiol( n=stock.n(pil.stock), wt=stock.wt(pil.stock), m=m(pil.stock), spwn=m.spwn(pil.stock),
               mat=predictModel(FLQuants(mat=pil.stock@mat), model=~mat),
               fec=predictModel(FLQuants(fec=pil.stock@mat*0+1), model=~fec),
               name="PIL", desc=pil.stock@desc, range=pil.stock@range)

units(pil)$m <- units(fec(pil)) <- units(mat(pil)) <- ""

pil <- window(pil, start=first.yr, end=last.yr ) # now need to fill wt, m, mat, fec, spwn in proj.yrs

pil@desc <- "WGHANSA2018 assessment output"


#---- FLBiols object ---- 

biols <- FLBiols(PIL = pil)


#============================================================================
# Projection period
#============================================================================

# Values set as the mean of the last 6 years (mean.yrs = hl6.yr)

# - Natural mortality: constant along the time series --> ok
m(biols$PIL)[,ac(proj.yrs),] <- m(biols$PIL[,ac(2017)])

# - Weights at age in the stock: decreasing trend for all ages --> mean 2012-2017 (last 6 years)
myrs <- ac(2012:2017)
# statistics
PILstwa.mean <- yearMeans(wt(biols$PIL)[,myrs,])
round(PILstwa.mean,3)[drop=T]
# 2012-2017
# 0     1     2     3     4     5     6 
# 0.000 0.031 0.049 0.064 0.066 0.075 0.079 

wt(biols$PIL)[,ac(proj.yrs),] <- PILstwa.mean

# - Maturity at age: no patterns --> mean 1993-2017 (i.e. over the whole time series)
myrs <- ac(hist.yrs)
# statistics
PILmat.mean <- yearMeans(mat(biols$PIL)[,myrs,])
round(PILmat.mean,5)[drop=T]
#knife-edge ogive
# 0 1 2 3 4 5 6 
# 0 1 1 1 1 1 1

mat(biols$PIL)[,ac(proj.yrs),] <- PILmat.mean

# - Fecundity
fec(biols$PIL)[,ac(proj.yrs),] <- c(rep(1,7))

# - Spawning
spwn(biols$PIL)[,ac(proj.yrs),] <- c(rep(0,7))

#============================================================================
# Reference points
#============================================================================

units(biols$PIL@n*wt(biols$PIL))
# [1] "t"

# reference points for PIL (tons) - WGHANSA2017
Fpa  <- 0.190 
Flim <- 0.250 
Bpa  <- 446331 
Blim <- 337448 
Bloss <- 112943
Fmsy <- 0.120 
Flow <- 0.10
Bmsy <- 385038

PIL_ref.pts <- c( Fpa=Fpa, Flim=Flim, Flow=Flow, Bpa=Bpa, Blim=Blim, Fmsy=Fmsy, Bmsy=Bmsy, Bloss=Bloss)

#==============================================================================
# BIOLS CONTROLS                                                           ----
#==============================================================================

growth.model <- c("ASPG")
biols.ctrl   <- create.biols.ctrl (stksnames=stks,growth.model=growth.model)


#==============================================================================
# SRs and BDs DATA                                                         ----
#==============================================================================

# Stock - recruitment (for age-structured stocks) ----

# Segmented regression (inflexion point at Blim from WGHANSA 2017)

pilsr <- FLSR(rec=pil@n[1,], ssb=ssb(pil))

pilsr@rec[,ac(ass.yr),] <- NA  # remove REC estimate for assessment year (11069700)


model(pilsr) <- segreg()

#params for sr medium regime
yrs.srmed <- 1993:2015
params_med <- fmle(window(pilsr, start=1993, end=2015), fixed=list(b=PIL_ref.pts[["Blim"]]))@params # for pilsr

pilsr <- fmle(pilsr, fixed=list(a=c(params_med)[1],b=PIL_ref.pts[["Blim"]]))

# pdf(paste(wd_plot,"PIL_srr_fit2017.pdf",sep=""))
# plot(pilsr, main="Segmented regression \n(inflex. at Blim)")
# #Profile the likelihood to check the fit
# profile(pilsr, main="Segmented regression \n(inflex. at Blim)")
# dev.off()

# Segmented regression (inflexion point at Blow for low productivity scenario)
Blow <- 196334 #change point of Hockey stick for the low productivity 

pilsr2 <- FLSR(rec=pil@n[1,], ssb=ssb(pil))

pilsr2@rec[,ac(ass.yr),] <- NA  # remove REC estimate for assessment year (11069700)

model(pilsr2) <- segreg()

yrs.srlow <- 2006:2017
params_low <- fmle(window(pilsr2, start=2006, end=2017), fixed=list(b=Blow))@params 

pilsr2 <- fmle(pilsr2, fixed=list(a=c(params_low)[1],b=Blow))

# pdf(paste(wd_plot,"PIL_srr_Blow.pdf",sep=""))
# plot(pilsr2, main="Segmented regression \n(inflex. at Blow)")
# #Profile the likelihood to check the fit
# profile(pilsr2, main="Segmented regression \n(inflex. at Blow)")
# dev.off()

# Mixture of both SRs
pilsr3 <- FLSR(rec=pil@n[1,], ssb=ssb(pil))

pilsr3@rec[,ac(ass.yr),] <- NA  # remove REC estimate for assessment year (11069700) 

model(pilsr3) <- segregmix()
params_low
params_med

yrs.srmix <- 1993:2017

pilsr3 <- fmle(pilsr3, fixed=list( a=c(params_low)[1], b=Blow, 
                                   A=c(params_med)[1], B=PIL_ref.pts[["Blim"]]))



#==============================================================================
# FLBEIA input object: SRs
#==============================================================================

# - Hockey - stick with inflexion point at Blim == mean productivity scenario

SRs_MED   <- list(PIL=FLSRsim(name = "PIL", model = "segreg",
                            rec = rec(pilsr), ssb = ssb(pilsr),
                            params = array( c(params_med),
                                            dim = c(length(params(pilsr)), dim(rec(pilsr))[c(2,4,6)]),
                                            dimnames = list(param = dimnames(params(pilsr))$params, year = ac(first.yr:last.yr), season = 1, iter = ni)),
                            uncertainty = exp(residuals(pilsr)), proportion = FLQuant( 1, dimnames=dimnames(biols$PIL@n[1,])[-1]),
                            covar = FLQuants(),range=range(pil)[1:5]))

# uncertainty for the projection years

SRs_MED[["PIL"]]@uncertainty[,ac(proj.yrs),] <- exp(rnorm( length(proj.yrs), 0, sqrt(var(log(SRs_MED[["PIL"]]@uncertainty[,ac(yrs.srmed),,,]),na.rm=T))))

# - SRs for low productivity regime
# - Hockey - stick with inflexion point at Blow

SRs_LOW <- list(PIL = FLSRsim(name = "PIL", model = "segreg",
                           rec = rec(pilsr2), ssb = ssb(pilsr2),
                           params = array( c(params_low),
                                           dim = c(length(params(pilsr2)), dim(rec(pilsr2))[c(2,4,6)]),
                                           dimnames = list(param = dimnames(params(pilsr2))$params, year = ac(first.yr:last.yr), season = 1, iter = ni)),
                           uncertainty = exp(residuals(pilsr2)), proportion = FLQuant( 1, dimnames=dimnames(biols$PIL@n[1,])[-1]),
                           covar = FLQuants(),range=range(pil)[1:5]))

# uncertainty for the projection years

SRs_LOW[["PIL"]]@uncertainty[,ac(proj.yrs),] <- exp(rnorm(length(proj.yrs), 0, sqrt(var(log(SRs_LOW[["PIL"]]@uncertainty[,ac(yrs.srlow),,,]),na.rm=T))))

# - SRs for low productivity regime
# - Hockey - stick with inflexion point at Blow

SRs_MIX <- list(PIL = FLSRsim(name = "PIL", model = "segreg",
                              rec = rec(pilsr3), ssb = ssb(pilsr3),
                              params = array( c(pilsr3@params),
                                              dim = c(length(params(pilsr3)), dim(rec(pilsr3))[c(2,4,6)]),
                                              dimnames = list(param = dimnames(params(pilsr3))$params, year = ac(first.yr:last.yr), season = 1, iter = ni)),
                              uncertainty = exp(residuals(pilsr3)), proportion = FLQuant( 1, dimnames=dimnames(biols$PIL@n[1,])[-1]),
                              covar = FLQuants(),range=range(pil)[1:5]))

# uncertainty for the projection years

SRs_MIX[["PIL"]]@uncertainty[,ac(proj.yrs),] <- exp(rnorm(length(proj.yrs), 0, sqrt(var(log(SRs_MIX[["PIL"]]@uncertainty[,ac(yrs.srmix),,,]),na.rm=T))))


# Biomass dynamics (for stocks in biomass) ----

BDs <- NULL


#==============================================================================
# FLEET DATA                                                               ----
#==============================================================================
#Note: we didn't use the function create.fleets.array

#---- fleets ----

fls <- "INT"

#---- metiers ----

INT.mets <- "ALL"

#---- metiers * stocks ----

INT.ALL.stks <- "PIL"


#---- FLFleets object ----
catch <- FLCatchExt(name=names(biols),landings.n = landings.n(pil.stock[,ac(hist.yrs)]), 
                    landings = landings(pil.stock[,ac(hist.yrs)]), landings.wt = landings.wt(pil.stock[,ac(hist.yrs)]),
                    discards.n = discards.n(pil.stock[,ac(hist.yrs)]), discards = discards(pil.stock[,ac(hist.yrs)]),
                    discards.wt = discards.wt(pil.stock[,ac(hist.yrs)]))

catch <- window(catch,start=first.yr,end=last.yr)

catches <- FLCatchesExt(catch)
names(catches) <- INT.ALL.stks

m <- FLMetierExt(catches = catches,name=INT.mets)

# empty FLQuant for filling with right dimensions
fq  <- FLQuant(dimnames = list(year = first.yr:last.yr), quant = 'age')

# blank quants
effmet <- vcost <- fq

# effort share for metier
effmet[,ac(hist.yrs)] <- 1
units(effmet)  <- 'NA'

# vcost for metier
vcost[,ac(hist.yrs)] <- NA
units(vcost)  <- 'NA'

m@effshare  <- effmet
m@vcost <- vcost

metiers <- FLMetiersExt(m)
names(metiers) <- INT.mets

# blank quants with the same dims
eff <- cap <- crw <- cos.fl <- fq

# fleet effort
eff[,ac(first.yr:last.yr)] <- 1

#fleet capacity, assume a high value
cap[,ac(first.yr:last.yr)] <- 5000

# fleet crewshare
crw[,ac(first.yr:last.yr)] <- NA

#fcost
cos.fl[,ac(first.yr:last.yr)] <- NA

fleet <- FLFleetExt(metiers=metiers,name=fls,effort = eff, fcost = cos.fl, capacity = cap, crewshare = crw)

fleets <- FLFleetsExt(fleet)
names(fleets) <- "INT"

# Set the units (PLEASE CHECK if same units are used in biols and fleets)
units(fleets$INT@metiers$ALL@catches$PIL)[c("landings.n","discards.n")]   <- units(biols$PIL)$n
units(fleets$INT@metiers$ALL@catches$PIL)[c("landings.wt","discards.wt")] <- units(biols$PIL)$wt
units(fleets$INT@metiers$ALL@catches$PIL)[c("landings","discards")] <- units(biols$PIL@n*wt(biols$PIL))
units(fleets$INT@metiers$ALL@catches$PIL)[c("alpha","beta","catch.q")] <- "1" 

# SOME CORRECTIONS
fleets$INT@metiers$ALL@catches$PIL@landings.sel[] <- 1
fleets$INT@metiers$ALL@catches$PIL@discards.sel[] <- 0
fleets$INT@metiers$ALL@catches$PIL@discards.wt <- fleets$INT@metiers$ALL@catches$PIL@landings.wt


# Set capacity to a very large value (assumed 1 in hist.yrs)
fleets$INT@capacity[] <- 5000

# Fix alpha and beta to 1
fleets$INT@metiers$ALL@catches$PIL@alpha[,ac(hist.yrs),] <- 1
fleets$INT@metiers$ALL@catches$PIL@beta[,ac(hist.yrs),] <- 1

# Estimate catchability
# fleets <- calculate.q.sel.flrObjs(biols, fleets)
#stock.wt at med year is assumed to be the catch.wt
catch.q.wt <- landings.wt(fleets$INT@metiers$ALL@catches$PIL)

C <- landings.n(fleets[["INT"]]@metiers[["ALL"]]@catches[["PIL"]]) * landings.wt(fleets[["INT"]]@metiers[["ALL"]]@catches[["PIL"]]) + 
  discards.n(fleets[["INT"]]@metiers[["ALL"]]@catches[["PIL"]]) * discards.wt(fleets[["INT"]]@metiers[["ALL"]]@catches[["PIL"]])
B <- n(biols[["PIL"]])*catch.q.wt*exp(-m(biols[["PIL"]])/2)
E <- effort(fleets[["INT"]]) * fleets[["INT"]]@metiers[["ALL"]]@effshare

catch.q(fleets[["INT"]]@metiers[["ALL"]]@catches[["PIL"]])[] <- C / sweep(B, c(2:6), E, "*")
catch.q(fleets[["INT"]]@metiers[["ALL"]]@catches[["PIL"]])[is.infinite(catch.q(fleets[["INT"]]@metiers[["ALL"]]@catches[["PIL"]]))] <- 0

#============================================================================
# Projection period
#============================================================================

# Values set as the mean of the last 6 years (mean.yrs = hl6.yr)

# - Weights at age in the catch (=landings, as no discards): 
#    mean 2012-2017 (last 6 years)
myrs <- ac(2015:2017)
# statistics
PILcwa.mean <- yearMeans(landings.wt(fleets$INT@metiers$ALL@catches$PIL)[,myrs,])
round(PILcwa.mean,3)[drop=T]
# 2015 - 2017
# 0     1     2     3     4     5     6 
# 0.025 0.048 0.065 0.076 0.084 0.089 0.094 

landings.wt(fleets$INT@metiers$ALL@catches$PIL)[,ac(proj.yrs),] <- PILcwa.mean

PILdwa.mean <- yearMeans(discards.wt(fleets$INT@metiers$ALL@catches$PIL)[,myrs,])
round(PILdwa.mean,3)[drop=T]
# 2015 - 2017
# 0     1     2     3     4     5     6 
# 0.025 0.048 0.065 0.076 0.084 0.089 0.094 

discards.wt(fleets$INT@metiers$ALL@catches$PIL)[,ac(proj.yrs),] <- PILdwa.mean

# - Catchability at age (Cobb Douglas): mean 2012-2017 (last 6 years)
myrs <- ac(2012:2017)
# statistics
PILqa.mean <- yearMeans(catch.q(fleets$INT@metiers$ALL@catches$PIL)[,myrs,])
PILqa.mean[drop=T]
# 0        1        2        3        4        5        6 
# 0.171332 0.549744 0.897403 1.000000 1.000000 1.000000 0.802675 

PILqa.var  <- yearVars(catch.q(fleets$INT@metiers$ALL@catches$PIL)[,myrs,])
PILqa.var[drop=T]
# 0 1 2 3 4 5 6 
# 0 0 0 0 0 0 0 
PILqa.cv   <- sqrt(PILqa.var)/yearMeans(catch.q(fleets$INT@metiers$ALL@catches$PIL)[,myrs,])
PILqa.cv[drop=T]
# 0 1 2 3 4 5 6 
# 0 0 0 0 0 0 0 
catch.q(fleets$INT@metiers$ALL@catches$PIL)[,ac(proj.yrs),]  <- PILqa.mean
catch.q(fleets$INT@metiers$ALL@catches$PIL)[,ac(proj.yr-1),] <- PILqa.mean # assessment up to proj.yr-1, but not info on catches in yr-1

#fleets$INT@metiers$ALL@catches$PIL@discards.sel[,ac(proj.yrs),] <- c(rep(1,7))
fleets$INT@metiers$ALL@catches$PIL@landings.sel[,ac(proj.yrs),] <- c(rep(1,7))
fleets$INT@metiers$ALL@effshare[,ac(proj.yrs),] <- 1
fleets$INT@metiers$ALL@catches$PIL@alpha[,ac(proj.yrs),] <- c(rep(1,7))
fleets$INT@metiers$ALL@catches$PIL@beta[,ac(proj.yrs),] <- c(rep(1,7))

#==============================================================================
# FLEETS CONTROLS                                                          ----
#==============================================================================

n.fls.stks       <- 1
fls.stksnames    <- "PIL"
effort.models    <- "SMFB"
effort.restr.INT <- "PIL"
restriction.INT  <- "catch"
catch.models     <- "CobbDouglasAge"
capital.models   <- "fixedCapital"
flq.PIL<- FLQuant(dimnames = list( age = "all", year = first.yr:last.yr, unit = PIL.unit, 
                                   season = 1:ns, iter = 1:ni))

#==============================================================================
# FLBEIA input object: fleets.ctrl
#==============================================================================

fleets.ctrl <- create.fleets.ctrl( fls=fls,n.fls.stks=n.fls.stks,fls.stksnames=fls.stksnames, 
                                   effort.models= effort.models, catch.models=catch.models,
                                   capital.models=capital.models, flq=flq.PIL,
                                   effort.restr.INT = effort.restr.INT, restriction.INT = restriction.INT)

fleets.ctrl$INT$PIL$discard.TAC.OS <- FALSE
# fleets.ctrl$INT$restriction <- "landings"


#==============================================================================
# COVARS DATA                                                              ----
#==============================================================================

covars <- NULL

#==============================================================================
# COVARS CONTROLS                                                          ----
#==============================================================================

covars.ctrl <- NULL


#==============================================================================
# MAIN CONTROLS                                                            ----
#==============================================================================

main.ctrl           <- list()
main.ctrl$sim.years <- c(initial = proj.yr, final = last.yr)


#==============================================================================
# INDICES DATA                                                             ----
#==============================================================================

indices_none <- NULL
indices_ss3 <- indices


#==============================================================================
# OBSERVATION CONTROLS                                                     ----
#==============================================================================

flq.PIL <- FLQuant(dimnames = list(age = PIL.age.min:PIL.age.max, year = first.yr:last.yr, unit = PIL.unit, 
                                   season = 1:ns, iter = 1:ni))

# - Perfect observation:

stkObs.models <- "perfectObs"


#==============================================================================
# FLBEIA input object: obs.ctrl
#==============================================================================

obs.ctrl_perf <- create.obs.ctrl(stksnames = stks,  stkObs.models = stkObs.models, flq.PIL = flq.PIL)


# # - Observation (--> errors in biological + fleet information + indices):
#obs.ctrl_SS3 <- create.obs.ctrl(stksnames = stks,flq.PIL=flq.PIL,stkObs.models = 'age2ageDat')
obs.ctrl_SS3 <- create.obs.ctrl(stksnames = stks,flq.PIL=flq.PIL)

obs.ctrl_SS3$PIL$indObs <- list(AcousticNumberAtAge = list(indObs.model = "ageInd"),
                                DEPM = list(indObs.model = "bioInd"))
obs.ctrl_SS3$PIL$obs.curryr <- TRUE

#obs.ctrl_SS3[["PIL"]]$stkObs$TAC.ovrsht <- FLQuant(dimnames = list(year = first.yr:last.yr, unit = PIL.unit,
#season = 1:ns, iter = 1:ni))
# slts <- c("nmort.error", "fec.error", "land.wgt.error", "stk.nage.error", "stk.wgt.error",
#           "disc.wgt.error", "land.nage.error", "disc.nage.error")
#  for(sl in slts){
#   obs.ctrl_SS3[["PIL"]]$stkObs[[sl]]   <- get(sl)
#   obs.ctrl_SS3[["PIL"]]$stkObs[[sl]][] <- rnorm(prod(dim(flq)),1,.1)
# }

#index.q for our case (is this right???)
indices_ss3$AcousticNumberAtAge@index.q[,] <- c(0,rep(1.31,6))
indices_ss3$DEPM@index.q[] <- 1.13
#last_DEPM<-2017
#noDEPMyears <- an(proj.yrs)[(((an(proj.yrs)-last_DEPM)-3)/3)%%1!=0]
#indices_ss3$DEPM@index.q[,ac(noDEPMyears),]<-NA

#==============================================================================
# ASSESSMENT CONTROLS                                                      ----
#==============================================================================

# - No assessment:

assess.models <- "NoAssessment"

#==============================================================================
# FLBEIA input object: assess.ctrl
#==============================================================================

assess.ctrl   <- create.assess.ctrl(stksnames = stks, assess.models = assess.models)

# Required assessment estimates also for ass.yr:
assess.ctrl$PIL$ass.curryr <- TRUE

assess.ctrl_perf <- assess.ctrl


# - Assessment emulator (--> stock and exploitation status):

assess.ctrl <- list( PIL=list(assess.model="assessEmul", 
                              work_w_Iter = TRUE, harvest.units="f", 
                              control=list( cv.age = c(NA,0.29,0.19,0.20,0.22,0.21,0.26), # cvs at age
                                            logcv.redval = 0.13, # cv reduction value for cvs at age (in log scale relative to assessment year)
                                            logcv.rednyr = 5,    # number of years in which we apply this reduction (relative to assessment year)
                                            cor.prevyr = 0.9)))  # autocorrelation with previous year

# Required assessment estimates also for ass.yr:
assess.ctrl$PIL$ass.curryr <- TRUE

assess.ctrl_emul <- assess.ctrl


# - SS3 assessment:
assess.ctrl_SS3 <- list(PIL=list(assess.model = "ss32flbeia", work_w_Iter = FALSE,harvest.units='f',
                             control=list(ref.name="ss3R",
                                          assess_dir="~/Documents/IPMA/SARDINE/WGHANSA2018/SS3/")))

# Required assessment estimates also for ass.yr:
assess.ctrl_SS3$PIL$ass.curryr <- TRUE

#==============================================================================
# ADVICE DATA                                                              ----
#==============================================================================

PIL_advice.TAC.flq <- FLQuant( c(quantSums(catchWStock(fleets, stock = "PIL"))), # TAC equal to catches
                               dimnames=list(quant=stks,year=first.yr:last.yr), iter=ni)

PIL_advice.TAC.flq[,ac(c(1990,1992,1994:1998)),] <- NA # no ICES advice
PIL_advice.TAC.flq[,ac(c(1987:1989,1991,1993,1999:2017)),] <- c(140000,150000,212000,176000,135000,38000,81000,
                                                                88000,95000,100000,128000,106000,96000,114000,
                                                                92000,71000,75000,75000,36000,55000,17000,16000,12000,23000)  # ICES #advice
PIL_advice.TAC.flq[,ac(2018),] <- c(14600) #! in 2018 

PIL_advice.quota.share.flq <- FLQuant( 1, dimnames=list(quant=stks,year=first.yr:last.yr), iter=ni)
PIL_advice.avg.yrs         <- c(2013:2017)

#==============================================================================
#              FLBEIA input object: advice
#==============================================================================

stks.data <- list(PIL=ls(pattern="^PIL"))

advice   <- create.advice.data(yrs,ns,ni,stks.data,fleets)

units(advice$TAC) <- "t"
#==============================================================================
# ADVICE CONTROL                                                           ----
#==============================================================================

advice.ctrl <- list()
for (st in stks) advice.ctrl[[st]] <- list()


advice.ctrl[["PIL"]]$HCR.model <- "pilHCRs"

advice.ctrl[["PIL"]]$nyears      <- 2
advice.ctrl[["PIL"]]$wts.nyears  <- 3
advice.ctrl[["PIL"]]$fbar.nyears <- 3
#! alternative fit new SRR or adopt one
#advice.ctrl[["PIL"]]$f.rescale <- FALSE # Default

advice.ctrl[["PIL"]]$intermediate.year <- "TAC" #! Assumption on intermediate year catches (Fsq or TAC)

advice.ctrl[["PIL"]]$sr <- list( model = "geomean", 
                                 # params = ,
                                 years = c(y.rm = 1, num.years = 5)) #! need to expand it in the simulations

#advice.ctrl[["PIL"]]$rel.yrs <- ac(rel.yrs) # names of the years used to estimate relative SSB (we don't use it)

advice.ctrl[["PIL"]]$AdvCatch <- rep(TRUE,length(first.yr:last.yr)) # TRUE advice in catches, FALSE advice in landings
names(advice.ctrl[["PIL"]]$AdvCatch) <- c(first.yr:last.yr)

advice.ctrl[["PIL"]]$ref.pts <- matrix( PIL_ref.pts, length(PIL_ref.pts), ni, 
                                        dimnames = list(names(PIL_ref.pts), 1:ni))

#! Additional reference points and rule number are rule-dependent


# Reference points required by each rule (in parenthesis, those that can be internally estimated):
# - For all HCR: Fmsy, Blim, Bmsy




#==============================================================================
# PROJECTION - assessment yr                                               ----
#==============================================================================

# Estimate catch-at-age (landings + discards) for assessment year

  # pay <- FLBEIA( biols = biols, SRs = SRs, fleets = fleets, covars = NULL, indices = NULL, advice = advice,
  #                main.ctrl = list( sim.years = c(initial = ass.yr+1, final = ass.yr+1)),
  #                biols.ctrl = biols.ctrl,  fleets.ctrl = fleets.ctrl, covars.ctrl = NULL,
  #                obs.ctrl = list( PIL = list(stkObs=list(stkObs.model="NoObsStock"), indObs="NoObsIndex")),
  #                assess.ctrl = list( PIL = list(assess.model="NoAssessment")),
  #                advice.ctrl = list( PIL = list(HCR.model="fixedAdvice", AdvCatch=advice.ctrl[["PIL"]]$AdvCatch)))
  # 
  # fleets <- pay$fleets

main.ctrl$sim.years["initial"] <- 2018

#==============================================================================
# SAVE FLBEIA INPUTS                                                       ----
#==============================================================================

save( biols, SRs_MED, SRs_LOW, SRs_MIX, yrs.srmed, yrs.srlow, yrs.srmix,
      fleets, covars, #indices,
      indices_none,indices_ss3, advice, 
      main.ctrl, biols.ctrl, fleets.ctrl, covars.ctrl, 
      obs.ctrl_perf, obs.ctrl_SS3, #obs.ctrl, 
      # PILstwa_obs.varlog, PILmat_obs.varlog, PILcwa_obs.varlog
      assess.ctrl_perf, assess.ctrl_emul, assess.ctrl_SS3, #assess.ctrl, 
      advice.ctrl, Blow,
      PILqa.cv,PILqa.mean,
      # PILstwa_pop.varlog, PILmat_pop.varlog, PILcwa_pop.varlog
      file=paste(wd_in,"PIL_input2018.RData",sep=""))

save( PIL_ref.pts, file=paste(wd_in,"PIL_refpts2018.RData",sep=""))

