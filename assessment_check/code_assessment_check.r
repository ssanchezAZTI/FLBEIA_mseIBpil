########################################################################

# code to check that the assessment works properly

########################################################################


# empty workspace

#rm(list=ls())


# load libraries

library(FLBEIA)
library(r4ss)
library(reshape2)
library(ggplot2)
library(ggplotFL)
library(gridExtra)

theme_set(theme_bw())

########################################################################

# set directories
# wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
# setwd(wd)

# directory with results
res.dir  <- file.path("//dsrbiltegia/comun$/AZTIMAR/PROYECTOS/Gestion Pesquera Sostenible/WKSARMP2019/output_cluster_iteraciones/output_iters")

# load objects for conditioning 
load(file.path("C:/use/GitHub/FLBEIA_mseIBpil/input","PIL_input2018.RData"))

# source
source("./R/fun/ss32flbeia.R")

# name of scenario
sc <- "ASSss3_HCR3_RECmed_INNvar_OERnaq"

# load object
load(file.path(res.dir, paste("out2018_",sc,"_740.RData",sep="")))

# give generic name
assign("obj", get(sc))

# q and sd surveys

flq <- obj$biols$PIL@n *NA
q.ac <- flq
q.ac[ac(0:6),] <- c(-Inf, 0.03363665, -0.06660685, -0.03524329,  0.18057145,  0.35486293, -0.16620986 )
sd.ac <- flq
sd.ac[ac(0:6),] <- c(0, 0.4836377, 0.5184707, 0.4399987, 0.5134851, 0.6282970, 0.7162383)

q.depm <- 1.1337
sd.depm <- sqrt(log(0.25^2+1))

# q and sd catch_at-age

flq <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL) *NA
mean.catch <- flq
mean.catch[ac(0:6),] <- catch_resid_logmeans
sd.catch <- flq
sd.catch[ac(0:6),] <- catch_resid_logsds

# assessment control object

assess.ctrl <- assess.ctrl_SS3
#assess.ctrl[["PIL"]]$control$ref_name <- "ss3R"
assess.ctrl[["PIL"]]$control$run_it<-paste0("0",sc)
assess.ctrl[["PIL"]]$control$ref_name <- "assess_ref"
assess.ctrl[["PIL"]]$control$assess_dir <- "C:/use/GitHub/FLBEIA_mseIBpil/ss3R/"


# definition of years

first.yr <- 1978  # First year of the historic data.
proj.yr  <- 2019  # first year of projection
proj.nyr <- 30    # Number of years in the projection period
hist.yrs <- first.yr:(proj.yr-1)   # historical period
last.yr  <- proj.yr + (proj.nyr-1) # Last year of projection
proj.yrs <- proj.yr:last.yr        # projection period
ass.yr <- proj.yr-1                # assessment year
yrs<-dimnames(biols[[1]]@n)$year

##initialize covars (here the results from the ss3 assessment are stored)

covars$ssb<-covars$fbar<-covars$rec<-FLQuant(NA, dimnames=list(eval.year=c("2017",proj.yrs), year=yrs))
covars$ssb[ac(2017),]<-ssb(biols$PIL)[,]
covars$rec[ac(2017),]<-(biols$PIL@n)[1,]
covars$qs<-FLQuant(NA, dimnames=list(qs=c("acoustic","depm"), year=yrs))
covars$sel<-FLQuant(NA, dimnames=list(age=0:6, year=yrs,unit=1:3))
covars$conv<-FLQuant(NA, dimnames=list(conv="conv",year=yrs))

# number of iterations

nit <- 20

##############################################

# CHECK ASSESSMENT FOR ALL THE PERIOD

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2047)

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2047)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2047)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks1 <- res.stks


p1 <- plot(ssb(res.stks1)/window(ssb(obj$biols$PIL), end=2047))+
  geom_vline(xintercept=2018)+ggtitle("740")

##############################################

# CHECK ASSESSMENT FOR ALL THE PERIOD
# CONSTANT CATCHABILITY FOR ACOUSTICS

q.ac[ac(0:6),] <- c(-Inf, rep(log(1.1), 6))

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2047)

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2047)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2047)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks2 <- res.stks

p2 <- plot(ssb(res.stks2)/window(ssb(obj$biols$PIL), end=2047))+
  geom_vline(xintercept=2018)

##############################################
##############################################

# CHECK ASSESSMENT UP TO 2020

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2020)

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2020)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2020)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks3 <- res.stks

plot(rec(res.stks3)/window(obj$biols$PIL@n[1,], end=2020))+
  geom_vline(xintercept=2018)

plot(window(obj$biols$PIL@n[1,], end=2020))+
  geom_vline(xintercept=2018)+geom_hline(yintercept=exp(16))

p3 <- plot(ssb(res.stks3)/window(ssb(obj$biols$PIL), end=2020))+
  geom_vline(xintercept=2018)

##############################################

# CHECK ASSESSMENT FOR ALL THE PERIOD
# TINY ERRORS in SURVEYS AND CATCH  

sd.ac[ac(0:6),] <- rep(0.001,7)
sd.depm <- sqrt(log(0.001^2+1))
sd.catch[ac(0:6),] <- rep(0.001,7)

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2047)

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2047)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2047)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks4 <- res.stks


p4 <- plot(ssb(res.stks4)/window(ssb(obj$biols$PIL), end=2047))+
  geom_vline(xintercept=2018)+ggtitle("740")


##############################################

# CHECK ASSESSMENT FOR ALL THE PERIOD
# TINY ERRORS in SURVEYS  

sd.ac[ac(0:6),] <- rep(0.001,7)
sd.depm <- sqrt(log(0.001^2+1))
sd.catch[ac(0:6),] <- catch_resid_logsds

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2047)

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2047)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2047)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks5 <- res.stks


p5 <- plot(ssb(res.stks5)/window(ssb(obj$biols$PIL), end=2047))+
  geom_vline(xintercept=2018)+ggtitle("740")

##############################################

# CHECK ASSESSMENT FOR ALL THE PERIOD
# TINY ERRORS in CATCH  

sd.ac[ac(0:6),] <- c(0, 0.4836377, 0.5184707, 0.4399987, 0.5134851, 0.6282970, 0.7162383)
sd.depm <- sqrt(log(0.25^2+1))
sd.catch[ac(0:6),] <- rep(0.001,7)

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2047)

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2047)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2047)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks6 <- res.stks


p6 <- plot(ssb(res.stks6)/window(ssb(obj$biols$PIL), end=2047))+
  geom_vline(xintercept=2018)+ggtitle("740")


save.image("main_defaults_ss3_setting.RData")


##############################################

# CHECK ASSESSMENT FOR ALL THE PERIOD
# CHANGE TO STEPNESS 0.4 IN SR (control txt changed)

sd.ac[ac(0:6),] <- c(0, 0.4836377, 0.5184707, 0.4399987, 0.5134851, 0.6282970, 0.7162383)
sd.depm <- sqrt(log(0.25^2+1))
sd.catch[ac(0:6),] <- catch_resid_logsds

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2047)

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2047)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2047)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks1b <- res.stks


p1b <- plot(ssb(res.stks1b)/window(ssb(obj$biols$PIL), end=2047))+
  geom_vline(xintercept=2018)+ggtitle("740")
p1b

##############################################

# CHECK ASSESSMENT FOR ALL THE PERIOD
# CHANGE TO ESTIMATE STEPNESS (control txt changed)

sd.ac[ac(0:6),] <- c(0, 0.4836377, 0.5184707, 0.4399987, 0.5134851, 0.6282970, 0.7162383)
sd.depm <- sqrt(log(0.25^2+1))
sd.catch[ac(0:6),] <- catch_resid_logsds

res.stks <- obj$stocks$PIL
res.stks <- propagate(res.stks, nit, fill.iter=TRUE)
res.stks <- window(res.stks, start=1978, end=2047)

nit <- 100

for (i in 1:nit){
  simindices <- obj$indices$PIL
  simindices[["AcousticNumberAtAge"]]@index <- obj$biols$PIL@n*rlnorm(1, q.ac, sd.ac)
  simindices[["DEPM"]]@index <- rlnorm(1, log(q.depm*ssb(obj$biols$PIL)), sd.depm)
  simindices <- window(simindices, start=1978, end=2047)
  
  simstock <- obj$stocks$PIL
  simstock@catch.n <- catch.n(obj$fleets$INT@metiers$ALL@catches$PIL)*rlnorm(1, mean.catch, sd.catch)
  simstock <- window(simstock, start=1978, end=2047)
  simstock@catch <- computeCatch(simstock)
  
  kk <- ss32flbeia(stock=simstock, indices=simindices, control=assess.ctrl[["PIL"]]$control,covars=covars)
  res.stks[,,,,,i] <- kk$stock
}


res.stks1c <- res.stks


p1c <- plot(ssb(res.stks1c)/window(ssb(obj$biols$PIL), end=2047))+
  geom_vline(xintercept=2018)+ggtitle("740")
p1c

##############################################
##############################################


