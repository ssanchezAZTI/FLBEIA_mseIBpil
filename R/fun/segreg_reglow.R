################################################################################
#  Mixture of 2 segreg() stock-recruitment models                              #
#  (one for low regime and other for medium regime)                            #
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  10/04/2019                                                       #
#   modified:                                                                  #
################################################################################

# segreg_reglow.r - Mixture of 2 FLCore:::segreg() stock-recruitment models
#                   with a covariate to check if already passed from low to medium regime
# ./segreg_reglow.r

# Copyright: AZTI, 2019
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


# rec is calculated as:
# - (reglow * a + (1-reglow) * A * uncAdd) * SSB    , if     SSB <= b
# - (reglow * a * b) + (1-reglow) * A * SSB * uncAdd, if b < SSB <= B
# - (reglow * a * b) + (1-reglow) * A * B * uncAdd  , if B < SSB
# 
# where:
# - reglow=1 if low recruitment regime, and 0 otherwise.
# - uncAdd corresponds to the uncertainty added when out of low regime,
#   with uncAdd = @uncertainty ^ ((sd of SR residuals for mean regime) / (sd of SR residuals for low regime) - 1).

segreg_reglow <- function () 
{
  logl <- function(a, b, A, B, rec, ssb, reglow, uncAdd) {
    loglAR1(log(rec), FLQuant(log(ifelse( c(ssb) <= b, (reglow * a + (1-reglow) * A * uncAdd) * c(ssb), 
                                          ifelse( c(ssb) <= B, (reglow * a * b) + (1-reglow) * A * SSB * uncAdd,
                                                  (reglow * a * b) + (1-reglow) * A * B * uncAdd))), 
                              dimnames = dimnames(ssb)))
  }
  
  model <- rec ~ FLQuant(ifelse( c(ssb) <= b, (reglow * a + (1-reglow) * A * uncAdd) * c(ssb),
                                 ifelse( c(ssb) <= B, (reglow * a * b) + (1-reglow) * A * c(ssb) * uncAdd,
                                         (reglow * a * b) + (1-reglow) * A * B * uncAdd)), 
                         dimnames = dimnames(ssb))
  
  initial <- structure(function(rec, ssb) {
    return(FLPar(alow = median(c(rec)/c(ssb), na.rm = TRUE), 
                 a = median(c(rec)/c(ssb), na.rm = TRUE), 
                 b = median(c(ssb), na.rm = TRUE),
                 reglow = 1))
  }, lower = rep(0, 4), upper = c(rep(Inf, 3),1))
  
  return(list(logl = logl, model = model, initial = initial))
}


#-------------------------------------------------------------------------------
# regimeCheck - checks first time SSB gets above a threshold --> changes SR
#-------------------------------------------------------------------------------

regimeCheck <-  function( covars, biols, SRs, fleets, year, season, ctrl, cvnm,...){
  
  stknm   <- ctrl[[cvnm]]$stock
  Bref    <- ctrl[[cvnm]]$Bref
  
  if (!is.null(ctrl[[cvnm]]$spwn.sson)) {
    ss <- ctrl[[cvnm]]$spwn.sson
  } else 
    ss <- 1
  
  sr.spwn <- which(SRs[[stknm]]@proportion[,year,,,,1]==1)
  
  
  # Check if appropriate names
  
  if ( !stknm %in% names(biols))
    stop( "Covariable '", cvnm, "' not available for '", stknm, "' stock")
  
  if ( !cvnm %in% names(SRs[[stknm]]@covar))
    stop( "Covariable '", cvnm, "' not defined in the '", stknm, "' stock-recrutiment relationship")
  
  
  # Assign correct values (except for last projection year)
  
  if (year+1 <= dim(biols[[stknm]]@n)[2]) {
    
    if (ss == ss) {
      
      # Estimate SSB for the following year
      
      biol <- biols[[stknm]]
      na <- dim(biol@n)[1]
      ns <- dim(biol@n)[4]
      ni <- dim(biol@n)[6]
      
      # IF season = 1 THEN age groups move to the next. 
      if(ss == 1){
        # total catch in year [year] ss [ns].
        catch.n <- catchStock(fleets,stknm)[,year,,ns]
        # middle ages
        biol@n[-c(1,na),year+1,,ss] <- (biol@n[-c(na-1,na),year,,ns]*exp(-biol@m[-c(na-1,na),year,,ns]/2) - catch.n[-c(na-1,na),])*
          exp(-biol@m[-c(na-1,na),year,,ns]/2) 
        # plusgroup
        biol@n[na,year+1,,ss]       <- (biol@n[na-1,year,,ns]*exp(-biol@m[na-1,year,,ns]/2) - catch.n[na-1,])*exp(-biol@m[na-1,year,,ns]/2) + 
          (biol@n[na,year,,ns]*exp(-biol@m[na,year,,ns]/2) - catch.n[na,])*exp(-biol@m[na,year,,ns]/2)
        
      } else {
        # total catch in year [year+1] ss [ss-1].
        catch.n <- catchStock(fleets,stock)[,year+1,,ss-1]
        # middle ages      # for unit == ss  and age = 1, it will be equal NA but be updated after with SRsim.
        biol@n[,year+1,,ss] <- (biol@n[,year+1,,ss-1]*exp(-biol@m[,year+1,,ss-1]/2) - catch.n)*exp(-biol@m[,year+1,,ss-1]/2) 
      }
      
      # Update SSB.
      ssb.val <- unitSums(quantSums(n(biol) * wt(biol) * fec(biol)*mat(biol) * 
                                  exp(-biol@m*spwn(biol)), na.rm=TRUE))[,year+1,,ss]
      
      # Set values for the covariate
      
      for (i in 1:ni) {
        if (covars[[cvnm]][,year,,,i]==0) {
          covars[[cvnm]][,year+1,,,i] <- 0 # once out of low regime not again in it
        } else
          covars[[cvnm]][,year+1,,,i] <- ifelse( ssb.val[,,,,i] < Bref, 1, 0)
      }
      
      SRs[[stknm]]@covar[[cvnm]][,year+1,,sr.spwn,] <- covars[[cvnm]][,year+1,,ss,]
      
    }
    
  }
  
  return(list(covars = covars, fleets = fleets, biols = biols, SRs = SRs))
}

