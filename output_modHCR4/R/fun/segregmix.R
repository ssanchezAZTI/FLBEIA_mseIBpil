################################################################################
#  Mixture of 2 segreg() stock-recruitment models                              # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  26/03/2019                                                       #
#   modified:                                                                  #
################################################################################

# segregmix.r - Mixture of 2 FLCore:::segreg() stock-recruitment models 
# ./segregmix.r

# Copyright: AZTI, 2019
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

# rec is calculated as:
# - a * SSB       , if     SSB <= b
# - a * b         , if b < SSB <= B
# - A * B * uncAdd, if B < SSB
# 
# where uncAdd corresponds to the uncertainty for SSB > B,
# with uncAdd = @uncertainty ^ ((sd of SR residuals for SSB > B) / (sd of SR residuals for SSB <= B) - 1)

segregmix <- function () 
{
  logl <- function(a, b, A, B, rec, ssb, uncAdd) {
    loglAR1(log(rec), FLQuant(log(ifelse( c(ssb) <= b, a * c(ssb), 
                                          ifelse( c(ssb) <= B, a * b, 
                                                  A * B * uncAdd))), dimnames = dimnames(ssb)))
  }
  
  model <- rec ~ FLQuant(ifelse( c(ssb) <= b, a * c(ssb), 
                                 ifelse( c(ssb) <= B, a * b, 
                                         A * B * uncAdd)), dimnames = dimnames(ssb))
  
  initial <- structure(function(rec, ssb) {
    return(FLPar(a = median(c(rec)/c(ssb), na.rm = TRUE), 
                 b = median(c(ssb), na.rm = TRUE), 
                 A = median(c(rec)/c(ssb), na.rm = TRUE), 
                 B = median(c(ssb), na.rm = TRUE)))
  }, lower = rep(0, 0, 0, 0), upper = rep(Inf, 4))
  
  return(list(logl = logl, model = model, initial = initial))
}

