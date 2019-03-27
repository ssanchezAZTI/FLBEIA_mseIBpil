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

segregmix <- function () 
{
  logl <- function(a, b, A, B, rec, ssb) {
    loglAR1(log(rec), FLQuant(log(ifelse( c(ssb) <= b, a * c(ssb), 
                                          ifelse( c(ssb) <= B, a * b, 
                                                  A * B))), dimnames = dimnames(ssb)))
  }
  
  model <- rec ~ FLQuant(ifelse( c(ssb) <= b, a * c(ssb), 
                                 ifelse( c(ssb) <= B, a * b, 
                                         A * B)), dimnames = dimnames(ssb))
  
  initial <- structure(function(rec, ssb) {
    return(FLPar(a = median(c(rec)/c(ssb), na.rm = TRUE), 
                 b = median(c(ssb), na.rm = TRUE), 
                 A = median(c(rec)/c(ssb), na.rm = TRUE), 
                 B = median(c(ssb), na.rm = TRUE)))
  }, lower = rep(0, 0, 0, 0), upper = rep(Inf, 4))
  
  return(list(logl = logl, model = model, initial = initial))
}

