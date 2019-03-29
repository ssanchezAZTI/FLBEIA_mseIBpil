################################################################################
#  Summary functions for IB sardine - summary results                          # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  11/10/2018                                                       #
#   modified: Laura Wise 04/01/2019                                            #
################################################################################

# ibpil_perfInd.R - summary statistics for each scenario
# msePIL8c9a/fun/ibpil_perfInd.R

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3



#------------------------------------------------------------------------------#
# perfInd.pil(obj) :: performance indicators for IB sardine  
#------------------------------------------------------------------------------#

perfInd.pil <- function( obj.bio="out.bio", obj.adv="out.adv", #obj.eco="out.flt", obj.adv="out.adv",
                         scenario, file.dat,
                         proj.yrs=2018:2037, Blim=337448, Blow=196334 ,TACpref=NA) {
  
  # biological data
  xx <- loadToEnv( file.dat)[[obj.bio]]
  xx$stock <- NULL
  
  # # economic data
  # yy <- loadToEnv( file.dat)[[obj.eco]][,c("year","scenario","effort","quotaUpt")]
  
  # # advice data
  # zz <- loadToEnv( file.dat)[[obj.adv]][,c("year","scenario","quotaUpt","tac")]
  
  
  # compute biological data to derive the performance metrics
  
  idx <-  xx$year %in% proj.yrs
  xx <- subset(xx, idx)
  
  
  # dimensions, etc
  
  it <- unique(xx$iter)
  nit <- length(it)
  yrnms <- proj.yrs
  nyr <- length(yrnms)
  first.yr <- proj.yrs[1]
  last.yr <- proj.yrs[length(proj.yrs)]
  
  
  # periods of interest
  initf <- c(first.yr:(first.yr+5)) # short-term first 5 years
  stf <- c((first.yr):(first.yr+10)) # short-term first 10 years
  ltf <- c((last.yr-10):last.yr) # equilibrium last 10 years
  allf <- c(first.yr:last.yr) # all forecast years
  
  # # initialise vector for output
  # out <- NULL
  
  # quantiles SSB
  out <- c(quantile(xx[,'ssb'],c(0.05,0.5,0.95)))
  
  # Mean SSB
  tmp <- mean(xx[,'ssb'])
  out <- c(out,tmp)
  
  # Median last SSB
  tmp <- median(xx[xx$year==max(yrnms),'ssb'])
  out <- c(out, tmp)
  
  # Interannual variation of B1+
  out <- c(out, apply(matrix(unlist(tapply( xx[,'ssb'], list(xx$iter), tacdif) ), nrow=nit, byrow=T), 2, mean))
  
  # # Risk 1: P(SSB < Blim)
   pBlim <- ifelse( xx[,'ssb'] < Blim, 1, 0)
  # tmp <- sum(pBlim) / (nit * nyr)
  # out <- c(out, tmp)
  # 
  # # Risk 2: P(SSB < Blim) at least once
  # tmp <- mean( tapply(pBlim, list(xx$iter), max) ) 
  # out <- c(out, tmp)
  
  # Risk MP: P(SSB < 0.8*Blim) in 5 years
  p08Blim <- ifelse( xx[xx$year %in% initf,'ssb'] < 0.8*Blim, 1, 0)
  tmp <-  sum(p08Blim) / (nit * length(initf))
  out <- c(out, tmp)
  
  # Risk recovery: P(SSB < Blim) in 5 years
  pBlim2 <- ifelse( xx[xx$year %in% initf,'ssb'] < Blim, 1, 0)
  tmp <-  sum(pBlim2) / (nit * length(initf))
  out <- c(out, tmp)
  
  # Risk 3: maximum anual P(SSB < Blim)
  tmp <- max( tapply(pBlim, list(xx$year), mean) ) 
  out <- c(out, tmp)
  
  # Risk MP_Low: P(SSB < 0.8*Blow) in 5 years
  p08Blow <- ifelse( xx[xx$year %in% initf,'ssb'] < 0.8*Blow, 1, 0)
  tmp <-  sum(p08Blow) / (nit * length(initf))
  out <- c(out, tmp)
  
  # Risk recovery_Low: P(SSB < Blow) in 5 years
  pBlow2 <- ifelse( xx[xx$year %in% initf,'ssb'] < Blow, 1, 0)
  tmp <-  sum(pBlow2) / (nit * length(initf))
  out <- c(out, tmp)
  
  # Risk 3_Low: maximum anual P(SSB < Blow)
  pBlow <- ifelse( xx[,'ssb'] < Blow, 1, 0)
  tmp <- max( tapply(pBlow, list(xx$year), mean) ) 
  out <- c(out, tmp)
  
  # Average number of years that SSB < Blim
  tmp <- mean (tapply(pBlim, list(xx$iter), sum) )
  out <- c(out, tmp)
  
  # Average/median number of years necessary to get SSB > Blim
  tmp <- tapply( pBlim, list(xx$iter), auxiliary.f)
  out <- c(out, mean(tmp))
  
  # Average number of years that SSB < Blow
  tmp <- mean (tapply(pBlow, list(xx$iter), sum) )
  out <- c(out, tmp)
  
  # Average/median number of years necessary to get SSB > Blow
  tmp <- tapply( pBlow, list(xx$iter), auxiliary.f)
  out <- c(out, mean(tmp))
  
  # Probability that the fishery is closed
  pclosed <- ifelse( xx[,'catch'] <= 1e-6, 1, 0)
  tmp <- sum(pclosed) / (nit * nyr)
  out <- c(out, tmp)
  
  # P(closure) at least once
  tmp <- mean( tapply(pclosed, list(xx$iter), max) ) 
  out <- c(out, tmp)
  
  # Average number of years that fishery is closed
  tmp <- mean (tapply(pclosed, list(xx$iter), sum) )
  out <- c(out, tmp)
  
  # quantiles catch
  tmp <- quantile(xx[,'catch'],c(0.05,0.5,0.95))
  out <- c(out,tmp)
  
  # Average catch
  out <- c(out, mean(xx[,'catch']))
  
  # Average sd catch
  out <- c(out, mean(tapply( xx[,'catch'], list(xx$iter), sd ) ))
  
  # Interanual variation
  out <- c(out, apply(matrix(unlist(tapply( xx[,'catch'], list(xx$iter), tacdif) ), nrow=nit, byrow=T), 2, mean)[1:3])
  
  # # P(TAC >= TACpref)
  # if (is.na(TACpref)) TACpref <- 0
  # if (TACpref>0) {
  #   pTACpref <- ifelse(xx[,'catch'] >= TACpref, 1, 0)
  #   tmp <- sum(pTACpref) / (nit * nyr)
  # } else tmp <- NA
  # out <- c(out, tmp)
  
  out <- as.data.frame(matrix(out, nrow=1))
  
  names(out) <- c( "p05_SSB","Median_SSB","p95_SSB","Mean_SSB", "Median_lastSSB",
                   "p5_SSB","pscaled_SSB","percDif_SSB","above5%Allyears_SSB",
                   #"Risk1", "Risk2", 
                   "MP_Risk","Recovery_Risk","Risk3", 
                   "MP_Risk_Low","Recovery_Risk_Low","Risk3_Low",
                   "years_SSB_under_Blim", "years_get_SSB_up_Blim",
                   "years_SSB_under_Blow", "years_get_SSB_up_Blow",
                   "closure", "closure_once", "years_closure",
                   "p05_catch","Median_catch","p95_catch",
                   "average_catch", "average_sd_catch",
                   "p5_catch","pscaled_catch","percDif_catch")
  
  out <- cbind(scenario=scenario, out)
  
  return(out)
}

# function to compute the average number of years necessary to get SSB > Blim
# it is applied to a vector containing 0's and 1's indicating whether SSB < Blim or not

auxiliary.f <- function(x){
  x <- as.vector(x)
  ind <- 1:length(x)
  tmp1 <- NULL
  tmp2 <- NULL
  if (x[1]==1){
    tmp1 <- c(tmp1, ind[1])
  }
  for(i in 2:length(x)){
    if(x[i-1]==0 & x[i]==1){
      tmp1 <- c(tmp1, ind[i])
    }
  }
  for(i in 1:(length(x)-1)){
    if(x[i]==1 & x[i+1]==0){
      tmp2 <- c(tmp2, ind[i])
    }
  }
  if (x[length(x)]==1){
    tmp2 <- c(tmp2, ind[length(x)])
  }
  out <- as.numeric(tmp2) - as.numeric(tmp1) + 1
  if(length(tmp1)==0) out <- 0
  mean(out)
}

tacdif <- function(dd){ # function to compute statistics for tac difference for 1 vector
  ny <- length(dd)
  dif <- dd[2:ny]-dd[1:(ny-1)]
  pdif <- dd[2:ny]/dd[1:(ny-1)]
  meandif <- mean(dif)
  meanpdif <- mean(pdif)
  intpdif <- sum(ifelse(pdif>1.05,1,0)) # sum of years that %dif is above 5%
  rangedif <- max(dif)-min(dif)
  brks1 <- c(-Inf,-5000,5000,Inf)
  pdif1 <- table(cut(dif, brks1))[2]/length(dif)
  brks2 <- c(-Inf, meandif-0.15*rangedif, meandif+0.15*rangedif, Inf)
  pdif2 <- ifelse(rangedif<1e-6, 1, table(cut(dif, brks2))[2]/length(dif))  
  out <- c(pdif1, pdif2,meanpdif,intpdif)
  names(out)<- c("p5","pscaled","percDif","p5Allyears")
  return(out)
}  

# #------------------------------------------------------------------------------#
# # OMplot :: ???  
# #------------------------------------------------------------------------------#
# 
# OMplot.ane <- function(object, hr, proj.yrs=2014:2034, Blim=21, main=NULL){
#   
#   require(arrayhelpers)
#   
#   xx <- array2df(object)
#   
#   names(xx)[1] <- 'value'
#   names(xx)[2] <- 'hr'
#   names(xx)[3] <- 'year'
#   names(xx)[4] <- 'iter'
#   names(xx)[5] <- 'indicator'
#   
#   xx <- xx[xx$hr==hr, ]
#   
#   yrs <- as.numeric(as.character(unique(xx$year)))
#   nyr <- length(yrs)
#   
#   par(mfrow=c(2,2), mar=c(2,5,1,1)+0.15)
#   
#   aux <- subset(xx, indicator=="rec")
#   dd <- data.frame(low=tapply(aux$value, list(aux$year), quantile, 0.05),
#                    med=tapply(aux$value, list(aux$year), quantile, 0.5),
#                    up=tapply(aux$value, list(aux$year), quantile, 0.95))
#   
#   matplot(yrs, dd, type="n", col=1, lty=c(2,1,2), lwd=2, xlab="", ylab="Recruits (millions)", ylim=c(0,22000))
#   polygon(c(yrs,yrs[nyr:1]), c(dd$low, dd$up[nyr:1]), col="grey", border=NA)
#   lines(yrs, dd$med, lwd=2)
#   abline(v=min(proj.yrs), lty=2)
#   
#   
#   aux <- subset(xx, indicator=="ssb")
#   dd <- data.frame(low=tapply(aux$value, list(aux$year), quantile, 0.05),
#                    med=tapply(aux$value, list(aux$year), quantile, 0.5),
#                    up=tapply(aux$value, list(aux$year), quantile, 0.95))
#   
#   matplot(yrs, dd, type="n", col=1, lty=c(2,1,2), lwd=2, xlab="", ylab="SSB (thousand tonnes)", ylim=c(0,220))
#   polygon(c(yrs,yrs[nyr:1]), c(dd$low, dd$up[nyr:1]), col="grey", border=NA)
#   lines(yrs, dd$med, lwd=2)
#   abline(v=min(proj.yrs), lty=2)
#   abline(h=Blim, lty=2, col=2)
#   
#   aux <- subset(xx, indicator=="tac")
#   dd <- data.frame(low=tapply(aux$value, list(aux$year), quantile, 0.05),
#                    med=tapply(aux$value, list(aux$year), quantile, 0.5),
#                    up=tapply(aux$value, list(aux$year), quantile, 0.95))
#   
#   matplot(yrs, dd, type="n", col=1, lty=c(2,1,2), lwd=2, xlab="", ylab="Catch (thousand tonnes)", ylim=c(0,40))
#   polygon(c(yrs,yrs[nyr:1]), c(dd$low, dd$up[nyr:1]), col="grey", border=NA)
#   lines(yrs, dd$med, lwd=2)
#   abline(v=min(proj.yrs), lty=2)
#   
#   aux <- subset(xx, indicator=="harvest")
#   dd <- data.frame(low=tapply(aux$value, list(aux$year), quantile, 0.05),
#                    med=tapply(aux$value, list(aux$year), quantile, 0.5),
#                    up=tapply(aux$value, list(aux$year), quantile, 0.95))
#   
#   matplot(yrs, dd, type="n", col=1, lty=c(2,1,2), lwd=2, xlab="", ylab="Harvest rates", ylim=c(0,2))
#   polygon(c(yrs,yrs[nyr:1]), c(dd$low, dd$up[nyr:1]), col="grey", border=NA)
#   lines(yrs, dd$med, lwd=2)
#   abline(v=min(proj.yrs), lty=2)
#   
#   title(main, outer=T, line=-1)
#   
# }