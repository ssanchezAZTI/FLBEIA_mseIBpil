################################################################################
#  Summary functions for IB sardine - summary results                          # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  11/10/2018                                                       #                                            #
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

perfInd.pil <- function( obj.bio="out.bio",scenario, file.dat,
                         proj.yrs=proj.yrs, Blim=337448, Blow=196334) {
  
  # biological data
  xx <- loadToEnv(file.dat)[[obj.bio]]
  xx$stock <- NULL
  
  
  # compute biological data to derive the performance metrics
  idx <-  xx$year %in% proj.yrs
  xx <- subset(xx, idx)
  
  
  # dimensions, etc
  
  it <- unique(xx$iter)
  nit <- length(it)
  yrnms <- proj.yrs
  nyr <- length(yrnms)
  mp_yr <- 2023

  
  # # initialise vector for output
  # out <- NULL
  
  # quantiles SSB
  out <- c(quantile(xx[,'ssb'],c(0.05,0.10,0.5,0.90,0.95)))
  
  # Mean SSB
  tmp <- mean(xx[,'ssb'])
  out <- c(out,tmp)
  
  # Median last SSB
  tmp <- median(xx[xx$year==max(yrnms),'ssb'])
  out <- c(out, tmp)
  
  # Interannual variation of B1+
  out <- c(out, apply(matrix(unlist(tapply( xx[,'ssb'], list(xx$iter), tacdif) ), nrow=nit, byrow=T), 2, mean))
  
  # MP Success: P(SSB > 0.8*Blim) in the year 2023
  p08Blim <- ifelse( xx[xx$year==mp_yr,'ssb'] >= 0.8*Blim, 1, 0)
  tmp <-  sum(p08Blim) / nit
  out <- c(out, tmp)
  
  # MP Success_Low: P(SSB > 0.8*Blow) in the year 2023
  p08Blow <- ifelse( xx[xx$year==mp_yr,'ssb'] >= 0.8*Blow, 1, 0)
  tmp <-  sum(p08Blow) / nit
  out <- c(out, tmp)
  
  # Average number of years necessary to get SSB >= 0.8Blim
  yrs.MP <- which(ifelse((tapply( ifelse( xx[,'ssb'] >= 0.8*Blim, 1, 0), list(xx$year), mean))>=0.90,1,0)==1)[1]
  tmp <- proj.yrs[yrs.MP]
  out <- c(out, tmp)
  
  # Average number of years necessary to get SSB >= 0.8Blow
  yrs.MP_Low <- which(ifelse((tapply( ifelse( xx[,'ssb'] >= 0.8*Blow, 1, 0), list(xx$year), mean))>=0.90,1,0)==1)[1]
  tmp <- proj.yrs[yrs.MP_Low]
  out <- c(out, tmp)
  
  # # Risk 1: P(SSB < Blim)
  pBlim <- ifelse( xx[,'ssb'] < Blim, 1, 0)
  tmp <- sum(pBlim) / (nit * nyr)
  out <- c(out, tmp)

  # # Risk 1_Low: P(SSB < Blow)
  pBlow <- ifelse( xx[,'ssb'] < Blow, 1, 0)
  tmp <- sum(pBlow) / (nit * nyr)
  out <- c(out, tmp)
  
  # Risk 2: P(SSB < Blim) at least once
  tmp <- mean( tapply(pBlim, list(xx$iter), max) )
  out <- c(out, tmp)
  
  # Risk 2_Low: P(SSB < Blow) at least once
  tmp <- mean( tapply(pBlow, list(xx$iter), max) )
  out <- c(out, tmp)
  
  # Risk 3: maximum anual P(SSB < Blim)
  tmp <- max( tapply(pBlim, list(xx$year), mean) ) 
  out <- c(out, tmp)
  
  # Risk 3_Low: maximum anual P(SSB < Blow)
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
  
  #  sd catch
  out <- c(out, mean(tapply( xx[,'catch'], list(xx$iter), sd ) ))
  
  # Interanual variation
  out <- c(out, apply(matrix(unlist(tapply( xx[,'catch'], list(xx$iter), tacdif) ), nrow=nit, byrow=T), 2, mean))
  
  out <- as.data.frame(matrix(out, nrow=1))
  
  names(out) <- c( "P5th_B1plus","P10th_B1plus","Median_B1plus","P90th_B1plus","P95th_B1plus","Mean_B1plus","Median_lastB1plus",
                   "IAV1_B1plus","IAV2_B1plus",
                   "P_B1plus_0.8Blim","P_B1plus_0.8Blow","years_B1plus_0.8Blim","years_B1plus_0.8Blow",
                   "avg_P_B1plus_Blim","avg_P_B1plus_Blow","once_P_B1plus_Blim","once_P_B1plus_Blow",
                   "max_P_B1plus_Blim","max_P_B1plus_Blow",
                   "years_B1plus_under_Blim", "years_get_B1plus_up_Blim",
                   "years_B1plus_under_Blow", "years_get_B1plus_up_Blow",
                   "closure", "closure_once", "years_closure",
                   "P5th_Catch","Median_Catch","P95th_Catch",
                   "Mean_Catch", "StDev_Catch",
                   "IAV1_Catch","IAV2_Catch")
  
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
  dd <- ifelse(dd<1.0e-06,NA,dd) #for catches zero we change to NA
  pdif <- dd[2:ny]/dd[1:(ny-1)]
  meandif <- mean(dif)
  meanpdif <- mean(pdif,na.rm=T)
  # rangedif <- max(dif)-min(dif)
  # brks1 <- c(-Inf,-5000,5000,Inf)
  # pdif1 <- table(cut(dif, brks1))[2]/length(dif)
  # brks2 <- c(-Inf, meandif-0.15*rangedif, meandif+0.15*rangedif, Inf)
  # pdif2 <- ifelse(rangedif<1e-6, 1, table(cut(dif, brks2))[2]/length(dif))  
  out <- c(meandif,meanpdif)
  names(out)<- c("absDif","percDif")
  return(out)
}  
