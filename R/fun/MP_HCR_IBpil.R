#-------------------------------------------------------------------------------
#   HCRs for the Iberian sardine
#   - annualTAC (F or TAC-based)
#
# Sonia Sanchez
# Created: 12/07/2018
# Changed: 2018-09-24 08:52:45 - alternative HCRs defined
# Modified by Laura Wise on 04/01/2019
#          2019-03-22 13:50:22 - ssanchez (some corrections)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# General HCR for the IBpil long term management plan - SSB based (SSBy+1)
#
#
# HCR0: ICES HCR (F-based HCR in absolute terms)
#
#  F advice depending on SSB in relation to Btrigger points is:
#      - 0             ,    0 < SSB <= Blim
#      - Fmsy*SSB/Bmsy , Blim < SSB <= Bmsy
#      - Fmsy          , Bmsy < SSB
#
#
# HCR1: Preferred F
#  F advice depending on SSB in relation to Btrigger points is:
#      - 0    ,        0 < SSB <= Bloss
#      - Floss ,     Bloss < SSB <= 0.8*Blim
#      - Fmsy , 0.8*Blim < SSB 
# 
#
# HCR2: Increasing F
#  F advice depending on SSB in relation to Btrigger points is:
#      - 0                                      ,      0 < SSB <= Bloss
#      - ((Fmsy-Flow)/(Blim-Bloss))*SSB + 
#         (-(Fmsy-Flow)*Bloss/(Blim-Bloss)+Flow), Bloss < SSB <= Blim
#      - Fmsy                                   , Blim < SSB
#
#
# NOTE: In all the rules SSB refers to SSB expected in TAC year (i.e. y+1)
#
#-------------------------------------------------------------------------------


# Fmsy <- 0.12; Floss <- 0.10; Flow <- 0.085
# Blim <- 337448
# Blow <- 196334
# Bloss <- 112943
# B1plus <- Bloss
# ((Fmsy-Flow)/(Blim-Bloss))*B1plus + (-(Fmsy-Flow)*Bloss/(Blim-Bloss)+Flow)


pilHCRs <- function(stocks, advice, advice.ctrl, year, season, stknm,...){
  
  # Assessed stock
  
  stk <- stocks[[stknm]]

  iter     <- dim(stk@m)[6]
  yrsnames <- dimnames(stk@m)[[2]]
  yrsnumbs <- as.numeric(yrsnames)

  assyrname <- yrsnames[year]
  assyrnumb <- yrsnumbs[year]

  # HCR selected
  
  rule <- advice.ctrl[[stknm]]$rule

  if (rule %in% c(0:4)) {
    
    stk <- window(stk, start=yrsnumbs[1], end=yrsnumbs[year-1]) #! CHECK WHEN ASSESSMENT USED
    
    # Short Term Forecast (STF)
    
    nyears      <- ifelse(is.null(advice.ctrl[[stknm]][['nyears']]), 3, advice.ctrl[[stknm]][['nyears']])
    wts.nyears  <- ifelse(is.null(advice.ctrl[[stknm]][['wts.nyears']]), 3, advice.ctrl[[stknm]][['wts.nyears']])
    fbar.nyears <- ifelse(is.null(advice.ctrl[[stknm]][['fbar.nyears']]), 3, advice.ctrl[[stknm]][['fbar.nyears']])
    f.rescale   <- ifelse(is.null(advice.ctrl[[stknm]][['f.rescale']]), TRUE, advice.ctrl[[stknm]][['f.rescale']])
    # disc.nyears  <- ifelse(is.null(advice.ctrl[[stknm]][['disc.nyears']]), wts.nyears, advice.ctrl[[stknm]][['disc.nyears']])

    # Fill the 0-s and NA-s with almost 0 values to avoid problems when the fishery is closed for example, or there is no catch...
    stk@harvest[stk@harvest < 0] <- 0.00001
          
    stk@catch.n[is.na(stk@catch.n)] <- 1e-6
    stk@landings.n[is.na(stk@landings.n)] <- 0
    stk@discards.n[is.na(stk@discards.n)] <- 1e-6

    stk@catch.n[stk@catch.n==0] <- 1e-6
    stk@landings.n[stk@landings.n==0] <- 1e-6
    stk@discards.n[stk@discards.n==0] <- 0

    stk@catch <- computeCatch(stk)
    stk@catch[is.na(stk@catch)] <- 0
    stk@landings <- computeLandings(stk)
    stk@discards <- computeDiscards(stk)
    
    ageStruct <- ifelse(dim(stk@m)[1] > 1, TRUE, FALSE)

     
    if(ageStruct == TRUE){
      
      if(any(stk@catch[,tail(dimnames(stk@catch)$year,3)]<1e-2)){
        stk <- stf_correctSel(stk, nyears = 2, wts.nyears = wts.nyears, fbar.nyears = fbar.nyears, f.rescale = f.rescale) #, disc.nyrs = disc.nyears)
      }else{
        stk <- FLash::stf(stk, nyears = 2, wts.nyears = wts.nyears, fbar.nyears = fbar.nyears, f.rescale = f.rescale) #, disc.nyrs = disc.nyears)
        
      }}else{
        stk <- stfBD(stk, nyears = 2, wts.nyears = wts.nyears, fbar.nyears = fbar.nyears)}
    
    # Build fwd.ctrl
    int.yr <- advice.ctrl[[stknm]]$intermediate.year
    
    for(i in 1:iter){

      int.yr <- ifelse(is.null(int.yr), 'Fsq', int.yr)

      if(int.yr == 'Fsq')
        fwd.ctrl <- fwdControl(data.frame(year = c(assyrnumb),  val = c(1), quantity = c('f'), rel.year = c(assyrnumb-1)))
      else
        fwd.ctrl <- fwdControl(data.frame(year = c(assyrnumb),  val = c(advice$TAC[stknm,year, drop=TRUE][i]), quantity = c('catch')))

      stki <- iter(stk, i)
      
      if(dim(stki@m)[1] > 1){
        # First estimate/extract the SR model and params.
        sr.pars  <- advice.ctrl[[stknm]]$sr$params # sr parameters if specified.
        sr.model <- advice.ctrl[[stknm]]$sr$model  # sr model, mandatory.
        if(is.null(sr.pars)){                   # if params missing => estimate the parameters using the specified years.
          if(is.null(advice.ctrl[[stknm]]$sr$years)) sr.yrs <- which(round(quantSums(stocks[[stknm]]@stock.n))!=0)[1]:(year-1)# yr0 missing => use all data years, except the assessment year for which rec is unknown
          else{
            y.rm <- as.numeric(advice.ctrl[[stknm]]$sr$years['y.rm'])
            nyrs <- as.numeric(advice.ctrl[[stknm]]$sr$years['num.years'])
            sr.yrs <- yrsnames[(year-y.rm-nyrs + 1):(year-y.rm)]
          }
          rec <- stki@stock.n[1,sr.yrs]
          ssb <- ssb(stki)[,sr.yrs]
          
          # if rec.age != 0 adjust rec and ssb.
          rec.age <- as.numeric(dimnames(rec)[[1]])
          if(rec.age != 0){
            rec <- rec[, -(1:rec.age),]
            ssb <- ssb[, 1:(dim(ssb)[2] - rec.age),]
          }
          
          if(sr.model != 'geomean') sr.pars <- try(params(fmle(FLSR(rec = rec, ssb = ssb, model = sr.model))), silent = TRUE) 
          
          if(class(sr.pars) == 'try-error' | sr.model == 'geomean'){
            sr.model <- 'geomean'
            sr.pars <- c(prod(c(rec))^(1/length(c(rec))))
            sr.pars <- FLPar(a = ifelse(is.na(sr.pars), 0, sr.pars))
          }
          
          sr1 <- sr.pars
        }
        else{ # sr.pars not null
          if(i == 1){
            sr1 <- iter(sr.pars,i)
          }
          sr1[] <-  iter(sr.pars,i)[]
        }
        
        stki <- fwd(stki, ctrl = fwd.ctrl, sr = list(model = sr.model, params = sr1))
      }
      else{
        
        # Extract the years to calculate the mean historical growth of the stock
        if(is.null(advice.ctrl[[stknm]]$growth.years))   growth.years <- max(1,(year - 11)):(year-1)
        else{
          y.rm <- as.numeric(advice.ctrl[[stknm]]$growth.years['y.rm'])
          nyrs  <- as.numeric(advice.ctrl[[stknm]]$growth.years['num.years'])
          growth.years <- yrsnames[(year-y.rm-nyrs + 1):(year-y.rm)]
        }
        
        stki <- fwdBD(stki, fwd.ctrl, growth.years)
        
      }
      
      iter(stk, i) <- stki
      
    }
    
    # STF End
    
    ref.pts <- advice.ctrl[[stknm]]$ref.pts # matrix[n,it]  rows = Blim, Btrigger, Fmsy,....
    Cadv <- ifelse(advice.ctrl[[stknm]][['AdvCatch']][year+1] == TRUE, 'catch', 'landings')
    
    # Last SSB (Age structured) or Biomass (Aggregated) estimate
    if(ageStruct) {
      b.datyr <- ssb(stk)[,year+1,drop = TRUE] # [it]
    } else
      b.datyr <- (stk@stock.n*stk@stock.wt)[,year+1,drop = TRUE] # [it]

    if (rule == 0) { # HCR0: ICES HCR (F-based HCR in absolute terms)

      # Find where the SSB (Age structured) OR Biomass (Aggregated) in relation to reference points.
        b.pos <- apply(matrix(1:iter,1,iter),2, function(i) findInterval(b.datyr[i], ref.pts[c('Blim', 'Bmsy'),i]))  # [it]
        Ftg   <- ifelse(b.pos == 0, 0, ifelse(b.pos == 1, ref.pts['Fmsy',]*b.datyr/ref.pts[ 'Bmsy',], ref.pts['Fmsy',]))
    
    } else if (rule == 1) { # HCR1: Preferred F
      
      # Checking reference points:
      if ( sum(ref.pts['Bloss',]>ref.pts['Blim',])>0 )
        stop("Check reference points for '", stknm, "'. In advice.ctrl[['", stknm, "']]$ref.pts, necessarily Bloss <= Blim")
      
      Brefs <- rbind(ref.pts['Bloss',],0.8*ref.pts['Blim',])
      
      # Find where the SSB (Age structured) OR Biomass (Aggregated) in relation to Btrig points.
      b.pos <- apply(matrix(1:iter,1,iter),2, function(i) findInterval(b.datyr[i], Brefs[,i]))  # [it]
      Ftg   <- ifelse(b.pos == 0, 0, ifelse(b.pos == 1, ref.pts['Floss',], ref.pts['Fmsy',]))
    
      
    } else if (rule == 2) { # HCR2: Increased F
      
      Flow <- ref.pts['Flow',]
      Fmsy <- ref.pts['Fmsy',]
      Bloss <- ref.pts['Bloss',]
      Blim <- ref.pts['Blim',]
      
      # Checking reference points:
      if ( sum(ref.pts['Bloss',]>ref.pts['Blim',])>0 )
        stop("Check reference points for '", stknm, "'. In advice.ctrl[['", stknm, "']]$ref.pts, necessarily Bloss <= Blim")
      
      Brefs <- rbind(ref.pts['Bloss',],ref.pts['Blim',])
      
      # Find where the SSB (Age structured) OR Biomass (Aggregated) in relation to Btrig points.
      b.pos <- apply(matrix(1:iter,1,iter),2, function(i) findInterval(b.datyr[i], Brefs[,i]))  # [it]
      Ftg   <- ifelse(b.pos == 0, 0, ifelse(b.pos == 1,(Flow-(Fmsy-Flow)*Bloss/(Blim-Bloss)) + 
                                                        ((Fmsy-Flow)/(Blim-Bloss))*b.datyr, Fmsy))
    } else if (rule == 3) { # HCR3, same as HCR1 but with different parameters

      # ad-hoc modification of rule parameters 
      ref.pts['Blim',] <- 196334
      ref.pts['Floss',] <- 0.027
      ref.pts['Fmsy',] <- 0.032
      
      Brefs <- rbind(ref.pts['Bloss',],0.8*ref.pts['Blim',])

      # Find where the SSB (Age structured) OR Biomass (Aggregated) in relation to Btrig points.
      b.pos <- apply(matrix(1:iter,1,iter),2, function(i) findInterval(b.datyr[i], Brefs[,i]))  # [it]
      Ftg   <- ifelse(b.pos == 0, 0, ifelse(b.pos == 1, ref.pts['Floss',], ref.pts['Fmsy',]))
      
      
    }else if (rule == 4) { # HCR4: Same as HCR2 but with different parameters
      
      # ad-hoc modification of rule parameters 
      ref.pts['Blim',] <- 196334
      ref.pts['Flow',] <- 0.023
      ref.pts['Fmsy',] <- 0.032

      Flow <- ref.pts['Flow',]
      Fmsy <- ref.pts['Fmsy',]
      Bloss <- ref.pts['Bloss',]
      Blim <- ref.pts['Blim',]
      
      Brefs <- rbind(ref.pts['Bloss',],ref.pts['Blim',])
      
      # Find where the SSB (Age structured) OR Biomass (Aggregated) in relation to Btrig points.
      b.pos <- apply(matrix(1:iter,1,iter),2, function(i) findInterval(b.datyr[i], Brefs[,i]))  # [it]
      Ftg   <- ifelse(b.pos == 0, 0, ifelse(b.pos == 1,(Flow-(Fmsy-Flow)*Bloss/(Blim-Bloss)) + 
                                              ((Fmsy-Flow)/(Blim-Bloss))*b.datyr, Fmsy))
    }
        
      # Fill the 0-s and NA-s with almost 0 values to avoid problems when the fishery is closed for example, or there is no catch...
      stk@harvest[stk@harvest < 0] <- 0.00001

      stk@catch.n[is.na(stk@catch.n)] <- 1e-6
      stk@landings.n[is.na(stk@landings.n)] <- 0
      stk@discards.n[is.na(stk@discards.n)] <- 1e-6

      stk@catch.n[stk@catch.n==0] <- 1e-6
      stk@landings.n[stk@landings.n==0] <- 1e-6
      stk@discards.n[stk@discards.n==0] <- 0

      stk@catch <- computeCatch(stk)
      stk@catch[is.na(stk@catch)] <- 0
      stk@landings <- computeLandings(stk)
      stk@discards <- computeDiscards(stk)
      
      ageStruct <- ifelse(dim(stk@m)[1] > 1, TRUE, FALSE)
      
      if(ageStruct == TRUE){
        if(any(stk@catch[,tail(dimnames(stk@catch)$year,3)]<1e-2)){
          stk <- stf_correctSel(stk, nyears = 1, wts.nyears = wts.nyears, fbar.nyears = fbar.nyears, f.rescale = f.rescale) #, disc.nyrs = disc.nyears)
        }else{
          stk <- FLash::stf(stk, nyears = 1, wts.nyears = wts.nyears, fbar.nyears = fbar.nyears, f.rescale = f.rescale) #, disc.nyrs = disc.nyears)
          
        }}else{
          stk <- stfBD(stk, nyears = 1, wts.nyears = wts.nyears, fbar.nyears = fbar.nyears)}
      
      for(i in 1:iter){
        
        if(rule == 1 & Ftg[i]==ref.pts['Floss',]){
          fwd.ctrl <- fwdControl(data.frame(year = c(assyrnumb+1), quantity = c('f','ssb'),rel.year=c(NA,assyrnumb)))
          fwd.ctrl@trgtArray <- array(NA, dim = c(2, 3, 1:iter), dimnames=list(rep(assyrnumb+1,2), c("min","val","max"),iter=1:iter))
          #Set f 
          fwd.ctrl@trgtArray[1,2,] <- Ftg[i]
          #Set target in SSB to increase 5% when B1+ between bloss and 0.8 blim
          fwd.ctrl@trgtArray[2,1,] <- 1.051
          
        } else {
          
          fwd.ctrl <- fwdControl(data.frame(year = c(assyrnumb+1),  val = c(Ftg[i]), quantity = c('f')))
        }
        
        stki <- iter(stk, i)
        
        if(dim(stki@m)[1] > 1){
          # First estimate/extract the SR model and params.
          sr.pars  <- advice.ctrl[[stknm]]$sr$params # sr parameters if specified.
          sr.model <- advice.ctrl[[stknm]]$sr$model  # sr model, mandatory.
          if(is.null(sr.pars)){                   # if params missing => estimate the parameters using the specified years.
            if(is.null(advice.ctrl[[stknm]]$sr$years)) sr.yrs <- which(round(quantSums(stocks[[stknm]]@stock.n))!=0)[1]:(year-1)# yr0 missing => use all data years, except the assessment year for which rec is unknown
            else{
              y.rm <- as.numeric(advice.ctrl[[stknm]]$sr$years['y.rm'])
              nyrs <- as.numeric(advice.ctrl[[stknm]]$sr$years['num.years'])
              sr.yrs <- yrsnames[(year-y.rm-nyrs + 1):(year-y.rm)]
            }
            rec <- stki@stock.n[1,sr.yrs]
            ssb <- ssb(stki)[,sr.yrs]
            
            # if rec.age != 0 adjust rec and ssb.
            rec.age <- as.numeric(dimnames(rec)[[1]])
            if(rec.age != 0){
              rec <- rec[, -(1:rec.age),]
              ssb <- ssb[, 1:(dim(ssb)[2] - rec.age),]
            }
            
            if(sr.model != 'geomean') sr.pars <- try(params(fmle(FLSR(rec = rec, ssb = ssb, model = sr.model))), silent = TRUE) 
            
            if(class(sr.pars) == 'try-error' | sr.model == 'geomean'){
              sr.model <- 'geomean'
              sr.pars <- c(prod(c(rec))^(1/length(c(rec))))
              sr.pars <- FLPar(a = ifelse(is.na(sr.pars), 0, sr.pars))
            }
            
            sr1 <- sr.pars
          }
          else{ # sr.pars not null
            if(i == 1){
              sr1 <- iter(sr.pars,i)
            }
            sr1[] <-  iter(sr.pars,i)[]
          }
          
          stki <- fwd(stki, ctrl = fwd.ctrl, sr = list(model = sr.model, params = sr1))

        }
        else{
          
          # Extract the years to calculate the mean historical growth of the stock
          if(is.null(advice.ctrl[[stknm]]$growth.years))   growth.years <- max(1,(year - 11)):(year-1)
          else{
            y.rm <- as.numeric(advice.ctrl[[stknm]]$growth.years['y.rm'])
            nyrs  <- as.numeric(advice.ctrl[[stknm]]$growth.years['num.years'])
            growth.years <- yrsnames[(year-y.rm-nyrs + 1):(year-y.rm)]
          }
          
          stki <- fwdBD(stki, fwd.ctrl, growth.years)
        }
        
        yy <- ifelse(slot(stki, Cadv)[,year+1] == 0, 1e-6, slot(stki, Cadv)[,year+1])
        
        advice[['TAC']][stknm,year+1,,,,i] <- yy # The TAC is given in terms of CATCH.
        
      }
      
      cat("Advice for ",dimnames(advice[['TAC']])$year[year+1],": ", yy, "\n")
  
  } else stop("Rule ",rule,"not valid for ",pilHCRs,". Please check advice.ctrl[[stknm]]$rule object.",sep="")
  
   return(advice)

}


stf_correctSel <-  function(object, nyears=3, wts.nyears=3, fbar.nyears=wts.nyears, f.rescale=FALSE,
                           arith.mean=TRUE, na.rm=TRUE, end=dims(object)$maxyear + nyears)
{
  dims <- dims(object)
  
  # check nyears and end match
  if(missing(nyears))
    nyears <- as.numeric(end) - dims$maxyear
  else if(dims$maxyear + nyears != end)
    stop("'nyears' and 'end' do not match: ", dims$maxyear + nyears, " vs. ", end)
  years      <- ac((dims$maxyear+1):end)
  wts.years  <- ac(seq(dims$maxyear-wts.nyears+1, dims$maxyear))
  fbar.ages  <- ac(range(object, 'minfbar'):range(object, 'maxfbar'))
  fbar.years <- ac(seq(dims$maxyear-fbar.nyears+1, dims$maxyear))
  years.catch_G0<- which(object@catch> 1e-2)
  fbar.years <- tail(years.catch_G0,3)
  nit <- dim(object@catch)[6]
  fbar.years.iters <- matrix(0,nrow=3,ncol=nit)
  
  for(ii in 1:nit){
    years.catch_G0<- which(iter(object@catch,ii)> 1e-5)
    fbar.years.iters[,ii] <- tail(years.catch_G0,3)
  }
  
  
  # arith or geometric
  if(arith.mean)
    fmean <- mean
  else  
    fmean <- function(x) exp(mean(log(x)))
  
  # window object
  res <- window(object, end=end)
  
  # average slots
  # *.wt, mat, m and *.spwn as average over wts.years
  for (i in c('catch.wt', 'landings.wt', 'discards.wt', 'stock.wt', 'mat', 'm', 'harvest.spwn', 'm.spwn')){
    flq<- apply(slot(res, i)[,wts.years], c(1,3:6),fmean, na.rm=na.rm)
    
    if (i=='catch.wt') for(ii in 1:nit)
      if (any(is.na(iter(flq,ii))))
        iter(flq,ii)  <- apply(iter(slot(res, 'catch.wt'),ii)[,fbar.years.iters[,ii]], c(1,3:6), fmean, na.rm=na.rm)
    
    for (j in years)
      slot(res, i)[,j] <-flq
  }
  
  # landings.n and discards.n as proportions of wts.years
  for (i in years)
    slot(res, 'discards.n')[,i] <- apply(slot(res, 'discards.n')[, wts.years]/slot(res, 'catch.n')[, wts.years], c(1,3:6), mean)
  slot(res, 'landings.n')[,years] <- 1 - slot(res, 'discards.n')[,years]
  
  # first f get the dimensions and after harvest as mean over fbar.years.iter
  f <-apply(slot(res, 'harvest')[, wts.years], c(1,3:6), fmean, na.rm=na.rm)
  for(ii in 1:nit){
    iter(f,ii)  <-apply(iter(slot(res, 'harvest'),ii)[,fbar.years.iters[,ii]], c(1,3:6), fmean, na.rm=na.rm)}
  
  for (i in years)
    slot(res, 'harvest')[,i] <- f
  
  # f.rescale
  if(f.rescale == TRUE)
  {
    # mean f over fbar ages and years
    fbar <- yearMeans(apply(slot(res, 'harvest')[fbar.ages, wts.years], c(2:6), mean,
                            na.rm=na.rm))
    lastfbar <- apply(slot(res, 'harvest')[fbar.ages, tail(wts.years,1)], 3:6, mean,
                      na.rm=na.rm)
    
    for(ii in 1:nit){
      iter(fbar,ii)[]  <- mean(apply(iter(slot(res, 'harvest'),ii)[fbar.ages, fbar.years.iters[,ii]], c(2:6), mean,
                                     na.rm=na.rm))
      iter(lastfbar,ii)[] <- apply(iter(slot(res, 'harvest'),ii)[fbar.ages, tail(fbar.years.iters[,ii],1)], 3:6, mean,
                                   na.rm=na.rm)
      iter(slot(res, 'harvest'),ii)[, years] <- sweep(iter(slot(res, 'harvest'),ii)[, years], 3:6,iter(fbar,ii), '/')
      iter(slot(res, 'harvest'),ii)[, years] <- sweep(iter(slot(res, 'harvest'),ii)[, years], 3:6, iter(lastfbar,ii), '*')
    }
    
    
    # # fbar for last REAL year
    # lastfbar <- apply(slot(res, 'harvest')[fbar.ages, tail(fbar.years,1)], 3:6, mean,
    #   na.rm=na.rm)
    
    # divide by fbar and multiply by lastfbar
    # slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, fbar, '/')
    # slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, lastfbar, '*')
  }
  return(res)
}
# }}}



