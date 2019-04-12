
setwd("~/GitHub/FLBEIA_mseIBpil/")


Blim <- 337448


for (sc in c("ASSss3_HCR6_RECmix_INNvar_OERnaq", "ASSss3_HCR6_RECmed_INNvar_OERnaq")) {
  
  print(sc)
  
  # Load data
  load( file.path("output/output_scenarios", paste("results2018_",sc,".RData",sep="")))
  
  # out.bio
  
  
  # Subset to projected years
  dat <- subset(out.bio, year>=2018)[,c("year","iter","biomass")]
  
  
  # Output
  res <- data.frame()
  
  for (it in 1:1000) {
    
    dd <- subset(dat, iter==it)
    d1 <- dd$biomass>=Blim
    yrc <- which(d1==TRUE)[1]               # first year with SSB>=Blim
    if (!is.na(yrc)) {
      lml <- any(d1[yrc:length(d1)]==FALSE) # falling again below Blim?
      nr  <- 0                              # iteration with no recovery? (i.e. always SSB<Blim)
    } else {
      lml <- FALSE  # falling again below Blim?
      nr <- 1       # iteration with no recovery? (i.e. always SSB<Blim)
    }
    
    res <- rbind(res, c(it, d1, lml, nr))
  }
  
  names(res) <- c("iter", dd$year, "low_mix_low", "no_recover")
  
  # res
  
  plml <- sum(res$low_mix_low)/nrow(res) # 0.168 (mix) | 0.306 (med)
  print(paste("Prob low-mix-low:", plml))
  
  pnr  <- sum(res$no_recover)/nrow(res)  # 0.47  (mix) | 0.001 (med)
  print(paste("Prob no recover:", pnr))
  
  
  # Save data
  write.table( res, file.path("output", paste("SSBgeBlim_",sc,".csv",sep="")), 
               dec = ".", sep = ";", row.names = FALSE)
  
  
}

