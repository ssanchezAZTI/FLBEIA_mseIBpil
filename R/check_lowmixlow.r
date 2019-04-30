
setwd("~/GitHub/FLBEIA_mseIBpil/")


Blim <- 337448


for (sc in c("ASSss3_HCR1_RECmix_INNvar_OERnaq", "ASSss3_HCR1_REClowmed_INNvar_OERnaq")) {
  
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
      lm <- all(d1[yrc:length(d1)]==TRUE) # keep above Blim?
      nr  <- 0                            # iteration with no recovery? (i.e. always SSB<Blim)
    } else {
      lml <- FALSE  # falling again below Blim?
      lm <- FALSE
      nr <- 1       # iteration with no recovery? (i.e. always SSB<Blim)
    }
    
    res <- rbind(res, c(it, d1, lml, lm, nr))
  }
  
  names(res) <- c("iter", dd$year, "low_med_low","low_med", "no_recover")
  
  # res
  
  plml <- sum(res$low_med_low)/nrow(res) 
  print(paste("Prob low-med-low:", plml))
  
  plm <- sum(res$low_med)/nrow(res) 
  print(paste("Prob low-med:", plm)) 
  
  pnr  <- sum(res$no_recover)/nrow(res)  
  print(paste("Prob no recover:", pnr))
  
  
  # Save data
  write.table( res, file.path("output", paste("SSBgeBlim_",sc,".csv",sep="")), 
               dec = ".", sep = ";", row.names = FALSE)
  
  
}

