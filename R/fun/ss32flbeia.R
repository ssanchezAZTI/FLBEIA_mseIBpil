#########################################################
#### Function to read from FLstock object,
#### create SS3 files, from existing reference ones,
#### run ss3 assessment with new data,
#### update FLR stock object with estimates from SS3.
#########################################################

########################################################
### Leire Citores september-october 2018 ###############
########################################################

ss32flbeia<-function(stock,indices,control,covars=covars){
  #save iteration number
  runi<-control$run_it
  
  #last year of the new stock object
  lasty<-range(stock)["maxyear"]
  stock@catch <- computeCatch(stock)

  ## if catch < 10^-6 , give small value to total catch, remove catch at age (done some lines later)
  ## and put the wt from year before. Not to have problems in SS3
  stock@catch[,which(stock@catch<10^-6)]<-10^-6
  stock@catch.wt[,which(stock@catch<10^-6)]<-stock@catch.wt[,ac(lasty-1)]
  
  #print
  cat("catches in it",runi,range(stock)["maxyear"],"\n", stock@catch[,ac(range(stock)["maxyear"])],"\n")
  #flush.console()


  #ref_name: reference ss3 assessment folder name
  ref_name<-control$ref_name

  #directory where the folder with the reference ss3 assessment is located
  assess_dir<-control$assess_dir
  
  # get current working directory
  dir0<-getwd()
  
  #set the new working directory 
  #assess_dir<-"C:\\Leire\\Sardina\\WGHANSA2018\\ss3R\\"
  setwd(assess_dir)
  
  #create a new folder where the new assessment will be run
  dir<-paste0("assess_temp",runi,lasty)
  dir.create(dir)
  
  #copy the reference assessment folder  into the new folder
  file.copy(ref_name, dir,recursive = T)
  
  #set the working directory inside this folder
  #at the end of the assessment this new folder will be deleted
  #the reference assessment is always kept
  #temp_dir<-paste0(assess_dir,paste0(dir,paste0("/",ref_name)))
  temp_dir<-paste0(dir,paste0("/",ref_name))
  setwd(temp_dir)

  
  
  ################################################################
  #### read data from new FRL objects and write to SS3 files
  #################################################################
  
  #last year of the new stock object
  lasty<-range(stock)["maxyear"]

  #read reference assessment data file
  datss<-SS_readdat("sardine.dat",verbose=F)
  
  ##control file
  ctl<-readLines("sardine.ctl")

  #natural mortality vector
  ctl[27]<-paste(m(stock)[,1],collapse=" ")
  #update the year in the control file (from reference assessment year to actual year)
  ctl_new<-gsub(pattern = ac(datss$endyr), replace = ac(lasty), x = ctl)
  #update the year recruitmen devs (assessment year-1)
  ctl_new<-gsub(pattern = ac(datss$endyr-1), replace = ac(lasty-1), x = ctl_new)
  #write the new control file, it overwrites the old one
  writeLines(ctl_new, con="sardine.ctl")
  
  #total catch
  catch<-melt(catch(stock)[,])[,c("value","year")]
  catch$seas<-1
  colnames(catch)<-colnames(datss$catch)
  datss$catch<-catch
  
  #age structured catch and index
  catchn<-dcast(na.omit(melt(catch.n(stock)[,])[,c("value","year","age")]),year~age)
  catchn<-cbind(catchn[,1],subset(datss$agecomp,FltSvy==1)[1,2:9],catchn[,-1])
  lastrow<-dim(catchn)[1] 
  #ADI!!! no catch at age for the last year
  catchn<-catchn[-lastrow,]
  ## remove catch at age when catch<10^6
  catch0years<-which(catchn[,"4"]<10^-6)
  if(length(catch0years)>0){
  catchn<-catchn[-catch0years,]}
  # set manullay sample size for catch.n for years > 1999
  catchn[,"Nsamp"][catchn[,1]>1990]<-75
  

  indexn<-dcast(na.omit(melt(indices[[1]]@index[,])[,c("value","year","age")]),year~age)
  indexn<-cbind(indexn[,1],subset(datss$agecomp,FltSvy==2)[1,2:9],indexn[,-1])
  colnames(indexn)<-colnames(catchn)<-colnames(datss$agecomp)
  agecomp<-rbind(catchn,indexn)
  datss$agecomp<-agecomp
 
  #biomass indices
  index2<-cbind(indexn[,1],rowSums(indexn[,c(10:16)]))
  se_log2<-subset(datss$CPUE,index==2)$se_log
  index2<-cbind(year=index2[,1],seas=1,index=2,obs=index2[,2],se_log=mean(se_log2)) #c(se_log2,se_log2_new))
  
  index3<-na.omit(melt(indices[[2]]@index[,])[,c("value","year")])
  se_log3<-subset(datss$CPUE,index==3)$se_log[1]
  index3<-cbind(year=index3$year,seas=1,index=3,obs=index3$value,se_log=se_log3)
  
  datss$CPUE<-as.data.frame(rbind(index2,index3))
  
  ################################################################################
  
  #number of lines
  datss$styr<-min(catch$year)
  datss$endyr<-an(lasty)
  datss$N_catch<-dim(datss$catch)[1]
  datss$N_cpue<-dim(datss$CPUE)[1]
  datss$N_agecomp<-dim(datss$agecomp)[1]
  
  #write the new data file, it overwrites the old one
  SS_writedat(datss,"sardine.dat", overwrite = T,verbose=F)
  
  
  ##wtatage file
  #stock weight (fleet=0)
  stockwt<-t(stock.wt(stock)[,,drop=T])
  stockwt<-cbind(rownames(stockwt),1,1,1,1,0,stockwt)
  stockwt<-apply(stockwt,1,function(x){paste(x,collapse="\t")})
  names(stockwt)<-NULL
  
  # weigth for age structured acoustic index (fleet=2) (same as stock weight)
  index2wt<-t(stock.wt(stock)[,,drop=T])
  index2wt<-cbind(rownames(index2wt),1,1,1,1,2,index2wt)
  index2wt<-apply(index2wt,1,function(x){paste(x,collapse="\t")})
  names(index2wt)<-NULL
  
  #catch weight (fleet 1 and -1)
  catchwt<-t(catch.wt(stock)[,,drop=T])
  catchwt<-cbind(rownames(catchwt),1,1,1,1,1,catchwt)
  catchwt<-apply(catchwt,1,function(x){paste(x,collapse="\t")})
  names(catchwt)<-NULL
  
  catchwt_1<-t(catch.wt(stock)[,,drop=T])
  catchwt_1<-cbind(rownames(catchwt_1),1,1,1,1,-1,catchwt_1)
  catchwt_1<-apply(catchwt_1,1,function(x){paste(x,collapse="\t")})
  names(catchwt_1)<-NULL
  
  #maturity (fleet=-2)
  mat<-t((mat(stock)*stock.wt(stock))[,,drop=T])
  mat<-cbind(rownames(mat),1,1,1,1,-2,mat)
  mat<-apply(mat,1,function(x){paste(x,collapse="\t")})
  names(mat)<-NULL
  
  #read the reference weight file
  wta<-readLines("wtatage.ss")[1:7] 
  #generate the new weigth matrix with the data from FLR objects
  wta_new<-c(wta,stockwt,catchwt,mat,catchwt_1,index2wt)
  #udate number of lines
  wta_new[1]<-ac(length(wta_new)-length(wta))
  #write the new weight file, it overwrtites the old one
  writeLines(wta_new, con="wtatage.ss")
  
  ################################################################
  ##                        execute SS3 
  ## (be careful to use the adecuate executable depending on the 
  ## operating sistem, windows or linux)
  ################################################################
  
  #system("./ss3")
  system('./SS3.exe')
  #system("ss3.exe")
  #system("SS3.exe -mcmc 100000 -mcsave 100")
  
  
  
  #####################################################
  ### S3 output back to FLR. Just harvest and stock:
  #####################################################

  dir.assess<-paste0(getwd(),"")
  assess <- SS_output(dir = dir.assess, model = "ss3", forecast=F,printstats=F,verbose=F,covar=F)
  maxyear <- assess$endyr 
  ages <- assess$agebins
  years <- assess$startyr:assess$endyr
  
  #------------------------------------
  # extract assessment outputs:
  # F-AT-AGE, REFERENCE F
  # we need to calculate F-at-age from apical F and selectivity at age
  #------------------------------------
  
  selectivity <- subset(assess$ageselex, fleet==1 & factor=="Asel2" & year %in% years, select=c("year", ac(ages)))
  
  idx <- grep("F_", assess$derived_quants$LABEL)
  f.apical <- data.frame(f=assess$derived_quants[idx,"Value"])
  f.apical$year <- years
  
  f.apsel <- merge(f.apical, selectivity, all.x = T,by="year")
  
  f <- f.apsel$f * f.apsel[,ac(ages)]
  
  harvest=FLQuant(unname(as.matrix(t(f))),quant="age",units="f", 
                  dimnames=list(age=ac(ages), year=ac(years)))
  
  
  #------------------------------------
  # extract assessment outputs:
  # NUMBERS-AT-AGE
  #------------------------------------
  
  natage <- subset(assess$natage, Era=="TIME" & `Beg/Mid`=="B" , select=ac(ages))
  
  stock.n <- FLQuant(unname(as.matrix(t(natage))), quant='age', units='NA', 
                     dimnames=list(age=ac(ages), year=ac(years)))
  
  
  #update stock object
  harvest(stock)<-harvest
  stock.n(stock)<-stock.n
  stock(stock)<-computeStock(stock)
  #outss3<-ss3toFLR(dir.assess=paste0(assess_dir,to,"\\"))
  
  ##fill covars to see retros
  covars$ssb[ac(lasty),ac(years)]<-ssb(stock)[,]
  covars$rec[ac(lasty),ac(years)]<-rec(stock)[,]
  covars$fbar[ac(lasty),ac(years)]<-fbar(stock)[,]
  
  params<-assess$parameters
  q2<-exp(subset(params,Label=="LnQ_base_2_Acoustic_survey")[,"Value"])
  q3<-exp(subset(params,Label=="LnQ_base_3_DEPM_survey")[,"Value"])
  covars$qs[1,ac(lasty)]<-q2
  covars$qs[2,ac(lasty)]<-q3
  
  covars$sel[,ac(lasty),3,,,]<-as.numeric(subset(selectivity,year==lasty)[1,-1])
  covars$sel[,ac(lasty),2,,,]<-as.numeric(subset(selectivity,year==2004)[1,-1])
  covars$sel[,ac(lasty),1,,,]<-as.numeric(subset(selectivity,year==1986)[1,-1])
  
  covars$conv[,ac(lasty)]<-assess$maximum_gradient_component
  
  #######################
  ###delete files
  #######################
  setwd(assess_dir)
  unlink(paste0(assess_dir,dir),recursive=T)
  #return to the original working directory
  setwd(dir0)

  return(list(stock = stock,covars=covars))
}