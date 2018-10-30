MakeSwarp = function(RA,DEC,imagesize,backsize,filter,input,output,doswarp=FALSE){
  
  ###########
  ## TO DO ##
  ###########
  .libPaths(c("/group/pawsey0160/skoushan/R_Library/",.libPaths()))
  library('celestial')
  library(data.table)
  
  Master <- fread("/group/pawsey0160/skoushan/waves/viking_finalmaster_2018.csv",header = TRUE)
  Filter = filter
  IMG_Size = imagesize  #IMAGE SIZE (1 sq deg + 2 arc min)
  B_Size = backsize     #BACKGROUND SIZE
  
  for (n in 1:length(Filter)){
    
    if(RA > 150 && RA < 230 && DEC > -5 && DEC < 5){
    
      
    #########
    # NORTH #
    #########
    # DIRECTORIES
    swarp = "/group/pawsey0160/SWarp/bin/swarp" # SWARP EXECUTE
    FITS = paste(" /group/pawsey0160/waves/viking/North/Large/",Filter[n],"/",sep='') # LOCATION FOR ALL FITS FILES
    txt = input # LOCATION FOR ALL INDIVIDUAL TXT FILES
    Config = paste("/group/pawsey0160/skoushan/swarp/swarp.config") # LOCATION FOR SWARP CONFIG FILE
    
    i = RA
    j = DEC
    # DEFINING RA AND DEC USING CELESTIAL TO READ ON SWARP
    RA_Cen = deg2hms((i),type='cat',sep=':')
    DEC_Cen = deg2dms((j),type='cat',sep=':')
    
    
    # DEFINING RA AND DEC USING IN FITS FILE NAME
    RA = round(i,digits=2)
    DEC = round(j,digits=2)  
    
    # DEFNING SEPARATE REGIONS AS 1 SQ DEG, HAVING THE MARGIN OF 0.5 SQ DEG TO JUST READ AND LOAD THE FITS FILES
    Marg = ((0.339*imagesize)/3600)*0.5
    Reg <- subset(Master,Master$DECchip > j-1-Marg & Master$DECchip < j+Marg & Master$RAchip > i-Marg & Master$RAchip < i+1+Marg & Master$filter == paste(Filter[n]))
    Names <- Reg$pawprint
    
    } else if (DEC > -36 && DEC < -26) {
    
    #########
    # SOUTH #
    #########
    South_Filter <- subset(Master,Master$filter == paste(Filter[n]) & Master$DECchip<(-20))
    South_R <- subset(South_Filter,South_Filter$RAchip>=300 & South_Filter$RAchip<=365)
    South_L <- subset(South_Filter,South_Filter$RAchip>-5 & South_Filter$RAchip<60)
    
    South_R$RAchip <- South_R$RAchip - 360
    South <- rbind(South_L,South_R)
    
    # DIRECTORIES
    swarp = "/group/pawsey0160/SWarp/bin/swarp" # SWARP EXECUTE
    FITS = paste(" /group/pawsey0160/waves/viking/South/",Filter[n],"/",sep='') # LOCATION FOR ALL FITS FILES
    txt = input # LOCATION FOR ALL INDIVIDUAL TXT FILES
    Config = paste("/group/pawsey0160/skoushan/swarp/swarp.config") # LOCATION FOR SWARP CONFIG FILE
    
    i = RA
    j = DEC
    
    if (i<0) {
      
      # DEFINING RA AND DEC USING CELESTIAL TO READ ON SWARP
      RA_Cen = deg2hms((i%%360),type='cat',sep=':')
    } else {RA_Cen = deg2hms((i),type='cat',sep=':')}
    
    DEC_Cen = deg2dms((j),type='cat',sep=':')
    
    # DEFINING RA AND DEC USING IN FITS FILE NAME
    if (i<0) {
      RA = round(i%%360,digits=2)%%360} else {RA = round(i,digits=2)%%360}
    DEC = round(j,digits=2)
    
    # DEFNING SEPARATE REGIONS AS 1 SQ DEG, HAVING THE MARGIN OF 0.5 SQ DEG TO JUST READ AND LOAD THE FITS FILES
    Marg = ((0.339*imagesize)/3600)*0.5
    Reg <- subset(South,South$DECchip > j-1-Marg & South$DECchip < j+Marg & South$RAchip > i-Marg & South$RAchip < i+1+Marg & South$filter == paste(Filter[n]))
    Names <- Reg$pawprint
    
    } else {print('--- RA or DEC is not within VIKINGs range! ---')}
    
    #########
    # GO ON #
    #########
    # READING THE REGIONS INCLUDE DATA (IF LENGTH(NAMES) = 0, NO DATA IS COPIED TO TXT FILE)
    if (length(Names) > 32){
      
      for (k in 0:(length(Names)+1)){
        
        if (k==0) {
          cat(swarp,file = paste(txt,RA,"_",DEC,"_",Filter[n],".txt",sep=''),append = TRUE)}
        
        else if (k>0 & k<(length(Names)+1)) {
          
          # WHERE THE FITS FILES HAS BEEN LOCATED
          cat(paste(FITS,paste(gsub('.{5}$', '', Names[k]),"_",Filter[n],"_", Reg$detector[k],".fits",sep=''),sep='') ,file = paste(txt,RA,"_",DEC,"_",Filter[n],".txt",sep=''),append = TRUE)}
        
        else if (k==(length(Names)+1)) {
          
          Output = output # LOCATION FOR OUTPUT
          
          # WHERE THE OUTPUT GO
          if(Filter[n]=='Ks'){
            cat(paste(' ','-c',' ',Config,' ','-','IMAGEOUT_NAME',' ',Output,'K.fits',' ','-','IMAGE_SIZE',' ',IMG_Size,',',IMG_Size,' ','-','CENTER',' ',RA_Cen,',',DEC_Cen,' ','-','BACK_SIZE',' ',B_Size,sep=''),"\n",file = paste(txt,RA,"_",DEC,"_",Filter[n],".txt",sep=''),append = TRUE)}
          else{cat(paste(' ','-c',' ',Config,' ','-','IMAGEOUT_NAME',' ',Output,Filter[n],'.fits',' ','-','IMAGE_SIZE',' ',IMG_Size,',',IMG_Size,' ','-','CENTER',' ',RA_Cen,',',DEC_Cen,' ','-','BACK_SIZE',' ',B_Size,sep=''),"\n",file = paste(txt,RA,"_",DEC,"_",Filter[n],".txt",sep=''),append = TRUE)}
          
        }
        
        
        else if (Names==0) {
        }
      }
      
    }
    
  }      
  
  
  for (n in 1:length(Filter)){
    if (doswarp==TRUE){
      system(paste('bash',' ',input,RA,'_',DEC,'_',Filter[n],'.txt',sep=''))
    } else if (doswarp==FALSE){}
  }
  
} #function
