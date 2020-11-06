rm(list=ls())
#setwd()
library(ggplot2)
#####################INITIAL CONDITIONS######################

scenario <- c("historic", "RCP4.5", "RCP8.5")
sensitivity<- c("yes")
t=100
timedays= 365*t
yr= 2010
scalingfactor= 1000
drymatter= 3*scalingfactor    #Kg

##################DEFINITIONS#########################

RCP4.5n2o<- vector("numeric", length= t)
RCP4.5ch4<- vector("numeric", length= t)
RCP8.5n2o<- vector("numeric", length= t)
RCP8.5ch4<- vector("numeric", length = t)

AnnualGLobalch4<- scan("ch41984-2017.txt")
RCP4.5yearlych4<- scan("RCP4.5ch4.txt")
RCP8.5yearlych4<- scan("RCP8.5ch4.txt")
ch4bias<--sum(AnnualGLobalch4[10:23]-(RCP4.5yearlych4[19:32]+RCP8.5yearlych4[19:32])/2)/14 #1993-2006

AnnualGLobaln2o<- scan("n2o1999-2017.txt")
RCP4.5yearlyn2o<- scan("RCP4.5n2o.txt") 
RCP8.5yearlyn2o<- scan("RCP8.5n2o.txt")
n2obias<- sum(AnnualGLobaln2o[1:12]-((RCP4.5yearlyn2o[25:36]+RCP8.5yearlyn2o[25:36])/2))/12 #1999-2010


RCP4.5ch4<- scan("RCP4.5ch4.txt")+ch4bias
RCP4.5n2o<- scan("RCP4.5n2o.txt")+n2obias
RCP8.5n2o<- scan("RCP8.5n2o.txt")+n2obias
RCP8.5ch4<- scan("RCP8.5ch4.txt") +ch4bias

#######INTERPOLATE RCP VALUES
CO2RCP4.5xval<- scan("IRFRCP4.5CO2XVAL.txt")
CO2RCP4.5yval<- scan("IRFRCP4.5CO2YVAL.txt")
CO2RCP8.5xval<- scan("IRFRCP8.5CO2XVAL.txt")
CO2RCP8.5yval<- scan("IRFRCP8.5CO2YVAL.txt")

CO2RCP4.5<- approx(CO2RCP4.5xval, CO2RCP4.5yval, seq(1:100), method= "linear", n=100)
CO2RCP8.5<- approx(CO2RCP8.5xval, CO2RCP8.5yval, seq(1:100), method= "linear", n=100)
IRF4.5<- c(1, CO2RCP4.5$y[1:99])
IRF8.5<- c(1, CO2RCP8.5$y[1:99])

#########

historicco2<- scan("historical co2 values.txt")
RCP4.5<- scan("RCP4.5CO2FUTVOL.txt")
RCP8.5<- scan("RCP8.5CO2FUTVOL.txt")

#########bias averaged from 1980- 2007, both RCPs were equal on this interval

co2bias<- sum(historicco2[1:27]-RCP4.5[1:27])/27
RCP4.5futvol<- RCP4.5+co2bias
RCP8.5futvol<- RCP8.5+ co2bias




EmissionsfactorN= 0.00041
EmissionsfactorC= 0.00596
EmissionsFactorCO2= 1.489
EmissionsfactorNOx<- 0.0009
EmissionsfactorVOC<- 0.0293
EmissionsfactorCO<- 0.127
EmissionsfactorOC= 0.00915 
EmissionsfactorBC= 0.00056 

GWP20CH4<- 84
GWP100CH4<- 28
GWP20NOx<- 19
GWP100NOx<- -11
GWP20VOC<- 14
GWP100VOC<- 4.5
GWP20CO<- 7.8
GWP100CO<- 2.2
GWP20BC<- 2900
GWP100BC<- 830
GWP20OC<- -160
GWP100OC<- -69

EarthArea= 510e12 #m^2
AtmMass= 5.1*10^18 #kg
AvgAtmMol= 0.02896 #kg/mol
AtmMolQuant= AtmMass/AvgAtmMol #mol

CH4lifetime= 12.4 #yr
Ozonelife= 23.4 #days
N2Olifetime= 121 

MethMol= 0.016043 #kg/mol
COMol= 0.02801 #kg/mol
CO2Mol= 0.044009 #kg/mol
NitMol= 0.044013 #kg/mol

RFperDU= 0.042

ConcentrationC <- vector("numeric", length = t)
VolumeO3<- vector("numeric", length= t)
concfraction<- vector("numeric", length = t)
ConcentrationCO2<- vector("numeric", length= t)
ConcentrationN <- vector("numeric", length = t)
RFBorealO3<- vector("numeric", length= t)
cumulativeRFCH4historic<- vector("numeric", length= t)
cumulativeRFCH4RCP4.5<- vector("numeric", length= t)
cumulativeRFCH4RCP8.5<- vector("numeric", length= t)
cumulativeRFN2Ohistoric<- vector("numeric", length= t)
cumulativeRFN2ORCP4.5<- vector("numeric", length= t)
cumulativeRFN2ORCP8.5<- vector("numeric", length= t)
cumulativeRFCO2historic<- vector("numeric", length= t)
cumulativeRFCO2RCP4.5<- vector("numeric", length= t)
cumulativeRFCO2RCP8.5<- vector("numeric", length= t)
cumulativeRFOZONE<- vector("numeric", length= t)
cumulativeRFCH4<- vector("numeric", length = t)

for (i in scenario){
  #####CH4#######
  if(i == "historic"){              
    ambientconc= AnnualGLobalch4[(yr-1984):(yr-1984+t-1)]
    ch4ref<- AnnualGLobalch4[(yr-1984):(yr-1984+t-1)]
    n2oref= AnnualGLobaln2o[(yr-1999):(yr-1999+t-1)]
  }else if(i == "RCP4.5"){
    ambientconc= RCP4.5ch4[(yr-1975):(yr-1975+t-1)]
    ch4ref= RCP4.5ch4[(yr-1975):(yr-1975+t-1)]
    n2oref= RCP4.5n2o[(yr-1975):(yr-1975+t-1)]
  }else if(i == "RCP8.5"){
    ambientconc= RCP8.5ch4[(yr-1975):(yr-1975+t-1)]
    ch4ref= RCP8.5ch4[(yr-1975):(yr-1975+t-1)]
    n2oref= RCP8.5n2o[(yr-1975):(yr-1975+t-1)]
  }
  
  
  CH4CONC <- function(dm, t){
    return((EmissionsfactorC*dm*exp(1)^(-t/CH4lifetime)))
  }
  CH4PPB<- function(Conc, fut){           
    return((Conc*10^9*AvgAtmMol/(AtmMass*MethMol))+fut)
  }
  for(g in 1:t){    
    ConcentrationC[g]<- CH4CONC(dm=drymatter, g-1)
  }
  VolumeC<- CH4PPB(ConcentrationC, fut= ambientconc)
  RFCH4 <- function(ch4, ch4o = ch4ref, N = n2oref){
    A<- 0.036*(ch4^0.5-ch4o^0.5)
    B<- (0.47*log(1+2.02*(10^-5)*(ch4^0.75)*(N^0.75))+(5.31*(10^-15)*ch4*(ch4^1.52)*(N^1.52)))
    C<- (0.47*log(1+2.02*(10^-5)*(ch4o^0.75)*(N^0.75))+(5.31*(10^-15)*ch4o*(ch4o^1.52)*(N^1.52))) 
    return(A-(B-C))
  }
  RFGlobeC <- RFCH4(VolumeC)
  RFBorealC <- RFGlobeC*(EarthArea)/scalingfactor 
  
  for(h in 1:t){
    cumulativeRFCH4[h]<-sum(RFBorealC[1:h])
  }
  
  ###################PRECURSORS#######################
  if(sensitivity== "yes"){
    
    precursor<- c("NOx", "CO", "VOC")
    aerosol<- c("Direct", "Indirect")
    for(q in c(precursor, aerosol)){
      
      
      if(q == "NOx"){
        ratio1<- (GWP20NOx*EmissionsfactorNOx)/ (GWP20CH4*EmissionsfactorC *(cumulativeRFCH4[1]/cumulativeRFCH4[20]))
        ratio20<- (GWP20NOx*EmissionsfactorNOx)/ (GWP20CH4*EmissionsfactorC)
        ratio100<- (GWP100NOx*EmissionsfactorNOx)/ (GWP100CH4*EmissionsfactorC)
      }else if(q == "CO"){
        ratio20<- (GWP20CO*EmissionsfactorCO)/ (GWP20CH4*EmissionsfactorC)
        ratio100<- (GWP100CO*EmissionsfactorCO)/ (GWP100CH4*EmissionsfactorC)
      }else if( q == "VOC"){
        ratio20<- (GWP20VOC*EmissionsfactorVOC)/ (GWP20CH4*EmissionsfactorC)
        ratio100<- (GWP100VOC*EmissionsfactorVOC)/ (GWP100CH4*EmissionsfactorC)
      }else if(q== "Direct"){
        ratio20BC<- (GWP20BC*EmissionsfactorBC)/ (GWP20CH4*EmissionsfactorC)
        ratio100BC<- (GWP100BC*EmissionsfactorBC)/ (GWP100CH4*EmissionsfactorC)
        ratio20OC<- (GWP20OC*EmissionsfactorOC)/ (GWP20CH4*EmissionsfactorC)
        ratio100OC<- (GWP100OC*EmissionsfactorOC)/ (GWP100CH4*EmissionsfactorC)
      }else if(q== "Experiment"){#### WILL Delete later##
        ratio20<- (GWP20NOx*EmissionsfactorNOx)/ (GWP20CH4*EmissionsfactorC)
        ratio100<- -.3*ratio20+ratio20
      }
      
      
      
      cumulativediff<- vector("numeric", length = t)
      for(h in 1:t){
        cumulativediff[h+1]<- cumulativeRFCH4[h+1]- cumulativeRFCH4[h]
      }
      cumulativediff20<- cumulativediff[21:100]
      cumulativeCH4scale1<- vector("numeric", length= 80)
      for(h in 1:80){
        cumulativeCH4scale1[h]<- sum(cumulativediff20[1:h])
      }
      fractionincrease<- cumulativeCH4scale1/cumulativeCH4scale1[80]
    
      GWPratio<- vector("numeric", length= t)
      for(h in 21:t){
        GWPratio[h]<- ratio20+(ratio100-ratio20)*fractionincrease[h-20]
      }
      for(h in 1:20){
        GWPratio[h]<- ratio20
      }
      
      if(q== "NOx"){
        x<-c(1, 20, 100)
        y<- c(ratio1, ratio20, ratio100)*10
        data<- data.frame(x, y)
        
        
        g<- nls(y ~ (a*exp(-x*t)+c), data = data, start = list( a= 4.5, t=0.12, c= -0.6), trace= TRUE, control= list(maxiter = 10, tol= 1e-12, warnOnly= TRUE))
        
        
        coefficients<- as.numeric(coef(g))
        
        
        exp.decay.ratio<- function(time){
          return(coefficients[1]*exp(-time*coefficients[2])+coefficients[3])
        }
        
        
        GWPratio<- vector("numeric", length= t)
        for(h in 1:t){
          GWPratio[h]<- exp.decay.ratio(h)/10
        }
      }
      
      
      
      cumulativeRF<- cumulativeRFCH4*GWPratio
      
      cumulativeRFmean<- vector("numeric", length= t)
      for(h in 1:t){
        cumulativeRFmean[h]<- cumulativeRF[h]/h
      }
      
      annualRF<- vector("numeric", length= t)
      for(h in 1:(t-1)){
        annualRF[t-h+1]<- cumulativeRF[t-h+1]-cumulativeRF[t-h]
      }
      annualRF[1]<- cumulativeRF[1]
      annualRF
      
      
      
      
      if(q == "CO"){
        annualRFCO<- annualRF
        cumulativeRFCO<- cumulativeRF
        cumulativeRFCOmean<- cumulativeRFmean
        annualRFCOtimeseries<- ts(annualRF, start= yr, frequency = 1)
        cumulativeRFCOtimeseries<- ts(cumulativeRF, start= yr, frequency = 1)
        cumulativeRFCOmeantimeseries<- ts(cumulativeRFmean, start= yr, frequency = 1)
        ratioCO<- GWPratio
        
      }else if(q == "NOx"){
        annualRFNOx<- annualRF
        cumulativeRFNOx<- cumulativeRF
        cumulativeRFNOxmean<- cumulativeRFmean
        annualRFNOxtimeseries<- ts(annualRF, start= yr, frequency = 1)
        cumulativeRFNOxtimeseries<- ts(cumulativeRF, start= yr, frequency = 1)
        cumulativeRFNOxmeantimeseries<- ts(cumulativeRFmean, start= yr, frequency = 1)
        ratioNOx<- GWPratio
        
      }else if( q == "VOC"){
        annualRFVOC<- annualRF
        cumulativeRFVOC<- cumulativeRF
        cumulativeRFVOCmean<- cumulativeRFmean
        annualRFVOCtimeseries<- ts(annualRF, start= yr, frequency = 1)
        cumulativeRFVOCtimeseries<- ts(cumulativeRF, start= yr, frequency = 1)
        cumulativeRFVOCmeantimeseries<- ts(cumulativeRFmean, start= yr, frequency = 1)
        ratioVOC<- GWPratio
      }else if(q== "Direct"){
        CumulativeRFBCyr20<- ratio20BC*cumulativeRFCH4[20]
        CumulativeRFOCyr20<- ratio20OC*cumulativeRFCH4[20]
        CumulativeRFDirectyr20<-CumulativeRFBCyr20+CumulativeRFOCyr20
        
        annualRFDirect<- vector("numeric", length = t)
        cumulativeRFDirect<- vector("numeric", length= t)
        cumulativeRFDirectmean<- vector("numeric", length= t)
        
        annualRFDirect[1]<- CumulativeRFDirectyr20
        for(h in 2:t){
          annualRFDirect[h]<- 0
        }
        cumulativeRFDirect[1:t]<- annualRFDirect[1]
        for(h in 1:t){
          cumulativeRFDirectmean[h]<- annualRFDirect[1]/h
        }
        
        annualRFDirecttimeseries<- ts(annualRFDirect, start= yr, frequency = 1)
        cumulativeRFDirecttimeseries<- ts(cumulativeRFDirect, start= yr, frequency = 1)
        cumulativeRFDirectmeantimeseries<- ts(cumulativeRFDirectmean, start = yr, frequency = 1)
      }else if(q== "Indirect"){
        ratio.source= "Ward"
        if(ratio.source== "Forester AR4"){
          direct.to.indirect<- -2.66
        }
        if(ratio.source == "Ward"){
          direct.to.indirect<- -1.64/0.13
        }
        annualRFIndirect<- direct.to.indirect*annualRFDirect
        cumulativeRFIndirect<- direct.to.indirect*cumulativeRFDirect
        cumulativeRFIndirectmean<- direct.to.indirect*cumulativeRFDirectmean
        annualRFIndirecttimeseries<- ts(annualRFIndirect, start= yr, frequency = 1)
        cumulativeRFIndirecttimeseries<- ts(cumulativeRFIndirect, start= yr, frequency = 1)
        cumulativeRFIndirectmeantimeseries<- ts(cumulativeRFIndirectmean, start = yr, frequency = 1)
        
      }else if(q== "Experiment"){
        annualRFEX<- annualRF
        cumulativeRFEX<- cumulativeRF
        cumulativeRFEXmean<- cumulativeRFmean
        annualRFEXtimeseries<- ts(annualRF, start= yr, frequency = 1)
        cumulativeRFEXtimeseries<- ts(cumulativeRF, start= yr, frequency = 1)
        cumulativeRFEXmeantimeseries<- ts(cumulativeRFmean, start= yr, frequency = 1)
        ratioEX<- GWPratio
      }
    }
  }
  
  
  
  if(i == "historic"){
    timeserieshistoricCH4<- ts(RFBorealC, start = yr, frequency = 1)
    for(h in 1:t){
      cumulativeRFCH4historic[h]<-sum(RFBorealC[1:h])/h
    }
    cumulativetsCH4historic<- ts(cumulativeRFCH4historic, start= yr, frequency = 1)
    if( sensitivity == "yes"){
      annualRFCOtimeserieshistoric<- annualRFCOtimeseries
      cumulativeRFCOtimeserieshistoric<- cumulativeRFCOtimeseries
      cumulativeRFCOmeantimeserieshistoric<- cumulativeRFCOmeantimeseries
      annualRFNOxtimeserieshistoric<- annualRFNOxtimeseries
      cumulativeRFNOxtimeserieshistoric<- cumulativeRFNOxtimeseries
      cumulativeRFNOxmeantimeserieshistoric<- cumulativeRFNOxmeantimeseries
      annualRFVOCtimeserieshistoric<- annualRFVOCtimeseries
      cumulativeRFVOCtimeserieshistoric<- cumulativeRFVOCtimeseries
      cumulativeRFVOCmeantimeserieshistoric<- cumulativeRFVOCmeantimeseries
      annualRFCOhistoric<- annualRFCO
      annualRFNOxhistoric<- annualRFNOx
      annualRFVOChistoric<- annualRFVOC
      annualRFDirecthistoric<- annualRFDirect
      annualRFIndirecthistoric<- annualRFIndirect
      cumulativeRFCOhistoric<- cumulativeRFCO
      cumulativeRFNOxhistoric<- cumulativeRFNOx
      cumulativeRFVOChistoric<- cumulativeRFVOC
      cumulativeRFDirecthistoric<- cumulativeRFDirect
      cumulativeRFIndirecthistoric<- cumulativeRFIndirect
      cumulativeRFCOmeanhistoric<- cumulativeRFCOmean
      cumulativeRFNOxmeanhistoric<- cumulativeRFNOxmean
      cumulativeRFVOCmeanhistoric<- cumulativeRFVOCmean
      cumulativeRFDirectmeanhistoric<- cumulativeRFDirectmean
      cumulativeRFIndirectmeanhistoric<- cumulativeRFIndirectmean
    }
  }else if(i == "RCP4.5"){
    timeseriesRCP4.5CH4<- ts(RFBorealC, start = yr, frequency = 1)
    for(h in 1:t){
      cumulativeRFCH4RCP4.5[h]<-sum(RFBorealC[1:h])/h
    }
    cumulativetsCH4RCP4.5<- ts(cumulativeRFCH4RCP4.5, start= yr, frequency = 1)
    if( sensitivity == "yes"){
      annualRFCOtimeseriesRCP4.5<- annualRFCOtimeseries
      cumulativeRFCOtimeseriesRCP4.5<- cumulativeRFCOtimeseries
      cumulativeRFCOmeantimeseriesRCP4.5<- cumulativeRFCOmeantimeseries
      annualRFNOxtimeseriesRCP4.5<- annualRFNOxtimeseries
      cumulativeRFNOxtimeseriesRCP4.5<- cumulativeRFNOxtimeseries
      cumulativeRFNOxmeantimeseriesRCP4.5<- cumulativeRFNOxmeantimeseries
      annualRFVOCtimeseriesRCP4.5<- annualRFVOCtimeseries
      cumulativeRFVOCtimeseriesRCP4.5<- cumulativeRFVOCtimeseries
      cumulativeRFVOCmeantimeseriesRCP4.5<- cumulativeRFVOCmeantimeseries
      annualRFCORCP4.5<- annualRFCO
      annualRFNOxRCP4.5<- annualRFNOx
      annualRFVOCRCP4.5<- annualRFVOC
      cumulativeRFCORCP4.5<- cumulativeRFCO
      cumulativeRFNOxRCP4.5<- cumulativeRFNOx
      cumulativeRFVOCRCP4.5<- cumulativeRFVOC
      cumulativeRFCOmeanRCP4.5<- cumulativeRFCOmean
      cumulativeRFNOxmeanRCP4.5<- cumulativeRFNOxmean
      cumulativeRFVOCmeanRCP4.5<- cumulativeRFVOCmean
      annualRFDirectRCP4.5<- annualRFDirect
      annualRFIndirectRCP4.5<- annualRFIndirect
      cumulativeRFDirectRCP4.5<- cumulativeRFDirect
      cumulativeRFIndirectRCP4.5<- cumulativeRFIndirect
      cumulativeRFDirectmeanRCP4.5<- cumulativeRFDirectmean
      cumulativeRFIndirectmeanRCP4.5<- cumulativeRFIndirectmean
    }
  }else if(i == "RCP8.5"){
    timeseriesRCP8.5CH4<- ts(RFBorealC, start = yr, frequency = 1)
    for(h in 1:t){
      cumulativeRFCH4RCP8.5[h]<-sum(RFBorealC[1:h])/h
    }
    cumulativetsCH4RCP8.5<- ts(cumulativeRFCH4RCP8.5, start= yr, frequency = 1)
    if( sensitivity == "yes"){
      annualRFCOtimeseriesRCP8.5<- annualRFCOtimeseries
      cumulativeRFCOtimeseriesRCP8.5<- cumulativeRFCOtimeseries
      cumulativeRFCOmeantimeseriesRCP8.5<- cumulativeRFCOmeantimeseries
      annualRFNOxtimeseriesRCP8.5<- annualRFNOxtimeseries
      cumulativeRFNOxtimeseriesRCP8.5<- cumulativeRFNOxtimeseries
      cumulativeRFNOxmeantimeseriesRCP8.5<- cumulativeRFNOxmeantimeseries
      annualRFVOCtimeseriesRCP8.5<- annualRFVOCtimeseries
      cumulativeRFVOCtimeseriesRCP8.5<- cumulativeRFVOCtimeseries
      cumulativeRFVOCmeantimeseriesRCP8.5<- cumulativeRFVOCmeantimeseries
      annualRFCORCP8.5<- annualRFCO
      annualRFNOxRCP8.5<- annualRFNOx
      annualRFVOCRCP8.5<- annualRFVOC
      cumulativeRFCORCP8.5<- cumulativeRFCO
      cumulativeRFNOxRCP8.5<- cumulativeRFNOx
      cumulativeRFVOCRCP8.5<- cumulativeRFVOC
      cumulativeRFCOmeanRCP8.5<- cumulativeRFCOmean
      cumulativeRFNOxmeanRCP8.5<- cumulativeRFNOxmean
      cumulativeRFVOCmeanRCP8.5<- cumulativeRFVOCmean
      annualRFDirectRCP8.5<- annualRFDirect
      annualRFIndirectRCP8.5<- annualRFIndirect
      cumulativeRFDirectRCP8.5<- cumulativeRFDirect
      cumulativeRFIndirectRCP8.5<- cumulativeRFIndirect
      cumulativeRFDirectmeanRCP8.5<- cumulativeRFDirectmean
      cumulativeRFIndirectmeanRCP8.5<- cumulativeRFIndirectmean
    }
  }
  
  #############N2O#################
  
  if(i == "historic"){
    ambientconc= AnnualGLobaln2o[(yr-1999):(yr-1999+t-1)]
    ch4ref<- AnnualGLobalch4[(yr-1984):(yr-1984+t-1)]
    n2oref= AnnualGLobaln2o[(yr-1999):(yr-1999+t-1)]
  }else if(i == "RCP4.5"){
    ambientconc= RCP4.5n2o[(yr-1975):(yr-1975+t-1)]
    ch4ref= RCP4.5ch4[(yr-1975):(yr-1975+t-1)]
    n2oref= RCP4.5n2o[(yr-1975):(yr-1975+t-1)]
  }else if(i == "RCP8.5"){
    ambientconc= RCP8.5n2o[(yr-1975):(yr-1975+t-1)]
    ch4ref= RCP8.5ch4[(yr-1975):(yr-1975+t-1)]
    n2oref= RCP8.5n2o[(yr-1975):(yr-1975+t-1)]
  }
  
  
  N2OCONC <- function(dm, t, life= N2Olifetime){
    return(dm*EmissionsfactorN*(exp(1)^(-t/life)))
  }
  N2Olife<- function(conc, conco){
    return(N2Olifetime*((conc/conco)^-0.05))
  }
  if(i== "RCP4.5"){
    N2OlifetimeD<-N2Olife(conc= n2oref[1], conco= AnnualGLobaln2o[(yr-1999):(yr-1999+t-1)])
  }else if(i== "RCP8.5"){
    N2OlifetimeD<-N2Olife(conc= n2oref[1], conco= AnnualGLobaln2o[(yr-1999):(yr-1999+t-1)])
  }else if(i== "historic"){
    N2OlifetimeD<- rep(N2Olife(conc= n2oref[1], conco= AnnualGLobaln2o[(yr-1999)]), 100)
  }
  
  for (h in 1:t) {
    ConcentrationN[h]<- N2OCONC(drymatter, h-1, life = N2OlifetimeD[h])
  }
  N2OPPB <- function(conc, fut){
    return((conc*AvgAtmMol*10^9)/(AtmMass*NitMol)+ fut)
  }
  VolumeN<- N2OPPB(conc= ConcentrationN, fut= ambientconc)
  RFN2O <- function(n2o, n2oo= n2oref, CH= ch4ref){
    D<- 0.12*(sqrt(n2o)-sqrt(n2oo))
    E<- 0.47*log(1+(2.01*10^-5)*(CH^0.75)*(n2o^0.75)+(5.31*10^-15)*(CH)*(CH^1.52)*(n2o^1.52))
    f<- 0.47*log(1+(2.01*10^-5)*(CH^0.75)*(n2oo^0.75)+(5.31*10^-15)*CH*(CH^1.52)*(n2oo^1.52))
    return(D-(E-f))
  }
  RFGlobeN<- RFN2O(n2o= VolumeN)
  RFBorealN<- RFGlobeN*(EarthArea/scalingfactor)
  
  
  if(i == "historic"){
    timeserieshistoricN2O<- ts(RFBorealN, start = yr, frequency = 1)
    for(h in 1:t){
      cumulativeRFN2Ohistoric[h]<-sum(RFBorealN[1:h])/h
    }
    cumulativetsN2Ohistoric<- ts(cumulativeRFN2Ohistoric, start= yr, frequency = 1)
  }else if(i == "RCP4.5"){
    timeseriesRCP4.5N2O<- ts(RFBorealN, start = yr, frequency = 1)
    for(h in 1:t){
      cumulativeRFN2ORCP4.5[h]<-sum(RFBorealN[1:h])/h
    }
    cumulativetsN2ORCP4.5<- ts(cumulativeRFN2ORCP4.5, start= yr, frequency = 1)
  }else if(i == "RCP8.5"){
    timeseriesRCP8.5N2O<- ts(RFBorealN, start = yr, frequency = 1)
    for(h in 1:t){
      cumulativeRFN2ORCP8.5[h]<-sum(RFBorealN[1:h])/h
    }
    cumulativetsN2ORCP8.5<- ts(cumulativeRFN2ORCP8.5, start= yr, frequency = 1)
  }
  
  ##########CO2###############
  
  if(i == "historic"){
    ambientconcco2<- historicco2[(yr-1980):(yr-1980+t-1)]
    IRF<- function(time){
      return(0.2173+0.2240*exp(-time/394.4)+0.2824*exp(-time/36.54)+0.2763*exp(-time/4.304))
    }
    CO2CONC<- function(dm, time){
      return(EmissionsFactorCO2*dm*IRF(time))
    }
    for(k in 1:t){
      ConcentrationCO2[k]<- CO2CONC(dm= drymatter, time= k-1)
    }
  }else if(i == "RCP4.5"){
    ambientconc<- RCP4.5futvol[(yr-1980):(yr-1980+t-1)]
    IRF<- IRF4.5[1:t]
    ConcentrationCO2<- IRF*EmissionsFactorCO2*drymatter
  }else if(i== "RCP8.5"){
    ambientconc<- RCP8.5futvol[(yr-1980):(yr-1980+t-1)]
    IRF<- IRF8.5[1:t]  
    ConcentrationCO2<- IRF*EmissionsFactorCO2*drymatter
  }
  
  
  
  
  CO2PPM<- function(conc){
    return((conc*AvgAtmMol*(10^6))/(AtmMass*CO2Mol))
  }
  VolumeCO2<- CO2PPM(conc= ConcentrationCO2)
  RFGlobeCO2<- 5.35*log((VolumeCO2+ambientconc)/ambientconc) 
  RFBorealCO2<- RFGlobeCO2*(EarthArea/scalingfactor)
  
  if(i == "historic"){
    RFhistoric<- RFBorealCO2
    timeserieshistoricCO2<- ts(RFBorealCO2, start= yr, frequency = 1)
    for(h in 1:t){
      cumulativeRFCO2historic[h]<-sum(RFBorealCO2[1:h])/h
    }
    cumulativetsCO2historic<- ts(cumulativeRFCO2historic, start= yr, frequency = 1)
  }else if(i == "RCP4.5"){
    RF4.5<- RFBorealCO2
    timeseriesRCP4.5CO2<- ts(RFBorealCO2, start= yr, frequency= 1)
    for(h in 1:t){
      cumulativeRFCO2RCP4.5[h]<-sum(RFBorealCO2[1:h])/h
    }
    cumulativetsCO2RCP4.5<- ts(cumulativeRFCO2RCP4.5, start= yr, frequency = 1)
    #######Effectiveness#######
    RFholdCoconstant4.5<- 5.35*log((VolumeCO2+ambientconc[1])/ambientconc[1])*(EarthArea/scalingfactor)
    RFholdCconstant4.5<-5.35*log((VolumeCO2[1]+ambientconc)/ambientconc)*(EarthArea/scalingfactor)
    RFholdCoconstant4.5ts<- ts(RFholdCoconstant4.5, start = yr, frequency = 1)
    RFholdCconstant4.5ts<- ts(RFholdCconstant4.5, start = yr, frequency = 1)
    IRF4.5ts<- ts((IRF*RFBorealCO2[1]), start = yr, frequency = 1)
  }else if(i == "RCP8.5"){
    RF8.5<- RFBorealCO2
    timeseriesRCP8.5CO2<- ts(RFBorealCO2, start= yr, frequency= 1)
    for(h in 1:t){
      cumulativeRFCO2RCP8.5[h]<-sum(RFBorealCO2[1:h])/h
    }
    cumulativetsCO2RCP8.5<- ts(cumulativeRFCO2RCP8.5, start= yr, frequency = 1)
    #####Effectiveness######
    RFholdCoconstant8.5<- 5.35*log((VolumeCO2+ambientconc[1])/ambientconc[1])*(EarthArea/scalingfactor)
    RFholdCconstant8.5<-5.35*log((VolumeCO2[1]+ambientconc)/ambientconc)*(EarthArea/scalingfactor)
    RFholdCoconstant8.5ts<- ts(RFholdCoconstant8.5, start = yr, frequency = 1)
    RFholdCconstant8.5ts<- ts(RFholdCconstant8.5, start = yr, frequency = 1)
    IRF8.5ts<- ts((IRF*RFBorealCO2[1]), start = yr, frequency = 1)
  }
  
  ##############OZONE#################
  
  COPPB<- function(dm){
    return((EmissionsfactorCO*dm*AvgAtmMol*(10^9))/(AtmMass*COMol))   
  }
  
  #concentration for first 15 days
  
  volume_5<- function(time){
    return((0.0206*time+0.0349)*COPPB(drymatter))
  }
  
  volume_5.t<- function(time){
    return((VolumeO3[5])*exp(-time/Ozonelife))
  }
  for(j in 1:5){
    VolumeO3[j]<- volume_5(j-1)
  }
  for(j in 1:(timedays-5)){
    VolumeO3[j+5]<- volume_5.t(j)
  }
  O3MolperMeter<- ((VolumeO3*AtmMolQuant)/((10^9)*EarthArea))
  DUs<- O3MolperMeter*2241
  RFGlobeO3<- DUs*RFperDU
  RFBorealO3day<- (RFGlobeO3*EarthArea)/scalingfactor
  
  for(l in 1:(t)){
    RFBorealO3[l]<- mean(RFBorealO3day[(365*(l-1)):(365*l)])
  }
  
  RFBorealO3yr<- RFBorealO3 
  timeseriesozone<- ts(RFBorealO3yr, start = yr, frequency= 1)
  for(y in 1:t){
    cumulativeRFOZONE[y]<-sum(RFBorealO3yr[1:y])/y
  }
  cumulativetsOZONE<- ts(cumulativeRFOZONE, start= yr, frequency = 1)
  
  
  
  ###########PLOTS#####################
  
  if(i == "historic"){

     year<- (seq(1:t)+yr-1)
    RFannual<- c(RFBorealO3, RFBorealCO2, RFBorealN, RFBorealC)
    name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t))
    if(sensitivity== "yes"){
      RFannual<- c(RFBorealO3, RFBorealCO2, RFBorealN, RFBorealC, annualRFCOhistoric, annualRFVOChistoric, annualRFNOxhistoric)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NMVOC", t), rep("NOx", t))
    }
    annualdata<- data.frame(RFannual, year, name)
    annualdata$name <- factor(annualdata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx","Ozone"))
    print(ggplot(annualdata, aes(x= year, y= RFannual, fill= name)) + geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("Historic Annual RF per GHG"))
    
    RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2historic, cumulativeRFN2Ohistoric, cumulativeRFCH4historic)
    if(sensitivity == "yes"){
      RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2historic, cumulativeRFN2Ohistoric, cumulativeRFCH4historic, cumulativeRFCOmeanhistoric, cumulativeRFNOxmeanhistoric, cumulativeRFVOCmeanhistoric)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NOx", t), rep("NMVOC", t))
    }
    cumulativedata<- data.frame(RFcumulative, year, name)
    cumulativedata$name <- factor(cumulativedata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx", "Ozone"))
    print(ggplot(cumulativedata, aes(x= year, y= RFcumulative, fill= name)) + geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("Historic Cumulative Mean RF per GHG"))

    RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2historic, cumulativeRFN2Ohistoric, cumulativeRFCH4historic)
    if(sensitivity == "yes"){
      RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2historic, cumulativeRFN2Ohistoric, cumulativeRFCH4historic, cumulativeRFCOmeanhistoric, cumulativeRFNOxmeanhistoric, cumulativeRFVOCmeanhistoric, cumulativeRFDirectmeanhistoric, cumulativeRFIndirectmeanhistoric)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NOx", t), rep("NMVOC", t), rep("Direct", t), rep("Indirect", t))
    }
    cumulativedata<- data.frame(RFcumulative, year, name)
    cumulativedata$name <- factor(cumulativedata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx","Direct", "Indirect",  "Ozone"))
    print(ggplot(cumulativedata, aes(x= year, y= RFcumulative, fill= name)) + geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("Historic Cumulative Mean RF per GHG"))
    
    
    meanhistoric<- matrix(c(mean(timeserieshistoricCO2), mean(timeserieshistoricCH4), mean(timeserieshistoricN2O), mean(RFBorealO3day)), ncol= 4)
    colnames(meanhistoric)<- c("CO2", "CH4", "N2O", "O3") 
    rownames(meanhistoric)<- "mean RF historic"
    if(sensitivity== "yes"){
      meanhistoric<- matrix(c(mean(timeserieshistoricCO2), mean(timeserieshistoricCH4), mean(timeserieshistoricN2O), mean(RFBorealO3day), mean(annualRFNOxtimeserieshistoric), mean(annualRFVOCtimeserieshistoric), mean(annualRFCOtimeserieshistoric), mean(annualRFNOxtimeserieshistoric+annualRFVOCtimeserieshistoric+annualRFCOtimeserieshistoric), mean(annualRFDirecthistoric), mean(annualRFIndirecthistoric)), ncol = 10)
      colnames(meanhistoric)<- c("CO2", "CH4", "N2O", "O3", "NOx", "NMVOC", "CO", "Precursor Total", "Aerosol Direct", "Aerosol Indirect")
      rownames(meanhistoric)<- "Mean RF historic"
    }
    print(meanhistoric)
    
    totalannualhistoric<- RFBorealO3 + RFBorealCO2+RFBorealN+RFBorealC
    if(sensitivity== "yes"){
      totalannualhistoric<- RFBorealO3+RFBorealCO2+RFBorealN+RFBorealC+annualRFCOtimeserieshistoric+annualRFNOxtimeserieshistoric+annualRFVOCtimeserieshistoric
    }
    totalannualhistoricts<- ts(totalannualhistoric, start= 2010, frequency = 1)
    totalcumulativehistoric<- cumulativeRFOZONE+cumulativeRFCO2historic+cumulativeRFN2Ohistoric+cumulativeRFCH4historic
    if(sensitivity== "yes"){
      totalcumulativehistoric<- cumulativeRFOZONE+cumulativeRFCO2historic+cumulativeRFN2Ohistoric+cumulativeRFCH4historic+cumulativeRFCOmeanhistoric+cumulativeRFNOxmeanhistoric+cumulativeRFVOCmeanhistoric
    }
    totalcumulativehistoricts<- ts(totalcumulativehistoric, start= 2010, frequency = 1)
    
    if(sensitivity== "yes"){
    cumulativemean<- c(cumulativeRFCO2historic[80], cumulativeRFCO2historic[20], cumulativeRFCH4historic[80], cumulativeRFCH4historic[20], cumulativeRFN2Ohistoric[80], cumulativeRFN2Ohistoric[20], cumulativeRFOZONE[80], cumulativeRFOZONE[20], cumulativeRFVOCmeanhistoric[80], cumulativeRFVOCmeanhistoric[20], cumulativeRFNOxmeanhistoric[80], cumulativeRFNOxmeanhistoric[20], cumulativeRFCOmeanhistoric[80], cumulativeRFCOmeanhistoric[20], cumulativeRFDirectmeanhistoric[80], cumulativeRFDirectmeanhistoric[20], cumulativeRFIndirectmeanhistoric[80], cumulativeRFIndirectmeanhistoric[20],
    + sum(cumulativeRFCO2historic[80], cumulativeRFCH4historic[80], cumulativeRFN2Ohistoric[80], cumulativeRFOZONE[80], cumulativeRFVOCmeanhistoric[80], cumulativeRFNOxmeanhistoric[80], cumulativeRFCOmeanhistoric[80], cumulativeRFDirectmeanhistoric[80], cumulativeRFIndirectmeanhistoric[80]), 
    + sum(cumulativeRFCO2historic[20], cumulativeRFCH4historic[20], cumulativeRFN2Ohistoric[20], cumulativeRFOZONE[20], cumulativeRFVOCmeanhistoric[20], cumulativeRFNOxmeanhistoric[20], cumulativeRFCOmeanhistoric[20], cumulativeRFDirectmeanhistoric[20], cumulativeRFIndirectmeanhistoric[20]),
    + sum(cumulativeRFCO2historic[80], cumulativeRFCH4historic[80], cumulativeRFN2Ohistoric[80], cumulativeRFOZONE[80]),
    + sum(cumulativeRFCO2historic[20], cumulativeRFCH4historic[20], cumulativeRFN2Ohistoric[20], cumulativeRFOZONE[20]))
    Species<- c(rep("CO2", 2), rep("CH4", 2), rep("N2O", 2), rep("Ozone", 2), rep("NMVOC", 2), rep("NOx", 2), rep("CO", 2), rep("Direct", 2), rep("Indirect", 2), rep("Total w/ Sensitivity", 2), rep("Total w/o Sensitivity", 2))
    Timeafter<- rep(c("80 Years", "20 Years"), 11)
    barhorizontal<- data.frame(cumulativemean, Species, Timeafter)
    print(ggplot(barhorizontal, aes(x= Species, y= cumulativemean, fill= Timeafter))+ geom_bar(position = "dodge", stat = "identity")+labs(fill= "Time After Burn", y= "RF (w/m^2)", title= "Cumulative Mean RF Historic")+coord_flip())
    }
    
  }else if(i == "RCP4.5"){
    
    year<- (seq(1:t)+yr-1)
    RFannual<- c(RFBorealO3, RFBorealCO2, RFBorealN, RFBorealC)
    name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t))
    if(sensitivity== "yes"){
      RFannual<- c(RFBorealO3, RFBorealCO2, RFBorealN, RFBorealC, annualRFCORCP4.5, annualRFVOCRCP4.5, annualRFNOxRCP4.5)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NMVOC", t), rep("NOx", t))
    }
    annualdata<- data.frame(RFannual, year, name)
    annualdata$name <- factor(annualdata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx","Ozone"))
    print(ggplot(annualdata, aes(x= year, y= RFannual, fill= name)) +geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("RCP4.5 Annual RF per GHG"))
    
    RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP4.5, cumulativeRFN2ORCP4.5, cumulativeRFCH4RCP4.5)
    if(sensitivity == "yes"){
      RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP4.5, cumulativeRFN2ORCP4.5, cumulativeRFCH4RCP4.5, cumulativeRFCOmeanRCP4.5, cumulativeRFNOxmeanRCP4.5, cumulativeRFVOCmeanRCP4.5)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NOx", t), rep("NMVOC", t))
    }
    cumulativedata<- data.frame(RFcumulative, year, name)
    cumulativedata$name <- factor(cumulativedata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx","Ozone"))
    print(ggplot(cumulativedata, aes(x= year, y= RFcumulative, fill= name)) + geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("RCP4.5 Cumulative Mean RF per GHG"))
    
    RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP4.5, cumulativeRFN2ORCP4.5, cumulativeRFCH4RCP4.5)
    if(sensitivity == "yes"){
      RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP4.5, cumulativeRFN2ORCP4.5, cumulativeRFCH4RCP4.5, cumulativeRFCOmeanRCP4.5, cumulativeRFNOxmeanRCP4.5, cumulativeRFVOCmeanRCP4.5, cumulativeRFDirectmeanRCP4.5, cumulativeRFIndirectmeanRCP4.5)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NOx", t), rep("NMVOC", t), rep("Direct", t), rep("Indirect", t))
    }
    cumulativedata<- data.frame(RFcumulative, year, name)
    cumulativedata$name <- factor(cumulativedata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx", "Direct", "Indirect", "Ozone"))
    print(ggplot(cumulativedata, aes(x= year, y= RFcumulative, fill= name)) + geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("RCP4.5 Cumulative Mean RF per GHG"))
    
    
    meanRCP4.5<- matrix(c(mean(timeseriesRCP4.5CO2), mean(timeseriesRCP4.5CH4), mean(timeseriesRCP4.5N2O), mean(RFBorealO3day)), ncol= 4)
    colnames(meanRCP4.5)<- c("CO2", "CH4", "N2O", "O3")
    rownames(meanRCP4.5)<- "mean RF 4.5"
    if(sensitivity== "yes"){
      meanRCP4.5<- matrix(c(mean(timeseriesRCP4.5CO2), mean(timeseriesRCP4.5CH4), mean(timeseriesRCP4.5N2O), mean(RFBorealO3day), mean(annualRFNOxtimeseriesRCP4.5), mean(annualRFVOCtimeseriesRCP4.5), mean(annualRFCOtimeseriesRCP4.5), mean(annualRFNOxtimeseriesRCP4.5+annualRFVOCtimeseriesRCP4.5+annualRFCOtimeseriesRCP4.5), mean(annualRFDirectRCP4.5), mean(annualRFIndirectRCP4.5)), ncol= 10)
      colnames(meanRCP4.5)<- c("CO2", "CH4", "N2O", "O3", "NOx", "NMVOC", "CO", "Precursor Total", "Aerosol Direct", "Aerosol Indirect")
      rownames(meanRCP4.5)<- "Mean RF 4.5"
    }
    print(meanRCP4.5)
    
    totalannualRCP4.5<- RFBorealO3+RFBorealCO2+RFBorealN+RFBorealC
    if(sensitivity== "yes"){
      totalannualRCP4.5<- RFBorealO3+RFBorealCO2+RFBorealN+RFBorealC+annualRFCOtimeseriesRCP4.5+annualRFNOxtimeseriesRCP4.5+annualRFVOCtimeseriesRCP4.5
    }
    totalannualRCP4.5ts<- ts(totalannualRCP4.5, start= 2010, frequency = 1)
    totalcumulativeRCP4.5<- cumulativeRFOZONE+cumulativeRFCO2RCP4.5+cumulativeRFN2ORCP4.5+cumulativeRFCH4RCP4.5
    if(sensitivity== "yes"){
      totalcumulativeRCP4.5<- cumulativeRFOZONE+cumulativeRFCO2RCP4.5+cumulativeRFN2ORCP4.5+cumulativeRFCH4RCP4.5+cumulativeRFCOmeanRCP4.5+cumulativeRFNOxmeanRCP4.5+cumulativeRFVOCmeanRCP4.5
    }
    totalcumulativeRCP4.5ts<- ts(totalcumulativeRCP4.5, start= 2010, frequency = 1)
    
    
    label<- c(rep("Constant Emission", t), rep("Constant Atmospheric Concentration",t), rep("Actual Observed RF", t))
    sens4.5<-  c(RFholdCconstant4.5, RFholdCoconstant4.5, RF4.5)
    RCP4.5sensitivity<- data.frame(year, sens4.5 , label)
    print(ggplot(RCP4.5sensitivity, aes(x= year, y= sens4.5, group= label)) + geom_line(aes(linetype= label, col =label)) + scale_linetype_manual(values=c("twodash", "dotted", "longdash"))+ggtitle("RCP4.5 CO2 Effectiveness") + ylim(1.5, 5)+ labs(y = "RF (w/m^2)"))
    
    if(sensitivity== "yes"){
    cumulativemeanRCP4.5<- c(cumulativeRFCO2RCP4.5[80], cumulativeRFCO2RCP4.5[20], cumulativeRFCH4RCP4.5[80], cumulativeRFCH4RCP4.5[20], cumulativeRFN2ORCP4.5[80], cumulativeRFN2ORCP4.5[20], cumulativeRFOZONE[80], cumulativeRFOZONE[20], cumulativeRFVOCmeanRCP4.5[80], cumulativeRFVOCmeanRCP4.5[20], cumulativeRFNOxmeanRCP4.5[80], cumulativeRFNOxmeanRCP4.5[20], cumulativeRFCOmeanRCP4.5[80], cumulativeRFCOmeanRCP4.5[20], cumulativeRFDirectmeanRCP4.5[80], cumulativeRFDirectmeanRCP4.5[20], cumulativeRFIndirectmeanRCP4.5[80], cumulativeRFIndirectmeanRCP4.5[20],
    + sum(cumulativeRFCO2RCP4.5[80], cumulativeRFCH4RCP4.5[80], cumulativeRFN2ORCP4.5[80], cumulativeRFOZONE[80], cumulativeRFVOCmeanRCP4.5[80], cumulativeRFNOxmeanRCP4.5[80], cumulativeRFCOmeanRCP4.5[80], cumulativeRFDirectmeanRCP4.5[80], cumulativeRFIndirectmeanRCP4.5[80]), 
    + sum(cumulativeRFCO2RCP4.5[20], cumulativeRFCH4RCP4.5[20], cumulativeRFN2ORCP4.5[20], cumulativeRFOZONE[20], cumulativeRFVOCmeanRCP4.5[20], cumulativeRFNOxmeanRCP4.5[20], cumulativeRFCOmeanRCP4.5[20], cumulativeRFDirectmeanRCP4.5[20], cumulativeRFIndirectmeanRCP4.5[20]),
    + sum(cumulativeRFCO2RCP4.5[80], cumulativeRFCH4RCP4.5[80], cumulativeRFN2ORCP4.5[80], cumulativeRFOZONE[80]),
    + sum(cumulativeRFCO2RCP4.5[20], cumulativeRFCH4RCP4.5[20], cumulativeRFN2ORCP4.5[20], cumulativeRFOZONE[20]))
    Species<- c(rep("CO2", 2), rep("CH4", 2), rep("N2O", 2), rep("Ozone", 2), rep("NMVOC", 2), rep("NOx", 2), rep("CO", 2), rep("Direct", 2), rep("Indirect", 2), rep("Total w/ Sensitivity", 2), rep("Total w/o Sensitivity", 2))
    Timeafter<- rep(c("80 Years", "20 Years"), 11)
    barhorizontalRCP4.5<- data.frame(cumulativemeanRCP4.5, Species, Timeafter)
    print(ggplot(barhorizontalRCP4.5, aes(x= Species, y= cumulativemeanRCP4.5, fill= Timeafter))+ geom_bar(position = "dodge", stat = "identity")+labs(fill= "Time After Burn", y= "RF (w/m^2)", title= "Cumulative Mean RF RCP4.5")+coord_flip())
    }
    
  }else if(i== "RCP8.5"){
    year<- (seq(1:t)+yr-1)
    RFannual<- c(RFBorealO3, RFBorealCO2, RFBorealN, RFBorealC)
    name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t))
    if(sensitivity== "yes"){
      RFannual<- c(RFBorealO3, RFBorealCO2, RFBorealN, RFBorealC, annualRFCORCP8.5, annualRFVOCRCP8.5, annualRFNOxRCP8.5)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NMVOC", t), rep("NOx", t))
    }
    annualdata<- data.frame(RFannual, year, name)
    annualdata$name <- factor(annualdata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx","Ozone"))
    print(ggplot(annualdata, aes(x= year, y= RFannual, fill= name)) +geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("RCP8.5 Annual RF per GHG"))
    
    RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP8.5, cumulativeRFN2ORCP8.5, cumulativeRFCH4RCP8.5)
    if(sensitivity == "yes"){
      RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP8.5, cumulativeRFN2ORCP8.5, cumulativeRFCH4RCP8.5, cumulativeRFCOmeanRCP8.5, cumulativeRFNOxmeanRCP8.5, cumulativeRFVOCmeanRCP8.5)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NOx", t), rep("NMVOC", t))
    }
    cumulativedata<- data.frame(RFcumulative, year, name)
    cumulativedata$name <- factor(cumulativedata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx","Ozone"))
    print(ggplot(cumulativedata, aes(x= year, y= RFcumulative, fill= name)) + geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("RCP8.5 Cumulative Mean RF per GHG"))
    
    RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP8.5, cumulativeRFN2ORCP8.5, cumulativeRFCH4RCP8.5)
    if(sensitivity == "yes"){
      RFcumulative<- c(cumulativeRFOZONE, cumulativeRFCO2RCP8.5, cumulativeRFN2ORCP8.5, cumulativeRFCH4RCP8.5, cumulativeRFCOmeanRCP8.5, cumulativeRFNOxmeanRCP8.5, cumulativeRFVOCmeanRCP8.5, cumulativeRFDirectmeanRCP8.5, cumulativeRFIndirectmeanRCP8.5)
      name<- c(rep("Ozone", t), rep("Carbon Dioxide", t), rep("Nitrous Oxide", t), rep("Methane", t), rep("Carbon Monoxide", t), rep("NOx", t), rep("NMVOC", t), rep("Direct", t), rep("Indirect", t))
    }
    cumulativedata<- data.frame(RFcumulative, year, name)
    cumulativedata$name <- factor(cumulativedata$name, levels = c("Carbon Dioxide", "Methane", "Nitrous Oxide", "NMVOC", "Carbon Monoxide","NOx","Direct", "Indirect", "Ozone"))
    print(ggplot(cumulativedata, aes(x= year, y= RFcumulative, fill= name)) + geom_area(position= position_stack(reverse= T)) + scale_fill_brewer(palette = "Spectral") + ggtitle("RCP8.5 Cumulative Mean RF per GHG"))
    
   
    
    meanRCP8.5<- matrix(c(mean(timeseriesRCP8.5CO2), mean(timeseriesRCP8.5CH4), mean(timeseriesRCP8.5N2O), mean(RFBorealO3day)), ncol= 4)
    colnames(meanRCP8.5)<- c("CO2", "CH4", "N2O", "O3")
    rownames(meanRCP8.5)<- "mean RF 8.5"
    if(sensitivity== "yes"){
      meanRCP8.5<- matrix(c(mean(timeseriesRCP8.5CO2), mean(timeseriesRCP8.5CH4), mean(timeseriesRCP8.5N2O), mean(RFBorealO3day), mean(annualRFNOxtimeseriesRCP8.5), mean(annualRFVOCtimeseriesRCP8.5), mean(annualRFCOtimeseriesRCP8.5), mean(annualRFNOxtimeseriesRCP8.5+annualRFVOCtimeseriesRCP8.5+annualRFCOtimeseriesRCP8.5), mean(annualRFDirectRCP8.5), mean(annualRFIndirectRCP8.5)), ncol= 10)
      colnames(meanRCP8.5)<- c("CO2", "CH4", "N2O", "O3", "NOx", "NMVOC", "CO", "Precursor Total", "Aerosol Direct", "Aerosol Indirect")
      rownames(meanRCP8.5)<- "Mean RF 8.5"
    }
    print(meanRCP8.5)
    
    totalannualRCP8.5<- RFBorealO3+RFBorealCO2+RFBorealN+RFBorealC
    if(sensitivity== "yes"){
      totalannualRCP8.5<- RFBorealO3+RFBorealCO2+RFBorealN+RFBorealC+annualRFCOtimeseriesRCP8.5+annualRFNOxtimeseriesRCP8.5+annualRFVOCtimeseriesRCP8.5
    }
    totalannualRCP8.5ts<- ts(totalannualRCP8.5, start= 2010, frequency = 1)
    totalcumulativeRCP8.5<- cumulativeRFOZONE+cumulativeRFCO2RCP8.5+cumulativeRFN2ORCP8.5+cumulativeRFCH4RCP8.5
    if(sensitivity== "yes"){
      totalcumulativeRCP8.5<- cumulativeRFOZONE+cumulativeRFCO2RCP8.5+cumulativeRFN2ORCP8.5+cumulativeRFCH4RCP8.5+cumulativeRFCOmeanRCP8.5+cumulativeRFNOxmeanRCP8.5+cumulativeRFVOCmeanRCP8.5
    }
    totalcumulativeRCP8.5ts<- ts(totalcumulativeRCP8.5, start= 2010, frequency = 1)
    
    
    label<- c(rep("Constant Emission", t), rep("Constant Atmospheric Concentration",t), rep("Actual Observed RF", t))
    sens8.5<-  c(RFholdCconstant8.5, RFholdCoconstant8.5, RF8.5)
    RCP8.5sensitivity<- data.frame(year, sens8.5 , label)
    print(ggplot(RCP8.5sensitivity, aes(x= year, y= sens8.5, group= label)) + geom_line(aes(linetype= label, col =label)) + scale_linetype_manual(values=c("twodash", "dotted", "longdash"))+ggtitle("RCP8.5 CO2 Effectiveness") + ylim(1, 5) + labs(y = "RF (w/m^2)"))
    
    if(sensitivity== "yes"){
    cumulativemeanRCP8.5<- c(cumulativeRFCO2RCP8.5[80], cumulativeRFCO2RCP8.5[20], cumulativeRFCH4RCP8.5[80], cumulativeRFCH4RCP8.5[20], cumulativeRFN2ORCP8.5[80], cumulativeRFN2ORCP8.5[20], cumulativeRFOZONE[80], cumulativeRFOZONE[20], cumulativeRFVOCmeanRCP8.5[80], cumulativeRFVOCmeanRCP8.5[20], cumulativeRFNOxmeanRCP8.5[80], cumulativeRFNOxmeanRCP8.5[20], cumulativeRFCOmeanRCP8.5[80], cumulativeRFCOmeanRCP8.5[20], cumulativeRFDirectmeanRCP8.5[80], cumulativeRFDirectmeanRCP8.5[20], cumulativeRFIndirectmeanRCP8.5[80], cumulativeRFIndirectmeanRCP8.5[20], 
    + sum(cumulativeRFCO2RCP8.5[80], cumulativeRFCH4RCP8.5[80], cumulativeRFN2ORCP8.5[80], cumulativeRFOZONE[80], cumulativeRFVOCmeanRCP8.5[80], cumulativeRFNOxmeanRCP8.5[80], cumulativeRFCOmeanRCP8.5[80], cumulativeRFDirectmeanRCP8.5[80], cumulativeRFIndirectmeanRCP8.5[80]), 
    + sum(cumulativeRFCO2RCP8.5[20], cumulativeRFCH4RCP8.5[20], cumulativeRFN2ORCP8.5[20], cumulativeRFOZONE[20], cumulativeRFVOCmeanRCP8.5[20], cumulativeRFNOxmeanRCP8.5[20], cumulativeRFCOmeanRCP8.5[20], cumulativeRFDirectmeanRCP8.5[20], cumulativeRFIndirectmeanRCP8.5[20]),
    + sum(cumulativeRFCO2RCP8.5[80], cumulativeRFCH4RCP8.5[80], cumulativeRFN2ORCP8.5[80], cumulativeRFOZONE[80]),
    + sum(cumulativeRFCO2RCP8.5[20], cumulativeRFCH4RCP8.5[20], cumulativeRFN2ORCP8.5[20], cumulativeRFOZONE[20]))
    Species<- c(rep("CO2", 2), rep("CH4", 2), rep("N2O", 2), rep("Ozone", 2), rep("NMVOC", 2), rep("NOx", 2), rep("CO", 2), rep("Direct", 2), rep("Indirect", 2), rep("Total w/ Sensitivity", 2), rep("Total w/o Sensitivity", 2))
    Timeafter<- rep(c("80 Years", "20 Years"), 11)
    barhorizontalRCP8.5<- data.frame(cumulativemeanRCP8.5, Species, Timeafter)
    print(ggplot(barhorizontalRCP8.5, aes(x= Species, y= cumulativemeanRCP8.5, fill= Timeafter))+ geom_bar(position = "dodge", stat = "identity")+labs(fill= "Time After Burn", y= "RF (w/m^2)", title= "Cumulative Mean RF RCP8.5")+coord_flip())
    }
  }
}


Scenario<- c(rep("Historic", t), rep("RCP4.5", t), rep("RCP8.5", t))
total.annual<- c(totalannualhistoric, totalannualRCP4.5, totalannualRCP8.5)
total<- data.frame(year, Scenario, total.annual)
if(sensitivity== "yes"){
  print(ggplot(total, aes(x= year, y= total.annual, group= Scenario)) + geom_line(aes(linetype= Scenario, col =Scenario)) + scale_linetype_manual(values=c("twodash", "dotted", "longdash"))+ggtitle("Total Annual with Sensitivity") + ylim(1, 22) + labs(y = "RF (w/m^2)"))
}else{
  print(ggplot(total, aes(x= year, y= total.annual, group= Scenario)) + geom_line(aes(linetype= Scenario, col =Scenario)) + scale_linetype_manual(values=c("twodash", "dotted", "longdash"))+ggtitle("Total Annual Mean") + ylim(1, 20.5) + labs(y = "RF (w/m^2)"))
}



Scenario<- c(rep("Historic", t), rep("RCP4.5", t), rep("RCP8.5", t))
total.cumu<- c(totalcumulativehistoric, totalcumulativeRCP4.5, totalcumulativeRCP8.5)
total<- data.frame(year, Scenario, total.cumu)
if(sensitivity== "yes"){
  print(ggplot(total, aes(x= year, y= total.cumu, group= Scenario)) + geom_line(aes(linetype= Scenario, col =Scenario)) + scale_linetype_manual(values=c("twodash", "dotted", "longdash"))+ggtitle("Total Cumulative Mean with Sensitivity") + ylim(1, 23) + labs(y = "RF (w/m^2)"))
}else{
  print(ggplot(total, aes(x= year, y= total.cumu, group= Scenario)) + geom_line(aes(linetype= Scenario, col =Scenario)) + scale_linetype_manual(values=c("twodash", "dotted", "longdash"))+ggtitle("Total Cumulative Mean") + ylim(1, 20.5) + labs(y = "RF (w/m^2)"))
}

if(sensitivity== "yes"){
  plot.ts(totalannualhistoricts, col= "red", main= "Total Annual RF in Scenarios (with sensitivity)")
}else{
  plot.ts(totalannualhistoricts, col= "red", main= "Total Annual RF in Scenarios")
}
lines(totalannualRCP4.5ts, col= "blue")
lines(totalannualRCP8.5ts, col= "green")








