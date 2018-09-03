#Notice that for computing the multiscale time depend entropy using SampEn you can 
#select a time-serie (only one column) and to compute the multiscale time depend 
#entropy using Cross-SampEn you can select two time series (two columns)
#Inputs:
#SLIDE is a vector with the sliding windows size
#maxtau is a number with the maximum value of tau
#method is a string that could be "SampEn" or "Cross-SampEn"
#filename is a name for to save the result in a RData file
#m is a number for the template length m (vector size for Cross-SampEn) Default 2

multiscale_time_depend_entropy <- function(serie,SLIDE,method,maxtau,filename,m)
{

  if (missing(serie)) {print("Missing argument serie in the function")}
  if (missing(SLIDE)) {print("Missing argument SLIDE in the function")}
  if (missing(method)) {print("Setting method as SampEn"); method='SampEn'}
  if (missing(maxtau)) {print("Setting maxtau as 10"); maxtau=10}
  if (missing(filename)) {print("Setting filename as multiscale_TD_SampEn"); filename='multiscale_TD_SampEn'}
  if (missing(m)) {print("Setting m as 2"); m=2}  
  
series<-list()
entropies<-list()

for(tau in 1:maxtau) 
  { 
      #to apply rollmean for obtaining the series with differents tau  
      series[[tau]]<-as.matrix(rollmean(serie,tau, na.rm = TRUE, fill = NA,align = "right"))
      #to remove the NA data
      serie_temp<-series[[tau]]
      if (method=="SampEn") 
      {
        maxlim=length(serie_temp)
        serie_temp<-serie_temp[(tau:maxlim)]
        maxlim=length(serie_temp)
      }
      if (method=="Cross-SampEn") 
      {
        maxlim=dim(serie_temp)[1]
        serie_temp<-serie_temp[(tau:maxlim),]
        maxlim=dim(serie_temp)[1]
      }
      C <- list()

      for (j in 1:length(SLIDE))
      {
        
        x=0 #to clean the vector x  
        
        for (i in 1:(maxlim-SLIDE[j]+1))
          {
            if (method=="SampEn") x[i]<-sample_entropy(serie_temp[i:(SLIDE[j]+i-1)])
            if (method=="Cross-SampEn")
            {
              S1=as.matrix(serie_temp[i:(SLIDE[j]+i-1),1])
		  S2=as.matrix(serie_temp[i:(SLIDE[j]+i-1),2])
		  N= dim(S1)[1]
		  output<-.C("cross_sampen", x = as.double(S1), y = as.double(S2),
 			n=as.integer(N), m = as.integer(m), r = as.double(r), At =as.integer(0),
		 	Bt =as.integer(0), E = as.double(0))
		  x[i] <- output$E  

            }
          }
      
        C[[j]] <-x
        
      }
  entropies[[tau]]<-C
  }

filename<-paste(filename,'.RData',sep="")
save(entropies, file = filename)


return(entropies)

}

##########################################################################
#Example of main_multiscale.R (multiscale_time_depend_entropy function) 
##########################################################################
#This script load the time series and prepare the dataset to calculate 
#the multiscale time depend entropy
##########################################################################
#Loading libraries
library("zoo")
library("pracma")
library("tictoc")
#Configuring the path
path="/home/entropy-master"
setwd(path)
print(paste("Current path > ",path,sep=""))
#Set on the parameters
SLIDE<-c(100,150,200) #Sliding windows sizes
maxtau=4 #Maximum number of Tau
m=2 #The template length m (vector size for Cross-SampEn) Default 2
method="Cross-SampEn" 
r=0.2
#Loading data
file="example_dataset.txt" 
dataset<-read.table(file, header = TRUE, sep = "\t", dec = ".")
dataset1<-dataset[-1,] #To remove the first row (for NA data in the first row)
series<-as.data.frame(dataset1, rownames.force = NA) #To change the format of data.frame to matrix
 
#****************Starting the calculations****************
tic()
info=Sys.info()
print(info[1])
if (info[1]=="Windows"){dyn.load("cross-sampen.dll")}
if (info[1]=="Linux") {dyn.load("cross-sampen.so")}

sn<-(series[,(3):(4)]) #It extracts the time series to compute multiscale time depend entropy 
series_<-zoo(sn) # It creates a time-serie with format type zoo
filename=paste("Multiscale_",method)
multiscale_time_depend_entropy(series_,SLIDE,method,maxtau,filename,m)

if (info[1]=="Windows"){dyn.unload("cross-sampen.dll")}
if (info[1]=="Linux") {dyn.unload("cross-sampen.so")}
toc()
