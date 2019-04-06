#Notice that for computing the multiscale time-dependent entropy using SampEn, you must use a single time-series (only one column), and to calculate the multiscale time-dependent entropy using Cross-SampEn you must use a time series with two columns.
#Inputs:
#SLIDE is a vector with the sliding windows sizes
#maxtau is a number with the maximum value of tau
#method is a string that could be "SampEn" or "Cross-SampEn."
#filename is a name for saving the result in an RData file
#m is a number for the length of vector size, default 2

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

set.seed(1234)

for(tau in 1:maxtau) 
  { 
    #applying rollmean function for obtaining series with several taus values   
      series[[tau]]<-as.matrix(rollmean(serie,tau, na.rm = TRUE, fill = NA,align = "right"))
      #removing not available data (NA)
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
        
        x=0 #restarting vector x 
        
        for (i in 1:(maxlim-SLIDE[j]+1))
          {
            #if (method=="SampEn") x[i]<-sample_entropy(serie_temp[i:(SLIDE[j]+i-1)]) #If you want to compare the performance uncomment this line and comment "executing SampEn" section

			#-----executing SampEn
            if (method=="SampEn") 
			{
   			S1=as.matrix(serie_temp[i:(SLIDE[j]+i-1)])
			N= dim(S1)[1]
			output<-.C("sampen", x = as.double(S1), n=as.integer(N), m = as.integer(m), r = as.double(r), 
			At =as.integer(0), Bt =as.integer(0), E = as.double(0))
			#x[i]<-sample_entropy(serie_temp[i:(SLIDE[j]+i-1)],edim=m)
			#x[i]<-approx_entropy(serie_temp[i:(SLIDE[j]+i-1)],edim=m)
			}
			#--------------------

			#-----executing Cross-SampEn
			if (method=="Cross-SampEn")
            {
            S1=as.matrix(serie_temp[i:(SLIDE[j]+i-1),1])
		  	S2=as.matrix(serie_temp[i:(SLIDE[j]+i-1),2])
		  	N= dim(S1)[1]
		  	output<-.C("cross_sampen", x = as.double(S1), y = as.double(S2),
 			n=as.integer(N), m = as.integer(m), r = as.double(r), At =as.integer(0),
		 	Bt =as.integer(0), E = as.double(0))
            }
			#--------------------
			
			x[i] <- output$E 
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
#Example (multiscale_time_depend_entropy function using Cross-SampEn)
##########################################################################
#This script load the time series and prepare the dataset for computing the multiscale time depend entropy
##########################################################################

#Loading libraries
library("zoo")
library("pracma")
library("tictoc")

#Configuring path
if (Sys.info()[1]=="Windows")
{
	dyn.load("cross-sampen.dll")
	dyn.load("sampen.dll")
	#path<-paste("C:/master-entropy/",sep='') #choose the files correct path 
}
if (Sys.info()[1]=="Linux") 
{
	dyn.load("cross-sampen.so")
	dyn.load("sampen.so")
	path<-paste("/home/",Sys.info()[7],"/Dropbox/launchscript/Rcodes/second_version",sep='')  #choose the files correct path 
}

setwd(path)
print(paste("Current path > ",path,sep=""))

#setting parameters
SLIDE<-c(100,150,200) #Sliding windows sizes depends on the time series length (never use a higher value of that length)   
maxtau=2 #maximum number of Tau, default 10
m=2 #length of m, default 2 (vector size)
method="Cross-SampEn" #choose the method SampEn or Cross-SampEn, default SampEn
r=0.2
filename_= "Multiscale_"  #Put a name for the output file
filename=paste(filename_,method,sep='')

#Loading data
file="example_dataset.txt" 
dataset<-read.table(file, header = TRUE, sep = "\t", dec = ".")
dataset1<-dataset[-1,] #removing the first row (NA data), it depends on the dataset
series<-as.data.frame(dataset1, rownames.force = NA) #changing the format of data.frame to matrix
 
#****************Starting the calculations****************
tic()
print(Sys.info()[1])

sn<-(series[,(3):(4)]) #It extracts the time series to compute multiscale time-dependent entropy
series_<-zoo(sn) # It creates a time-serie with format type zoo

multiscale_time_depend_entropy(series_,SLIDE,method,maxtau,filename,m)

#Unloading files
if (Sys.info()[1]=="Windows"){dyn.unload("cross-sampen.dll");dyn.unload("sampen.dll")}
if (Sys.info()[1]=="Linux") {dyn.unload("cross-sampen.so");dyn.unload("sampen.so")}
toc()

