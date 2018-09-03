#This script load the time series and prepare the dataset to calculate 
#the multiscale time depend entropy
##########################################################################
#****************Starting script without using the DLL file****************
##Before run the followings instructions, first you must run the 
##multiscale.R script
##########################################################################
#Loading libraries
library("readxl")
library("zoo")
library("pracma")
library(tictoc)
#Configuring the path
path="C:/codes"
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
 
#****************Starting script using the DLL file****************
tic()
info=Sys.info()
print(info[1])
if (info[1]=="Windows"){dyn.load("cross-sampen.dll")}
if (info[1]=="Linux") {dyn.load("cross-sampen.o")}

sn<-(series[,(3):(4)]) #It extracts the time series to compute multiscale time depend entropy 
series_<-zoo(sn) # It creates a time-serie with format type zoo
filename=paste("Multiscale_",method)
multiscale_time_depend_entropy(series_,SLIDE,method,maxtau,filename,m)

if (info[1]=="Windows"){dyn.unload("cross-sampen.dll")}
if (info[1]=="Linux") {dyn.unload("cross-sampen.o")}
toc()