##################################################################
#Loading libraries
library(tictoc)
#Configuring the path
path="C:/codes"
setwd(path)
print(paste("Current path > ",path,sep=""))
#Set on the parameters
m=2
r=0.2
#Loading data
file="example_dataset.txt" 
dataset<-read.table(file, header = TRUE, sep = "\t", dec = ".")
dataset1<-dataset[-1,] #To remove the first row (for NA data in the first row)
S1=as.matrix(dataset1$serie3)
S2=as.matrix(dataset1$serie4)
N= dim(S1)[1]
#****************Starting script using the DLL file****************
tic()
info=Sys.info()
print(info[1])
if (info[1]=="Windows"){dyn.load("cross-sampen.dll")}
if (info[1]=="Linux") {dyn.load("cross-sampen.o")}
output<-.C("cross_sampen", x = as.double(S1), y = as.double(S2),
 	n=as.integer(N), m = as.integer(m), r = as.double(r), At =as.integer(0),
 	Bt =as.integer(0), E = as.double(0))
dyn.unload("cross-sampen.dll")
A=output$At/((N-m)^2)
print(paste("At > ",output$At," A > ",A))
B=output$Bt/((N-m)^2)
print(paste("Bt > ",output$Bt," B > ",B))
p=output$At/output$Bt
print(paste("Probability (p) > ",p))
print(paste("Entropy > ",output$E))
print("BEST PERFORMANCE")
toc()

##########################################################################
#****************Starting script without using the DLL file****************
##Before run the followings instructions, first you must run the 
##cross-sampe.R script
#Only run the the below instructions if you want to compare the performance
tic()
serie<-cbind(S1,S2)
cross_sampen(serie,m1=m,m2=m,r=r)
toc()