##################################################################
#Loading libraries
library("tictoc")
#Configuring the path
path="/home/entropy-master"
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
#****************Starting the calculations**************** 
tic()
info=Sys.info()
print(info[1])
if (info[1]=="Windows"){dyn.load("cross-sampen.dll")}
if (info[1]=="Linux") {dyn.load("cross-sampen.so")}
output<-.C("cross_sampen", x = as.double(S1), y = as.double(S2),
 	n=as.integer(N), m = as.integer(m), r = as.double(r), At =as.integer(0),
 	Bt =as.integer(0), E = as.double(0))

if (info[1]=="Windows"){dyn.unload("cross-sampen.dll")}
if (info[1]=="Linux") {dyn.unload("cross-sampen.so")}
A=output$At/((N-m)^2)
print(paste("At > ",output$At," A > ",A))
B=output$Bt/((N-m)^2)
print(paste("Bt > ",output$Bt," B > ",B))
p=output$At/output$Bt
print(paste("Probability (p) > ",p))
print(paste("Entropy > ",output$E))
toc()

##########################################################################
##If you want to compare the performance to execute cross-sampen.R 
##script example 
##
