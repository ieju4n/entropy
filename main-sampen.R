##################################################################
#Loading libraries
library("tictoc")
#Configuring path
path<-paste("/home/",Sys.info()[7],"/Dropbox/launchscript/Rcodes/second_version",sep='')  #choose the files correct path 
setwd(path)
print(paste("Current path > ",path,sep=""))
#setting parameters
m=2
r=0.2
#Loading data
file="example_dataset.txt" 
dataset<-read.table(file, header = TRUE, sep = "\t", dec = ".")
dataset1<-dataset[-1,] #To remove the first row (for NA data in the first row)
S1=as.matrix(dataset1$serie3)
N= dim(S1)[1]
#****************Starting the calculations**************** 
tic()
print(Sys.info()[1])

if (Sys.info()[1]=="Windows"){dyn.load("sampen.dll")}
if (Sys.info()[1]=="Linux") {dyn.load("sampen.so")}

output<-.C("sampen", x = as.double(S1), n=as.integer(N), m = as.integer(m), r=as.double(r), At =as.integer(0), Bt =as.integer(0), E = as.double(0))

if (Sys.info()[1]=="Windows"){dyn.unload("sampen.dll")}
if (Sys.info()[1]=="Linux") {dyn.unload("sampen.so")}
A=output$At/((N-m)^2)
print(paste("At > ",output$At," A > ",A))
B=output$Bt/((N-m)^2)
print(paste("Bt > ",output$Bt," B > ",B))
p=output$At/output$Bt
print(paste("Probability (p) > ",p))
print(paste("Entropy (SampEn) > ",output$E))
toc()

##########################################################################
