##################################################################
cross_sampen <- function(serie,m1,m2,r,sflag,verbose)
{
  #[A,B,p,mm,E]=cross_sampen(serie,x,y,date,M,r,sflag)
  #Inputs:
  #
  #serie: matrix with time series
  #m1: minimum template length
  #m2: maximum template length 
  #If you want to run the function only for a template length m (vector size), 
    #make m1= m2 = same value
  #r: matching tolerance
  #sflag:    flag to standardize signals(default yes/sflag=1) 
  #
  #Outputs:
  #
  #E: cross sample entropy estimates for m=m1,...,m2
  #A: number of matches for m=m1+1,...,m2+1
  #B: number of matches for m=m1,...,m2 excluding last point
  #
  if (missing(verbose)) {verbose='FALSE'}
  if (missing(serie)) {if (verbose =='TRUE') print("Missing argument serie in the function")}
  if (missing(m1)) {if (verbose =='TRUE') print("Setting m1 as 2 "); m1=2}
  if (missing(m2)) {if (verbose =='TRUE') print("Setting m2 as 2 "); m2=2}
  if (missing(r)) {if (verbose =='TRUE') print("Setting r as 0.2 "); r=0.2}
  if (missing(sflag)) {if (verbose =='TRUE') print("Setting sflag as 1 (True) "); sflag=1}
  
  x=as.numeric(serie[,1])
  y=as.numeric(serie[,2])

  N=length(x);
  
  
  if (length(y)==N)
  {
    if (verbose =='TRUE') print('Starting.');
    if (sflag>0)
    {
      if (verbose =='TRUE') print('Normalize data.')
      sy=sd(y) 
      ymn=mean(y)
      y=y-ymn
      y=y/sy
      sx=sd(x)   
      xmn=mean(x)
      x=x-xmn
      x=x/sx   
    }
    
    A=matrix(0,m2,1);
    B=matrix(0,m2,1);
    p=matrix(0,m2,1);
    E=matrix(0,m2,1);
    mm=matrix(0,m2,1);
    
    for (m in m1:m2)
    {
      At=matrix(0,N-m+1,1);
      Bt=matrix(0,N-m+1,1);
      dist=matrix(0,m,1);
      for (i in 1:(N-m+1))
      {
        for (j in 1:(N-m+1))
        {
          for (k in 0:(m-1))
          {  
            dist[k+1]=abs(x[i+k]-y[j+k])  #dist=max(abs(x[i]-y[j]),abs(x[i+1]-y[j+1]));
          }
          if (max(dist)<=r) {
            Bt[i]=Bt[i]+1; 
          }
          if (i<=(N-m) && j<=(N-m)) if (max(max(dist),abs(x[i+m]-y[j+m]))<=r) At[i]=At[i]+1 
        }
      }
      #print(At) #Optional
      #print(Bt) #Optional
      if (verbose =='TRUE') print('Match vector counting...')
      B[m]=sum(Bt)/((N-m)^2);
      A[m]=sum(At)/((N-m)^2);
      p[m]=A[m]/B[m];
      E[m]=-log(p[m]);
      mm[m]=m;
    }
    
  }
  else
  {
    print('Error. Time series do not have the same size...')
  }
  if (verbose =='TRUE') print('Assemble and return the response')
  results <- data.frame(A,B,p,mm,E)
  return(results)
}

##################################################################
#Example of cross-sampen function in R. If you want to test the example
#first uncomment the instructions.
#Loading libraries
	#library(tictoc)
#Configuring the path
	#path="C:/codes"
	#setwd(path)
	#print(paste("Current path > ",path,sep=""))
#Set on the parameters
	#m=2
	#r=0.2
#Loading data
	#file="example_dataset.txt"
	#dataset<-read.table(file, header = TRUE, sep = "\t", dec = ".")
	#dataset1<-dataset[-1,] #To remove the first row (for NA data in the first row)
	#S1=as.matrix(dataset1$serie3)
	#S2=as.matrix(dataset1$serie4)
	#N= dim(S1)[1]

#****************Starting script without using the DLL file****************
	#tic()
	#serie<-cbind(S1,S2)
	#cross_sampen(serie,m1=m,m2=m,r=r,sflag=1,verbose=0)
	#toc()
