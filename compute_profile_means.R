compute_profile_means = function(PROFCODES,prof_index,ntimes,exp_min,exp_max,
                                 constraints_vector,min_delta){

  #this function computes mean values of 0, X, Y, X+Y for each profile.
  #parameters: PROFCODES (definition of all profiles); prof_index: profile
  #of interest; ntimes: how many times the vector of means is computed;
  #exp_min: min of expression range; exp_max: max of expression range;
  #constraints_vector: needed for linear solver; min_delta: minimum signal
  
  #example of use

  #load("constraints_vector")
  #source("compute_profile_means.R")
  #source("compute_minimum_delta.R")
  #PROFCODES = read.table("profile_codes_v2.txt",header = TRUE,sep = "\t")
  #   prof_index=1
  #   ntimes=5
  #   exp_min=2
  #   exp_max=16
  #   min_delta=1 #signal
  #x=compute_profile_means(PROFCODES,prof_index,ntimes,exp_min,exp_max,constraints_vector,min_delta)[,1:4]
  #barplot(x[1,])
  #add=x[1]+(x[2]-x[1])+(x[3]-x[1])
  #abline(h=add,col="red")
  

  options(warn=-1)  
  
  library(limSolve)
  
  b=cbind(as.numeric(PROFCODES[prof_index,1:10]))
  
  E=matrix(constraints_vector[b==0,],ncol=4)
  
  F=cbind(rep(0,dim(E)[1]))
  
  G1=matrix(constraints_vector[b==1,],ncol=4)
  H1=cbind(rep(0,dim(G1)[1]))
  
  G2=-matrix(constraints_vector[b==-1,],ncol=4)
  H2=cbind(rep(0,dim(G2)[1]))
  
  Gvar=rbind(diag(4),-diag(4))
  
  Hvar=cbind(c(exp_min,exp_min,exp_min,exp_min,
               -exp_max,-exp_max,-exp_max,-exp_max))
  
  G=rbind(G1,G2,Gvar)
  H=rbind(H1,H2,Hvar)
  
  if (dim(E)[1]==0 & dim(G)[1]>0){
    
    synth=xsample(G = G, H = H,iter=(5*ntimes+1))[[1]]
    
  } else {
    
    synth=xsample(E = E, F = F, G = G, H = H,iter=(5*ntimes+1))[[1]]
    
  }
  
  synth=synth[-1,]
  
  min_shift=apply(synth,1,function(x) compute_minimum_delta(x,PROFCODES,prof_index))
  
  output=cbind(synth,min_shift)
  
  output=output[which(min_shift>min_delta),]

  output=output[sample(1:ntimes),]
  
  return(output)
  
}

