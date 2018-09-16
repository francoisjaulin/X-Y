compute_minimum_delta = function(Mu,PROFCODES,prof_index){

  ADD = Mu[1] + Mu[2] - Mu[1] + Mu[3] - Mu[1]
  
  V1 = c(ADD - Mu[1],Mu[2] - Mu[1],Mu[3] - Mu[1],Mu[4] - Mu[1],
         Mu[2] - ADD,Mu[3] - ADD,Mu[4] - ADD,Mu[3] - Mu[2],
         Mu[4] - Mu[2],Mu[4] - Mu[3])

  return(min(abs(V1[ which(PROFCODES[prof_index,1:10]!=0)] )))
  
  }