PROFCODES = read.table("profile_codes_v2.txt",header = TRUE,sep = "\t")
PROFCODES[,1:10]
source("setPowerPointStyle.R")
setPowerPointStyle()
X1=c(4,1,5,4)
X2=c(4,3,6,4)
add1=X1[1]+(X1[2]-X1[1])+(X1[3]-X1[1])
add2=X2[1]+(X2[2]-X2[1])+(X2[3]-X2[1])

barplot(X1,axes=FALSE)
abline(h=add1,col="red")

load("constraints_vector")
source("compute_profile_means.R")
source("compute_minimum_delta.R")
source("setPowerPointStyle.R")
setPowerPointStyle()

prof_index=1 #index of profile to be simulated
ntimes=50 #n. of simulations
exp_min=2 #min range of expression value
exp_max=16 #max range of expression value
min_delta=0.5 #signal (minimum difference in expression between any two comparisons)
x=compute_profile_means(PROFCODES,prof_index,ntimes,exp_min,
                        exp_max,constraints_vector,min_delta)[,1:4]
x
colnames(x)=c("0","X","Y","X+Y")
head(x[,1:4])
barplot(x[1,],ylab='expression')
add=x[1,1]+(x[1,2]-x[1,1])+(x[1,3]-x[1,1])
abline(h=add,col="red")

source("simulate_from_means.R")
source("setPowerPointStyle.R")
setPowerPointStyle()

samples=5
noise_level=5
simulated_values=simulate_from_means(x[1,],prof_index,samples,
                                     noise_level,exp_min,exp_max)

design=factor(c(rep("0",samples),rep("X",samples),
                rep("Y",samples),rep("Y+X",samples)))

names(simulated_values)=c(rep('0',samples),rep('X',samples),
                          rep('Y',samples),rep('Y+X',samples))

simulated_values
boxplot(simulated_values~design,ylab='',
        outline=FALSE,col='gray',frame=F,axes=FALSE,medcol="white")
stripchart(simulated_values ~ design, vertical = TRUE, 
           method = "jitter", add = TRUE, pch = 20, col = 'black',cex=1.5)


source("setPowerPointStyle.R")
setPowerPointStyle()

noise_level=2
source("simulate_from_means.R")
simulated_values=simulate_from_means(x[1,],prof_index,samples,
                                     noise_level,exp_min,exp_max)

boxplot(simulated_values~design,ylab='',
        outline=FALSE,col='gray',frame=F,axes=FALSE,medcol="white")
stripchart(simulated_values ~ design, vertical = TRUE, 
           method = "jitter", add = TRUE, pch = 20, col = 'black',cex=1.5)

source("setPowerPointStyle.R")
source("match11.R")

profile.features=match11(simulated_values)
length(profile.features)
head(profile.features,10)


load("constraints_vector")
source("compute_profile_means.R")
source("compute_minimum_delta.R")
source("setPowerPointStyle.R")

ntimes=10 #n. of simulations
exp_min=2 #min range of expression value
exp_max=16 #max range of expression value
min_delta=0.5 #signal (minimum difference in expression between any two comparisons)
colnames(x)=c("0","X","Y","X+Y")

for (k in 1:123){
  prof_index=k 
  x=compute_profile_means(PROFCODES,prof_index,ntimes,exp_min,
                          exp_max,constraints_vector,min_delta)[,1:4]
  
  add=x[1,1]+(x[1,2]-x[1,1])+(x[1,3]-x[1,1])
  if (add<exp_min) add=0.1
  if (add>exp_max) add=max(x[1,])+1.5
  
  #png(file = paste(paste("profile",prof_index,sep="_"),'.png',sep=""),width = 400, height = 300)
  
  #setPowerPointStyle()
  barplot(x[1,],ylab='',yaxt='n', ann=FALSE,col='black',main=paste('Profile',prof_index),ylim=c(0,max(x[1,])+1.6))
  abline(h=add,col="magenta",lwd=3)
  #dev.off()
}

