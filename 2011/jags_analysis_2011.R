library(rjags)
#library(arm)
library(coda)
library(superdiag)
library(R2WinBUGS)

setwd("C:/Users/Lenchick/Google Drive/WASHU FALL2014/Third year paper/steps")
print(Sys.time())
iter=1000
source("spain2011.jags")
dd$selflr <- NULL
d1=dd

#pure spatial model, common coefficients (beta) for 2 ideological dimensions 
#write.model(basicVCL1,"basicVCL1")
#pure spatial model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions 
#write.model(basicVCL2,"basicVCL2")
#pure spatial model, conditional model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions,
#beta2 only for Catalona 9, Basque Country 16, Galicia 12, Valencia 10
#write.model(basicVCL3,"basicVCL3")
#pure spatial model, conditional model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions,
#beta2 only for Catalona 9, Basque Country 16, Galicia 12, Valencia 10, beta2s and beta1 are disting for the regions
#write.model(basicVCL4,"basicVCL4")
#pure spatial model, conditional model, distinct coefficients (beta1 and beta2) for 2 ideological dimensions,
#beta2 only for Catalona 9, Basque Country 16, Galicia 12, Valencia 10, beta2s and beta1 are disting for the regions +  age, gender
#write.model(basicVCL5,"basicVCL6")

#for basicVCL1-4
d1$sex =NULL
d1$lage=NULL

mspatial=NULL
mspatial.out=NULL

name1=c("beta","lambda","mu","taum","taul")
name2=c("beta1","beta2","lambda","mu","taum","taul")
name3=c("beta1","beta2","lambda","mu","a","s","taum","taul")

setwd("C:/Users/Lenchick/Google Drive/WASHU FALL2014/Third year paper/steps/jags2011")
sink("output_2011_1.txt", append=FALSE, split=FALSE)

print(paste("iter=",iter))

for (i in (1:1)){
  print(Sys.time())
  f=paste0("basicVCL",i,collapse = NULL)
  d=d1
  if (i==6) d=dd
  print(f)
  mspatial <- jags.model(file=f, data=d,  n.chains=3,  n.adapt=0) 
  update(mspatial, n.iter=iter)
  if (i==1) name=name1
  if (i %in% c(2,3,4,5,7)) name=name2
  if (i==6) name=name3
  mspatial.out<- coda.samples(model=mspatial, variable.names=name,n.iter=iter)
  m1=paste0(f,"res",collapse = NULL)
  m2=paste0(f,"res.out",collapse = NULL)
  dump("mspatial",file=m1)
  dump("mspatial.out",file=m2)
  print(summary(mspatial.out))
  dump("mspatial.out",file=m2)
  print( asap2.dic <- dic.samples(mspatial, n.iter=iter, type="pD") )
  print(asap2.dic)
  image_file=paste0(f,".png")
  png(filename=image_file)
  plot(mspatial.out)
  dev.off()
}
sink()
rm("mspatial")
rm("mspatial.out")
f


#setwd("C:/Users/Lenchick/Google Drive/WASHU FALL2014/Third year paper/steps")
#source("mspatial.out")
#print(summary(mspatial.out))
#+
#( asap2.dic <- dic.samples(mspatial, n.iter=iter, type="pD") )

#plot(mspatial.out,ask = dev.interactive())

#plot(mspatial.out)
