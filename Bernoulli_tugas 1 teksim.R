multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3])
  View(xj)
}
multiplicative_RNG(45,21139,417,150)

Bernoulli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
Bernoulli_1(150,0.83)