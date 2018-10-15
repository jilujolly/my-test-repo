CtoF<-function(C){
  K=1.8*(C)+32
  return(K)
}
CtoF(273)

FtoC<-function(K){
  C<-(K-32)/1.8
  return(C)
}
FtoC(0)
