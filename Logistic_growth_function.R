geomfunction2<- function(No, R, maxtime, K) {
  NN<- matrix(NA, nrow=1, ncol=maxtime+1)
  NN[1]=100
  for (tt in 1:10) {
    NN[tt+1]= NN[tt]*    (1+   R*    (1-(NN[tt]/K)))}
  NN
  plot(1:(maxtime+1), NN, xlab="time", ylab="N", col="blue")
}
geomfunction2(10, 0.7, 10, 500)

