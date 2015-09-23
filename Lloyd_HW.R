#3.2.1 mini exercise
geomFunction <- function (No, R, maxtime) {
  NN<- matrix(NA, nrow=1, ncol=maxtime+1)
  NN[1]=100
  for (tt in 1:10) {
    NN[tt+1]= NN[tt]*R
  }
  plot(1:(maxtime+1), NN, xlab="time", ylab="N", col="blue")
  NN
}

geomFunction(10, 0.7, 10)

#3.2.2 Exercise
geomFunction <- function (No, R, maxtime) {
  NN<- matrix(NA, nrow=1, ncol=maxtime+1)
  NN[1]=100
  for (tt in 1:10) {
    NN[tt+1]= NN[tt]*R
  }
  plot(1:(maxtime+1), NN, xlab="time", ylab="N", col="blue")
  NN
}

geomFunction(10, 0.7, 10)

#3.3.1 Mini exercise
geomfunction2<- function(No, R, maxtime, K) {
  NN<- matrix(NA, nrow=1, ncol=maxtime+1)
  NN[1]=100
  for (tt in 1:10) {
    NN[tt+1]= NN[tt]*    (1+   R*    (1-(NN[tt]/K)))}
  NN
  plot(1:(maxtime+1), NN, xlab="time", ylab="N", col="blue")
}
geomfunction2(10, 0.7, 10, 50)

#3.3.2 mini exercise


K=100
N0=10
par(mfrow = c(2,3))
rdVec=c(-0.3, 0.3, 1.3, 1.9, 2.2, 2.7)

geomfunction2<- function(maxtime) {
  NN<- matrix(NA, nrow=1, ncol=maxtime+1)
  NN[1]=N0
  for (tt in 1:length(rdVec)) {
    NN[tt+1]= NN[tt]*    (1+   R*    (1-(NN[tt]/K)))}
  }
geomfunction2(1) 
}


plot(1:(maxtime+1), NN, xlab="time", ylab="N", col="blue")
  
  