geomFunction <- function (No, R, maxtime) {
  NN<- matrix(NA, nrow=1, ncol=maxtime+1)
  NN[1]=100
  for (tt in 1:10) {
    NN[tt+1]= NN[tt]*R
  }
  plot(1:(maxtime+1), NN, xlab="time", ylab="N", col="blue")
  NN
  }

geomFunction(10, 0.95, 10)



