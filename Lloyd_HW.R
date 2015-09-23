#Initial Formula
N0<-25
RR<-1.05
ttMax<-100

NN<- matrix(NA, nrow=1, ncol=ttMax+1);NN
NN[1]<-N0

for (tt in 1:ttMax) {NN[tt+1]<- RR*NN[tt]
}
plot(1:(ttMax+1), NN, xlab="time", ylab="N", type="b", col='blue')

#Declining Population
N0<-25
RR<-0.9
ttMax<-100

NN<- matrix(NA, nrow=1, ncol=ttMax+1);NN
NN[1]<-N0

for (tt in 1:ttMax) {NN[tt+1]<- RR*NN[tt]
}
plot(1:(ttMax+1), NN, xlab="time", ylab="N", type="b", col='blue')

#function exercise
ttMax <- 100
geomFunction <- function(N0, RR) {
  NN<- matrix(NA, nrow=1, ncol=ttMax+1);NN
  NN[1]<-N0
  
  for (tt in 1:ttMax) {NN[tt+1]<- RR*NN[tt]
  }
  plot(1:(ttMax+1), NN, xlab="time", ylab="N", type="b", col='blue')
}
geomFunction(10,1.2)

#3.3.1 Mini Exercise
ttMax <- 100
geomfunction2<- function(N0, R, K) {
  NN<- matrix(NA, nrow=1, ncol=ttMax+1)
  NN[1]= N0
  for (tt in 1:ttMax) {
    NN[tt+1]= NN[tt]*    (1+   R*    (1-(NN[tt]/K)))}
  NN
  plot(1:(ttMax+1), NN, xlab="time", ylab="N", col="blue")
}
geomfunction2(10, 0.5, 100)

#3.3.2
ttMax <- 100
N0 <- 10
K = 100
par(mfrow=c(2,3))
rdVec<- c(-0.3, 0.3, 1.3, 1.9, 2.2, 2.7)

for (i in 1 to length(rdVec) {geomfunction2<- function(rdVec) {
  
NN<- matrix(NA, nrow=1, ncol=ttMax+1)
  NN[1]= N0
  for (tt in 1:length(rdVec)) {
    NN[tt+1]= NN[tt]*    (1+   rdVec*    (1-(NN[tt]/K)))}
  NN
  plot(1:(ttMax+1), NN, xlab="time", ylab="N", col="blue")
}
}
geomfunction2(-0.3)

install.packages('deSolve')
library(deSolve)

#4.1

expGrowthODE<- function (tt, yy, pars) {
  derivs <- pars["rr"] * yy
  return(list(derivs))
}

init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 0.1)

expGrowthOutput <- lsoda(init, tseq, expGrowthODE, pars)
plot(expGrowthOutput[ ,1], expGrowthOutput[,2])



#4.2

expGrowthODE<- function (tt, yy, pars) {
  derivs <- pars["rr"] * yy (1 - (yy/(pars["k"]))
                             return(list(derivs))
}

init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 0.9, k= 0.1)
#k=10

expGrowthOutput2<- lsoda(init, tseq, expGrowthODE, pars)
plot(expGrowthOutput2[ ,1], expGrowthOutput2[,2], col="red")



## Exponential growth comments

#This line sets up the function
  #The function name is expGrowthODE
  #The function inputs are tt, yy and pars
expGrowthODE <- function(tt, yy, pars){ 
  derivs <- pars[’rr’] * yy
  #This defines the equation
   # derivs is the output
   # the equation is: rr*yy
  
  return(list(derivs))
  #this line just outputs the answer, derivs, as a list
}


expGrowthODE<- function (tt, yy, pars) {
  derivs <- pars["rr"] * yy
  return(list(derivs))
}



head


















































































































































































































































































































































































































