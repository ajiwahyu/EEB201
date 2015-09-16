No <- 100
R <- 1.05
maxtime <- 10

NN<- matrix(NA, nrow=1, ncol=maxtime+1)
NN[1]=100
for (tt in 1:10) {
NN[tt+1]= NN[tt]*R}
NN
plot(1:(maxtime+1), NN, xlab="time", ylab="N", col="blue")
