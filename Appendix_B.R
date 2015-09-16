#B.1 Assignment
'?'(mean)
help(mean)
help.search("mean")
apropos("mean") #provide with list of places for functions related keyword)
'?' (mean.Date)
RsiteSearch("Violin")
'?'(RsiteSearch)
RSiteSearch("violin") #download functions thats not in the packages yet
RSiteSearch("violin", restrict=c("function"))
help(RSiteSearch)
#B.2 Assignement
a<- 2+3
b<- a+a
a+a; a + b
y <- c(8.3, 8.6, 10.7, 10.8, 11, 11, 11.1, 11.2, 11.3, 11.4)
y = c(8.3,8.6,10.7,10.8,11,11,11.1,11.2,11.3,11.4);y
1:4
4:1
-1:3
-(1:3)
seq(from = 1, to=3, by=0.2)
seq(1, 3, by=0.2)
seq(1, 3, length=7)
rep(1, 3)
rep(1:3, 2)
rep(1:3, each=2)
sum(y)
mean(y)
max(y)
length(y)
summary(y)
Names<- c("Sarah", "Yunluan")
Names
b<- c(TRUE, FALSE)
b
class(y)
class(b)
y>10
y>mean(y)
y==11
y!=11
#Algebra with vectors
a<1:3
a<-1:3
b<-4:6
a <- 1:3
a
a+b
a*b
a+1
a*2
1/a
a * 1:2
a*1:3
a*c(1,2,1)
1:4*1:2
y[1]
y[1:3]
y>mean(y)
y[y>mean(y)]
a<-c(5,3,6,NA)
a
is.na(a)
!is.na(a)
a[!is.na.(a)]
a[!is.na(a)]
na.exclude(a)
mean(a)
mean(a,na.rm=TRUE)
d<- na.exclude(a)
d
mean(d)
# Matrices
matrix(letters[1:4], ncol=2)
M<- matrix(1:4, nrow=2)
M
M2<- matrix(1:4, nrow=2, byrow=TRUE)
M2
I<- diag(1, nrow=2);I
Minv<-solve(M)
M %*% Minv
Minv
M[1,2]
M[1, 1:2]
M[,2]
M[,]
#Data Frames
dat<- data.frame(species=c("S.altissima","S.rugosa", "E.graminifolia", "A.pilosus"), treatment=factor(c("control","Water","Control", "Water")),height=c(1.1,0.8,0.9,1), width=c(1,1.7,0.6,0.2))
dat
dat[2,]
dat[3,4]
subset(dat, treatment=="Water")
dat[,2]=="Water"
dat[dat[,2]=="Water",]
#Factors
c("Control", "Medium","High")
rep(c("Control", "Medium", "High"), each = 3)
Treatment<- factor(rep(c("Control", "Medium", "High"), each=3));Treatment
levels(Treatment)
stripchart(1:9~Treatment)
Treatment<- factor(rep(c("Control", "Medium", "High"), each=3), levels=c("Control","Medium", "High"))
levels(Treatment)
stripchart(1:9~Treatment)
my.list<- list(My.y=y, b=b, Names, Weed.data=dat,My.matrix=M2, my.no=4)
my.list
my.list[["b"]]
my.list[[2]]
my.list$b
my.list[1:2]
my.list[["b"]][1]
#Data Frames are also lists
mean(dat$height)
#Functions
mean(1:4, trim=0)
class(1:10)
class(warpbreaks)
summary(1:10)
summary(warpbreaks)
summary(lm(breaks~wool, data =warpbreaks))
MyBogusMean <- functions(x, cheat = 0.05) {SumOfX <- sum(x) n<-length(x)
trueMean<- SumOfX/n
(1+cheat)*trueMean}
trueMean<- c(100,200,300)
MyBogusMean(trueMean)
#Sorting
e<-c(5,4,2,1,3)
sort(e)
e
oreder(e)
order(e)
e[order(e)]
dat
order.nos<- order(dat$height)
order.nos
dat[order.nos,]
dat[rev(order.nos),]
m<-matrix(1:10, nrow=2)
m
apply(m, MARGIN=1,mean)
sapply(1:10, function(i) mean(rnorm(5)))
gens<-10
output<-numeric(gens+1)
output[1]<-25
for (t in 1:gens) output[t+1]<- output[t]+round(rnorm(n=1,mean = 0,sd=2),0)
output
#Graphics
data(trees)
attach(trees)
plot(Girth, Height)
par(mar=c(5,4,3,2))
plot(Girth, Volume, type="n", main ="My Trees")
points(Girth, Volume, type="h", col="lightgrey",pch=19)
hts<- (Height-min(Height))/max(Height-min(Height))
my.colors<- hcl(h=30+270*hts, alpha=0.9)
text(Girth, Volume, Height, col=my.colors, cex=0.5 + hts)
tree.sort<- trees[order(trees$Girth, trees$Height),]
matplot(tree.sort$Girth, tree.sort[,2:3], type="b")
text(18, 40, "Volume", col="darkred")
text(10,58,"Height")
quartz(,4,4)
par(mar=c(5,4,2,4))
plot(Girth, Volume, main="My Trees")
par(new=TRUE)
plot(Girth, Height, axes=FALSE, bty="n", xlab="", ylab="", pch=3)
axis(4)
mtext("Height",side=4, line=3)
par(mar=c(5,4,2,4))
plot(Girth, Volume, main="My Trees")
par(new=TRUE)
plot(Girth, Height, axes=FALSE, bty="n", xlab="",ylab="", pch=3)
axis(4)
mtext("Height", side=4, line=3)
#Controlling Graphic Devices
quartz(width=5, height=3)
quartz(,5,5)
layout(matrix(c(1,2,3,3), nrow=2,byrow=TRUE))
plot(Girth, Height)
par(mar=c(3,3,1,1), mgp=c(1.6,0.2,0), tcl=0.2)
plot(Girth, Height)
par(mar=c(3,3,2,1), mgp=c(1.6,0.2,0), tcl =0.2)
plot(Girth, Height, axes=FALSE, xlim=c(8,22))
axis(1, tcl=-0.3)
axis(2, tick=F)
rug(Height, side=2,col=2)
title("A Third, Very Wide, Plot")
getwd()
quartz(,4,4)
plot(Height, Volume, main="Tree Data")
dev.print(pdf, "MyTree.pdf")
summary(Girth)
stem(Girth)
layout(matrix(c(1,2,2,3,4,4), nrow=2, byrow=TRUE))
plot(1:length(Girth), Girth, xlab="Order of Sample Collection?")
hist(Girth, prob=TRUE)
rug(Girth)
lines(density(Girth))
boxplot(Girth, main="Boxplot of Girth")
points(jitter(rep(1,length(Girth))),Girth)
qqnorm(log(Girth))
qqline(log(Girth))
title(sub="log transformed data")
