#trying in class exercise

cat("hello world")
getwd()
tips<- c("learn R", "love R")
tips
ls()
setwd("~/Users/AjiWahyuAnggoro/Documents/github/EEB201Boot/EEB201")
source("source.example.R")
library(ape)
tt<- read.tree("tree.tree")
tt
str(tt)
tt$tip
head(tt$tip.label)
dd<- read.table("data.txt")
dd<- read.table("data.txt", header=T, as.is=T, sep='\t')
dd
head(dd)
dim(dd)
dim(dd)[1]
dflength<- dim(dd)[1]
#generate some random size data
size<- runif(dflength)
size
cbind(dd, size)
newdd<- cbind(dd, size)
newdd
dd
dim(newdd)
head(dd$species)
dd[1,]
which(dd$mode=='MPF') #find me in dd in coloumn mode a valus that equals to MPF
dd$mode=='MPF'
dd[which(dd$mode=='MPF'),]
dd[,]
mpfs<- which(dd$mode=='MPF')
mpfs
just_mpf<- dd[which(dd$mode=='MPF'),]
head(just_mpf)
dim(dd)
dim(newdd)
notfish <- c("bat", "dolphin", "toad", "soldier")
for(animal in notfish) {cat(animal,"fish\n", sep="")}
for(animal in notfish) {cat(animal,"fish", "are tasty\n", sep="")}
for(animal in notfish) {cat(animal,"fish", " are tasty\n", sep="")}
for(animal in notfish) {cat(animal," fish", "are tasty\n", sep="")}
for(animal in notfish) {cat(animal,"fish", " are tasty\n", sep="")}
##while
xx<-1
while(xx<5) {
xx <- xx+1;
cat("value of xx", xx, "\n") xx<- xx+1; if (xx == 3) {break;} }
xx<-1
while(xx<10) {
  cat("value of xx", xx, "\n") 
  xx<- xx+1 
  if (xx == 7) {
    cat("lucky number", xx, "\n")}
  else if (xx == 2) {
    cat ("the number", xx, "is unlucky\n")}
 else {
   cat ("not excited about the number ", xx, "\n")}
}
print(xx)

