# excercise 1

snpsDataFrame=read.table('hapmap_CEU_r23a_chr2_ld-1.txt',header=TRUE)
head(snpsDataFrame)
dim(snpsDataFrame)
head(snpsDataFrame)
names(snpsDataFrame)
row.names(snpsDataFrame)
snps=as.matrix(snpsDataFrame)


## APPYLING A CHI-SQUARE TEST TO EACH SNP TO FORMALLY LOOK FOR DEPARTURES FROM HARDY-WEINBERG EXPECTATIONS ###

compute_chisquare=function(x){
        freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
        cnt0=sum(x==0,na.rm=TRUE)
        cnt1=sum(x==1,na.rm=TRUE)
        cnt2=sum(x==2,na.rm=TRUE)
        obscnts=c(cnt0,cnt1,cnt2)
        #print(obscnts)
        n=sum(obscnts)
        expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
        chisq=sum((obscnts-expcnts)^2/expcnts)
        return(chisq)
}

#let's make a second funtion that makes use of R's built in chisq.test function

compute_chisquare_2=function(x){
        freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
        cnt0=sum(x==0,na.rm=TRUE)
        cnt1=sum(x==1,na.rm=TRUE)
        cnt2=sum(x==2,na.rm=TRUE)
        obscnts=c(cnt0,cnt1,cnt2)
        #print(obscnts)
        n=sum(obscnts)
        #here we use the built-in function for the chi-sq distribution:
        exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) #note, here we don't multiply by n
        chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
        return(chisq)
}




# Apply the compute_chi_square function to each snp
chisqs=apply(snps,1,compute_chisquare)
chisqs2=apply(snps,1,compute_chisquare_2)

#check to see that the chisquare statistcs are the same:
#first do this by computing Pearson's correlation coefficient:
cor.test(chisqs,chisqs2)

#we can also do a quick scatterplot:
plot(chisqs,chisqs2)

# Compute p-values for each chi-square value using the pchisq function
pvals=pchisq(chisqs,1,lower.tail=FALSE)

signifthres<-0.05
sum(pvals<signifthres) 
mean(pvals<signifthres) # 181

signifthres<-0.01
sum(pvals<signifthres) 
mean(pvals<signifthres) # 41

signifthres<-0.001
sum(pvals<signifthres) 
mean(pvals<signifthres) # 5

length(pvals)
num_pval<- length(pvals)
# 4014

nn <- 1:length(pvals)
for(i in 1:length(pvals)){
        exp_pvals[i] <- nn[i]/num_pval
        
}


sort_pvals <- sort(pvals)
log_short_pvals <- -log10(sort_pvals)
plot(log_short_pvals)

log_exp_pvals <- -log10(exp_pvals)
plot(log_exp_pvals)

plot(log_short_pvals, log_exp_pvals)


par(mfrow = c(1,1))

qqplot(log_exp_pvals, log_short_pvals, plot.it = TRUE)

qqline(log_exp_pvals, datax = FALSE, distribution = rnorm, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       probs = c(0.01, 0.95), qtype = 7)


# excercise 2.a

zz=read.table('pheno.sim.2014-1.txt', header=TRUE)
sort(zz$glucose_mmolperL, decreasing = FALSE)
yy=sort(zz$glucose_mmolperL)
order(zz$glucose_mmolperL, decreasing = FALSE)
xx=order(zz$glucose_mmolperL, decreasing = FALSE)

# Exercise 2.b
glucose_mmolperL=yy[1:15]
indv=xx[1:15]
control=data.frame(glucose_mmolperL,indv)

# Exercise 2.c
glucose_mmolperL_H=yy[46:60]
indv_H=xx[46:60]
cases=data.frame(glucose_mmolperL_H, indv_H)
cases

#Exercise 2.d
d<- density(zz$glucose_mmolperL)
plot(d)

abline(v=quantile(zz$glucose_mmolperL,0.75),lty=2,lwd=3,col=2)
abline(v=quantile(zz$glucose_mmolperL,0.25),lty=2,lwd=3,col=1)

dev.off()


# 2.e
case_genotypes <- factor(snps["rs7584086_T",])
control_genotypes <- factor(snps["rs7584086_T",])

#2.f.g.h
case_genotypes
attr(case_genotypes, "level")
table(case_genotypes)
# case_genotypes
# 0  1  2 
# 39 17  3 

table(control_genotypes)

