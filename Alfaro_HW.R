# Exercise 1 
for (i in 1:9){ if (i<9) {cat("")}
                else if (i==9)
                {cat("*", sep='\n')}
}
#Exercise 2
for (i in 1:9){ 
  cat("*&", sep="")
}
#Exercise 3 (1)

15
#Exercise 3 (2)
-30
#Exercise 3 (3)
-4
#Exercise 4, inclu
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 4 == 0){
    cat(years[ii], 'Hooray, presidential elections!', sep = '\t', fill = T)
  }
  else if (years[ii] %% 2 == 0) {
    cat(years[ii], 'Hooray, congresional elections!', sep = '\t', fill = T)}
 }

#Exercise 5
bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);
interestRate <- 0.0125;
compounded <- c(1:6)
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i];   
}
compounded

#Exercise 6
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this


for (j in 1:5) {
    for (i in 1:length(bankAccounts)) {
    bankAccounts[i] <- bankAccounts[i]+income[i]-house[i]-food[i]-fun[i]
    bankAccounts[i] <- interestRate*bankAccounts[i] + bankAccounts[i]
    
    print(paste("the year is", j, "and the person is", i, "money is", bankAccounts[i]))   
  }}

#Exercise 7

bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this
year <- c(2015, 2016, 2017, 2018, 2019, 2020);

for (j in 1:length(year)) {
  for (i in 1:length(bankAccounts)) {
    if (year[j] %% 2 == 1 && i %% 2 == 1) {
      bankAccounts[i] <- bankAccounts[i]+income[i]-house[i]-food[i]-fun[i]+5000
      bankAccounts[i] <- interestRate*bankAccounts[i] + bankAccounts[i];
      }
    else {
      bankAccounts[i] <- bankAccounts[i]+income[i]-house[i]-food[i]-fun[i]
      bankAccounts[i] <- interestRate*bankAccounts[i] + bankAccounts[i];    
    }
    print(paste("the year is", j, "and the person is", i, "money is", bankAccounts[i])) 
  }}



  
#Exercise 8
x <- 0
y<- 0
while(x <= 16) {x <- x+1; (y= x+y);x}
print(y)

#Exercise 9
AA<- function(a){
  if (a <= -1){print('small')}
  if (a>-1 && a<1){print('medium')}
  if (a >= 1){print('big')}
}




