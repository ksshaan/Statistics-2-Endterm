#Two-Tailed Test of Population Mean with Known Variance
#Problem:
#Suppose the mean weight of King Penguins found in an Antarctic colony
#last year was 15.4 kg. In a sample of 35 penguins same time this year 
#in the same colony, the mean penguin weight is 14.6 kg. Assume the 
#population standard deviation is 2.5 kg. At .05 significance level, 
#can we reject the null hypothesis that the mean penguin weight does 
#not differ from last year?

#Solution
#The null hypothesis is that μ = 15.4. We begin with computing the 
#test statistic.

xbar = 14.6            # sample mean 
mu0 = 15.4             # hypothesized value 
sigma = 2.5            # population standard deviation 
n = 35                 # sample size 
z = (xbar−mu0)/(sigma/sqrt(n)) 
#z [1] −1.8931         # test statistic  

#We then compute the critical values at .05 significance level.

alpha = .05 
z.half.alpha = qnorm(1−alpha/2) 
c(−z.half.alpha, z.half.alpha) 
#[1] −1.9600  1.9600
#Answer
#The test statistic -1.8931 lies between the critical values 
#-1.9600 and 1.9600. Hence, at .05 significance level, we do not 
#reject the null hypothesis that the mean penguin weight does not 
#differ from last year.

#Using p-value
#Instead of using the critical value, we apply the pnorm function 
#to compute the two-tailed p-value of the test statistic. It doubles 
#the lower tail p-value as the sample mean is less than the hypothesized
#value. Since it turns out to be greater than the .05 significance level, 
#we do not reject the null hypothesis that μ = 15.4.

pval = 2 ∗ pnorm(z)    # lower tail 
#[1] 0.058339

