#Lower Tail Test of Population Mean with Unknown Variance
#Problem: Suppose the manufacturer claims that the mean lifetime of a 
#light bulb is more than 10,000 hours. In a sample of 30 light bulbs, 
#it was found that they only last 9,900 hours on average. Assume the 
#sample standard deviation is 125 hours. At .05 significance level, 
#can we reject the claim by the manufacturer?
  
#Solution
#The null hypothesis is that μ ≥ 10000. We begin with computing the test
#statistic.

xbar = 9900            # sample mean 
mu0 = 10000            # hypothesized value 
s = 125                # sample standard deviation 
n = 30                 # sample size 
t = (xbar−mu0)/(s/sqrt(n)) 
#> t                      # test statistic 
#[1] −4.3818
#We then compute the critical value at .05 significance level.

alpha = .05 
t.alpha = qt(1−alpha, df=n−1) 
#−t.alpha               # critical value 
#[1] −1.6991
#Answer
#The test statistic -4.3818 is less than the critical value of -1.6991. 
#Hence, at .05 significance level, we can reject the claim that mean 
#lifetime of a light bulb is above 10,000 hours.

#Alternative Solution
#Instead of using the critical value, we apply the pt function to compute 
#the lower tail p-value of the test statistic. As it turns out to be less
#than the .05 significance level, we reject the null hypothesis that
#μ ≥ 10000.

pval = pt(t, df=n−1) 
pval                   # lower tail p−value 
#[1] 7.035e−05