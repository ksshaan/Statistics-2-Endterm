#Upper Tail Test of Population Proportion
#Problem:Suppose that 12% of apples harvested in an orchard last year
#was rotten. 30 out of 214 apples in a harvest sample this year turns 
#out to be rotten. At .05 significance level, can we reject the null
#hypothesis that the proportion of rotten apples in harvest stays below
#12% this year?
  
#Solution:
#The null hypothesis is that p ≤ 0.12. We begin with computing the test 
#statistic.

pbar = 30/214          # sample proportion 
p0 = .12               # hypothesized value 
n = 214                # sample size 
z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 
z                      # test statistic 
#[1] 0.90875
#We then compute the critical value at .05 significance level.

alpha = .05 
z.alpha = qnorm(1−alpha) 
z.alpha                # critical value 
#[1] 1.6449
#Answer
#The test statistic 0.90875 is not greater than the critical value of 
#1.6449. Hence, at .05 significance level, we do not reject the null 
#hypothesis that the proportion of rotten apples in harvest stays below 
#12% this year.

#Alternative Solution 1
#Instead of using the critical value, we apply the pnorm function to 
#compute the upper tail p-value of the test statistic. As it turns out 
#to be greater than the .05 significance level, we do not reject the null
#hypothesis that p ≤ 0.12.

pval = pnorm(z, lower.tail=FALSE) 
pval                   # upper tail p−value 
#[1] 0.18174
