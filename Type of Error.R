################ Type II Error in Two-Tailed Test of Population Mean with Known Variance#############################

### Problem
# Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours.
# Assume actual mean light bulb lifetime is 9,950 hours and the population standard deviation is 120 hours.
# At .05 significance level, what is the probability of having type II error for a sample size of 30 light bulb?

# Given
Mu = 9950             # actual mean population
n = 30                # sample size 
sd = 120              # population standard deviation
alpha = .05           # significance level 
Mu0 = 10000           # hypothetical mean

# Solution:  

#1 Compute S.Dev of mean or S.E

SE = sd/sqrt(n)
SE


#2 compute Lower Bound for which Mu0 = 10000 would not be rejected.


test_stat = qnorm(p = alpha, mean = Mu0, sd = SE ) # Lower Bound of sample means
test_stat

# as the sample mean is greater than 9964 in a hypothesis test,
# the null hypothesis will not be rejected.


#3 we assume that the actual population mean is 9950, 
# we can compute the probability of the sample mean above 9964.

p_type2 = pnorm(test_stat, Mu, SE, lower.tail = F)
p_type2



#The probability of type II error for testing the null hypothesis ?? ??? 10000 at .05 significance level is 26.2%, 
#c and the power of the hypothesis test is 73.8%.

