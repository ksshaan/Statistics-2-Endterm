# We are Verifying two categorical variable independency with Chi sq statistic mathematically.

library('MASS')
# We are using "Cars93" data for our test 
data("Cars93")
Cars93

# we are testing origin of car variable vs Type of Cars variable 

# Null Hypothesis (Ho) : cars origin (USA/Non USA) is independent of type of cars made.4

# Alternate Hypothesis (H1): cars origin (USA/Non USA) is dependent of type of cars made.


# Observed Frequency are 
observed = table(Cars93$Origin, Cars93$Type)
observed
dim(observed)

# proportion of origin in USA and Non USA
org_tot = prop.table(apply(observed, 1, sum))
org_tot = as.matrix(org_tot)
org_tot
dim(org_tot)

# frequency of Cars type across cars type(Compact, Large, Midsize, Small, Sporty, Van)
type_tot = apply(observed,2, sum)
type_tot = as.matrix(type_tot)
t(type_tot)
dim(t(type_tot))


# Expected Frequency are:
Expected = org_tot%*%t(type_tot)   #2*1 1*6 ==>  2*6 matrix
Expected

# Calculating (E-O)^2/E as differences
difference = ((Expected - observed)^2)/Expected
difference

# Taking sum across Row & col to cal test stat.
test_stat = sum(difference)
test_stat                  # 14.07985

# Degree of fredom, dof_origin = (2-1)=> 1, dof_type = (6-1) => 5
deg_fredom = (2-1)*(6-1)
deg_fredom

# Lets test our statistic with 5% significance level
Sig_level = 0.05

# This value is calculated from Chi Square Table
crit_val = 11.0705  # From Chi Sq Table
crit_val

# Conclusion
# As test statistic is to the rgiht of critical value, i.e test_stat > crit_val
# we Reject Null Hypothesis at 5% significance Level

# Verifying Our Results:

chisq.test(Cars93$Type, Cars93$Origin)  # Tse

"Pearson's Chi-squared test

data:  Cars93$Type and Cars93$Origin
X-squared = 14.08, df = 5, p-value = 0.01511"

# Our Results are Matching with above test











