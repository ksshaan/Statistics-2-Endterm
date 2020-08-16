#ANOVA  - Completely Randomized Design
#In a completely randomized design, there is only one primary factor 
#under consideration in the experiment. The test subjects are assigned 
#to treatment levels of the primary factor at random.

#Example
#A fast food franchise is test marketing 3 new menu items. To find out 
#if they have the same popularity, 18 franchisee restaurants are randomly
#chosen for participation in the study. In accordance with the completely 
#randomized design, 6 of the restaurants are randomly chosen to test
#market the first new menu item, another 6 for the second menu item, and 
#the remaining 6 for the last menu item.

#Problem
#Suppose the following table represents the sales figures of the 3 new 
#menu items in the 18 restaurants after a week of test marketing. At .05 
#level of significance, test whether the mean sales volume for the 3 new 
#menu items are all equal.

#Item1 Item2 Item3 
#22    52    16 
#42    33    24 
#44     8    19 
#52    47    18 
#45    43    34 
#37    32    39

#Solution
#The solution consists of the following steps:
  
#  Copy and paste the sales figure above into a table file named 
#"fastfood.txt" with a text editor.
#Load the file into a data frame named df1 with the read.table function.
#As the first line in the file contains the column names, we set the 
#header argument as TRUE.

df1 = read.table("fastfood.txt", header=TRUE); df1 

#Concatenate the data rows of df1 into a single vector r .
r = c(t(as.matrix(df1))) # response data 
> Item1 Item2 Item3 
#22    52    16 
#42    33    24 
#44     8    19 
#52    47    18 
#45    43    34 
#37    32    39 

r

#Assign new variables for the treatment levels and number of observations.
f = c("Item1", "Item2", "Item3")   # treatment levels 
k = 3                    # number of treatment levels 
n = 6                    # observations per treatment
#Create a vector of treatment factors that corresponds to each element 
#of r in step 3 with the gl function.
help(gl)# Generate Factor Levels
tm = gl(k, 1, n*k, factor(f))   # matching treatments 
tm 

#Apply the function aov to a formula that describes the response r by 
#the treatment factor tm.
av = aov(r ~ tm)

#Print out the ANOVA table with the summary function.
summary(av) 
#             Df Sum Sq Mean Sq F value Pr(>F) 
#tm           2    745     373    2.54   0.11 
#Residuals   15   2200     147
#Answer
#Since the p-value of 0.11 is greater than the .05 significance level, 
#we do not reject the null hypothesis that the mean sales volume of the 
#new menu items are all equal.

tukey <-TukeyHSD(av)
plot(tukey)
#It is clear from P-adj value between the item groups that there is no 
#significant difference between each other.

