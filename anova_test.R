#install.packages("readxl")
library(readxl)


df = read_excel("fastfood.xlsx",sheet = "data", col_name=TRUE) 

sales_aov = aov(sales~item, data=df)

sales_aov
summary(sales_aov)

tukey <-TukeyHSD(sales_aov)
plot(tukey)