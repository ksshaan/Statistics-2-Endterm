library(readxl)


df = read_excel("ImportantVariables.xlsx",sheet = "data", col_name=TRUE) 
View(df)
df$Strength_scaled<-scale(df$Strength)
df$Time_scaled<-scale(df$Time)
df$Pressure_scaled<-scale(df$Pressure)
df$Temperature_scaled<-scale(df$Temperature)

lmr <- lm(Strength ~	Time_scaled	+ Pressure_scaled	+Temperature_scaled,data=df)

summary(lmr)