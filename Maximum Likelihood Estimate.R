library("MASS")
data("survey")

# Predicting gender based on student height.


df = data.frame("Gender" = survey$Sex, "Height" = survey$Height)
df = df[complete.cases(df),]
df

'Coefficients:
  (Intercept)       Height  
  -43.1119            0.2512'
'log_reg = glm(Gender~ Height, data= df, family = binomial)
log_reg
'

df$z = df$Height*0.2512 -43.1119
df

df$Prob_Male = exp(df$z)/(1+exp(df$z))
df


df$Prob_Female = 1 - df$Prob_Male
df


df$pred_class = ifelse(test = df$Prob_Female > 0.5, yes = "Female", no = "Male")
df

df$P_of_correct_class = ifelse(test = df$Gender == "Female", yes = df$Prob_Female, no = df$Prob_Male)
df

df$log = log(df$P_of_correct_class)

df


# Log_likelihood & Residual deviance for P = 0.5

log_likelihood = sum(df$log)  # Should be maximize = -83.5161
log_likelihood

Residual_deviance = -2*log_likelihood    #Should be minimize = 167.0322
Residual_deviance


#----------------------------------------------------------------------------------------------------#
################################## Null Deviance Calculation #########################################
#----------------------------------------------------------------------------------------------------#

library("MASS")
data("survey")

# Predicting gender based on student height.

df = data.frame("Gender" = survey$Sex, "Height" = survey$Height )
df = df[complete.cases(df),]
df
df$Height = mean(df$Height)
df
'Coefficients:
  (Intercept)       Height  
  -43.1119            0.2512'
'log_reg = glm(Gender~ Height, data= df, family = binomial)
log_reg'


df$z = df$Height*0.2512 -43.1119
df

df$Prob_Male = exp(df$z)/(1+exp(df$z))
df


df$Prob_Female = 1 - df$Prob_Male
df


df$pred_class = ifelse(test = df$Prob_Female > 0.5, yes = "Female", no = "Male")
df

df$P_of_correct_class = ifelse(test = df$Gender == "Female", yes = df$Prob_Female, no = df$Prob_Male)
df

df$log = log(df$P_of_correct_class)

df


# Log_likelihood & Residual deviance for P = 0.5

log_likelihood = sum(df$log)  # Should be maximize = -144.73
log_likelihood

Residual_deviance = -2*log_likelihood    #Should be minimize Null Dev= 289.47
Residual_deviance





