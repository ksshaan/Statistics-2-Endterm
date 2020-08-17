library(ISLR)
library(tibble)
as_tibble(Default)

View(Default)
dim(Default)
set.seed(42)
#splitting train vs test dataset equally
default_idx = sample(nrow(Default), 5000)
default_trn = Default[default_idx, ]
default_tst = Default[-default_idx, ]

default_trn_lm = default_trn
default_tst_lm = default_tst

# simple model with only balance as a predictor.

model_glm = glm(default ~ balance, data = default_trn, family = "binomial")
#Function which allows us to make predictions based on 
#different probability cutoffs.

get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}

#Let’s use this function to obtain predictions using a 
#low, medium, and high cutoff. (0.1, 0.5, and 0.9)

test_pred_10 = get_logistic_pred(model_glm, data = default_tst, res = "default", 
                                 pos = "Yes", neg = "No", cut = 0.1)
test_pred_50 = get_logistic_pred(model_glm, data = default_tst, res = "default", 
                                 pos = "Yes", neg = "No", cut = 0.5)
test_pred_90 = get_logistic_pred(model_glm, data = default_tst, res = "default", 
                                 pos = "Yes", neg = "No", cut = 0.9)

#Now we evaluate accuracy, sensitivity, and specificity
#for these classifiers.
test_tab_10 = table(predicted = test_pred_10, actual = default_tst$default)
test_tab_50 = table(predicted = test_pred_50, actual = default_tst$default)
test_tab_90 = table(predicted = test_pred_90, actual = default_tst$default)

test_con_mat_10 = confusionMatrix(test_tab_10, positive = "Yes")
test_con_mat_50 = confusionMatrix(test_tab_50, positive = "Yes")
test_con_mat_90 = confusionMatrix(test_tab_90, positive = "Yes")
help("confusionMatrix")
help(rbind)

#We see then sensitivity decreases as the cutoff is increased.
#Conversely, specificity increases as the cutoff increases. 
#This is useful if we are more interested in a particular error, instead of giving them equal weight.
#Note that usually the best accuracy will be seen near c=0.50

#Instead of manually checking cutoffs, we can create an ROC 
#curve (receiver operating characteristic curve) which will 
#sweep through all possible cutoffs,and plot the sensitivity and specificity.

library(pROC)
test_prob = predict(model_glm, newdata = default_tst, type = "response")
test_roc = roc(default_tst$default ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)
#A good model will have a high AUC, that is as often as possible a high sensitivity and specificity.