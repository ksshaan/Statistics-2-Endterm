#Loading required R packages
library(tidyverse)
library(caret)
theme_set(theme_classic())

#Preparing the data
#We’ll use the iris data set,for predicting iris species based on the 
#predictor variables Sepal.Length, Sepal.Width, Petal.Length, Petal.Width.

#Discriminant analysis can be affected by the scale/unit in which predictor
#variables are measured. It’s generally recommended to standardize/normalize
#continuous predictor before the analysis.

# Load the data
data("iris")
# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- iris$Species %>%
createDataPartition(p = 0.8, list = FALSE)
train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]

# Estimate preprocessing parameters , NOrmalizing the data
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

library(MASS)
# Fit the model
model <- lda(Species~., data = train.transformed)
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class==test.transformed$Species)

library(MASS)
model <- lda(Species~., data = train.transformed)
model

plot(model)

predictions <- model %>% predict(test.transformed)
names(predictions)

The predict() function returns the following elements:
  
#class: predicted classes of observations.
#posterior: is a matrix whose columns are the groups, rows are the individuals and values are the posterior probability that the corresponding observation belongs to the groups.
#x: contains the linear discriminants, described above
  
# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
tail(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3)  
help(predict)
lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
geom_point(aes(color = Species))
