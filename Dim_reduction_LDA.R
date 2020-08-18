library("MASS")
# Calling Data
data("decathlon2")
decathlon2
dim(decathlon2)

str(decathlon2)



deca_prcomp = prcomp(decathlon2[c(1:12)], center = TRUE, scale = TRUE)
summary(deca_prcomp)
sum(deca_prcomp$sdev^2)
deca_prcomp$sdev^2

class(deca_prcomp$x)
dim(deca_prcomp$x)

attributes(deca_prcomp)


screeplot(deca_prcomp, type = 'l', npcs = 10, main = 'Screenplot of first 10 PCs')
abline(h = 1, col = "red", lty=5)

legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cum_prop = cumsum(deca_prcomp$sdev^2)/sum(deca_prcomp$sdev^2)
cum_prop



plot(cum_prop[0:10], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)



plot(deca_prcomp$x[,1],deca_prcomp$x[,2], xlab="PC1 (44.3%)", 
     ylab = "PC2 (14.5%)", main = "PC1 / PC2 - plot")
library("factoextra")

fviz_pca_ind(deca_prcomp, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = decathlon2$Competition, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Competition") +
  ggtitle("2D PCA-plot from 12 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))





df = as.data.frame(deca_prcomp$x)
df
dim(df)
names(df)

df$competition = decathlon2$Competition

library(scatterplot3d)

#deca_prcomp
# plot
with(df, scatterplot3d(PC1, PC2, PC3, color = as.numeric(competition), pch=20,main="PCA For Decathlon2 datasset"), phi=20, theta =15 ) 
legend("topleft", pch=20, col=c("black", "red"), legend=c("Decastar", "OlympicG"))

df


df$competition = as.numeric(df$competition)-1
df

smp_size_raw <- floor(0.75 * nrow(df))

train_ind_raw <- sample(nrow(df), size = smp_size_raw)

train.df <- as.data.frame(df[train_ind_raw,])
test.df <- as.data.frame(df[-train_ind_raw,])
dim(train.df)
dim(test.df)
test.df

df_lda
df_lda <- lda(competition ~ ., data = df)
df_lda_pred <- predict(df_lda, newdata = df)

df_lda_pred_post = as.data.frame(df_lda_pred$posterior)  # Evalute Model
df_lda_pred_post


pred <- prediction(df_lda_pred_post[,2], df$competition)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values# Plot
plot(roc.perf) 
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#convert to data frame 
newdata <- data.frame(type = df[,13], lda = df_lda_pred$class)
library(ggplot2)
ggplot(newdata) + geom_point(aes(df$PC1, df$PC2, colour = type), size = 2.5)



#deca_prcomp
# plot
with(df_lda, scatterplot3d(PC1, PC2, PC3, color = as.numeric(competition), pch=20,main="PCA For Decathlon2 datasset"), phi=20, theta =15 ) 
legend("topleft", pch=20, col=c("black", "red"), legend=c("Decastar", "OlympicG"))

