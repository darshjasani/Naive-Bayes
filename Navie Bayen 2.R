data(iris)


library(e1071)
library(caTools)
library(caret)

ran = sample(1:nrow(iris),0.7*nrow(iris))
norm = function(x) { (x-min(x))/(max(x)-min(x))}
df_norm = as.data.frame(lapply(iris[,c(1,2,3,4)], norm))

x_train = df_norm[ran,]
y_train = iris[ran,5]

x_test = df_norm[-ran,]
y_test = iris[-ran,5]

nb = naiveBayes(Species ~ ., data = x_train)

y_hat =  predict(classifier_cl, newdata = test_cl)

tab = table(y_hat,y_test)
accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
precision = function(x){diag(x)/rowSums(x) * 100}
recall = function(x){diag(x)/colSums(x) * 100}

accuracy(tab)
precision(tab)
recall(tab)
F1_score  = (2 * precision(tab) * recall(tab)) / (precision(tab) + recall(tab))



