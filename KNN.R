data(iris)
ran = sample(1:nrow(iris),0.6*nrow(iris))
norm = function(x) { (x-min(x))/(max(x)-min(x))}
df_norm = as.data.frame(lapply(iris[,c(1,2,3,4)], norm))

x_train = df_norm[ran,]
y_train = iris[ran,5]

x_test = df_norm[-ran,]
y_test = iris[-ran,5]

k = 3
min3 = c()
edist = function(a,b) { sqrt(sum((a - b)^2))}
i=1
final = function(x,j) {
  if (i <= k){
    min3[[as.character(j)]]<<-x
    i<<- i+1
  }
  else{
    min3[[as.character(j)]]<<-x
    min3<<-sort(min3)
    min3<<-min3[-(k+1)]
    }
} 
y_hat = c()
myclass = c()
a = 1;
predict = function(i1){
  
  index = names(min3)
  for (m in 1:k) {
        p = as.matrix(y_train[as.integer(index[m])])
        myclass[[m]]<<- p
  }
  class = table(myclass)
  y_hat[a]<<-names(class)[which(class==max(class))]
  a<<-a+1
}

for (i1 in 1:nrow(x_test)) {
  for( j in 1:nrow(x_train)) {
    y  = edist(x_train[j,],x_test[i1,])
    final(y,j)
  }
  predict(i1)
}

tab = table(y_hat,y_test)
accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
precision = function(x){diag(x)/rowSums(x) * 100}
recall = function(x){diag(x)/colSums(x) * 100}

accuracy(tab)
precision(tab)
recall(tab)
F1_score  = (2 * precision(tab) * recall(tab)) / (precision(tab) + recall(tab))
F1_score
