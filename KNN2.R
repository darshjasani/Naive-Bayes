customer = c("John","Rachel","Ruth","Tom","Neil")
age = c(35,22,63,59,25)
income = c(35,50,200,170,40)
cards = c(3,2,1,1,4)
response = c("Yes","No","No","No","Yes")
dataset = data.frame(customer,age,income,cards,response)

levels(dataset$response) = c(1,2)
dataset1 = as.data.frame(lapply(dataset[2:5],as.numeric))
matrixA = matrix(as.matrix(dataset1),nrow = 5,ncol = 4)


a = c("David",37,50,2,2)
a1 = as.data.frame(lapply(a[2:5], as.numeric))

x_train = matrixA
y_train = matrixA[,4]

x_test = a1
y_test = a1[,4]

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
accuracy(tab)
