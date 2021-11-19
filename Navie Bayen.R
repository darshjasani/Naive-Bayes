chills = c("Y","Y","Y","N","N","N","N")
runny_nose = c("N","Y","N","Y","N","Y","Y")
head_ache = c("Mild","No","Strong","Mild","No","Strong","Strong")
fever = c("Y","N","Y","Y","N","Y","N")
Flu = c("N","Y","Y","Y","N","Y","N")

data = matrix(as.matrix(c(chills,runny_nose,head_ache,fever,Flu)),nrow=7,ncol=5)

new = c("Y","Y","Mild","Y","Y")
ftable = function(x,y) {
  r = table(x,y)
  return(r)
}
p=1
q=1
for (i in 1:4){
a = ftable(data[,i],data[,5])
b = colSums(as.matrix(a[,"N"]))
c = colSums(as.matrix(a[,"Y"]))
d = a[new[i],"N"]
e = a[new[i],"Y"]

p = p * d/b
q = q * e/c
}

z = ftable(data[,5],data[,5])
p_yes =  q * z[2,2]/7
p_no = p * z[1,1]/7

yes_pro =  p_yes/(p_yes+p_no)
no_pro =  p_no/(p_yes+p_no)

if(yes_pro > no_pro) {
  print("Person having FLu is Yes")
} else{
  print("Person having FLu is No")
}

