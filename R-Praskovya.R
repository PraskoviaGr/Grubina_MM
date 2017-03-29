#Начало.

iris[1,3]
head[iris]
x=iris
tail(iris)
x(1,1:4)

#Задание № 1
x <- list("a" = mean(iris$Petal.Width), "b" = mean(iris$Sepal.Width), "c" = mean(iris$Petal.Length), "d"=mean(iris$Sepal.Length))
x
typeof(x)
x
#Задание № 2
ir=iris[c(1:4)] #ирис без последней колонки
y=vector()
for(i in 1:length(iris$Sepal.Length)){
  y[i] <- mean(t(ir[i,])) #в У вектор из средних
}
y


# Задание № 3 (про нуклеотиды)
nucl <- c("A", "T", "G", "C") 
DNA = nucl[runif(1000, 1, 5)] #массив случайных нуклеотидов
dna=summary(factor(DNA)) #сколько их всего
dna
dna_at=dna[-c(2, 3)] #убираем лишние
ratio=dna_at/length(DNA) #доля в общей цепочке
ratio


#Заддание № 4 ( про буквы)
txt = letters[runif(1000, 1, 26)] #массив случайных букв
txt=factor(letters[runif(1000, 1, 26)])
txt1=summary(txt)[c("a", "e","i","o","u","y")] #сколько их всего
txt1


#Задание №5
iris
names(iris)
iris$Petal.Length
setosa=c(iris$Petal.Length[1:50])
setosa
versicolor=c(iris$Petal.Length[51:100])
versicolor
virginica=c(iris$Petal.Length[101:150])
mean(setosa)
mean(versicolor)
mean(virginica)
Species=c(mean(setosa),mean(versicolor),mean(virginica))
sort(Species)
f=factor(sort(Species))
f
names(f)<- c("setosa", "versicolor", "virginica")
f
#Задание №6 (Напишите функцию для рассчета медианы вектора)
median <- function(x) {
  z=sort(x)
  if((length(z)%%2)!=0){
    result = z[(length(x)/2)+1]
  }
  else
    result = (z[length(x)/2]+z[length(x)/2+1])/2
  return(result)
}
median(iris$Sepal.Length)

#Задание № 7
iris
names(iris)
x=iris$Sepal.Length[1:50]
y=iris$Petal.Length[1:50]
plot(x,y,main="The dependence of the length of the sepal length of petal to form setosa", xlab="Sepal.Length",ylab="$Petal.Length", col="purple")
x=NULL
y=NULL
x=iris$Sepal.Length[51:101]
y=iris$Petal.Length[51:101]
plot(x,y,main="The dependence of the length of the sepal length of petal to form versicolor", xlab="Sepal.Length",ylab="$Petal.Length", col="red")
x=NULL
y=NULL
x=iris$Sepal.Length[101:150]
y=iris$Petal.Length[101:150]
plot(x,y,main="The dependence of the length of the sepal length of petal to form virginica", xlab="Sepal.Length",ylab="$Petal.Length", col="orange")
x=NULL
y=NULL


#Задание № 8
library(ggplot2)
diamonds
levels(factor(diamonds$clarity))
x=levels(factor(diamonds$clarity))
y=vector()
for (i in 1:length(x)) {
  y[i]=mean(diamonds$price[(diamonds$price>1000) & diamonds$clarity==x[i]])
}
y

#Задание № 9 (к. Спирмена)
spirman <- function(x, y) {
  if(length(x)==length(y) && is.vector(x)==TRUE && is.vector(y)==TRUE){
    p=1
    n=length(x)
    rx=rank(x)
    ry=rank(y)
    for(i in 1:n)
    {
      p = p - ((( rx[i] - ry[i] )^2)*6)/(n*(n^2-1))
    }
  }
  else{
    if(is.vector(x)==TRUE && is.vector(y)==TRUE)
      print("Ошибка. Вектора должны быть одинаковой длины")
    else
      print("Ошибка.Входные данные должны быть векторами")
    p=-1
  }
  return(p)
}
spirman(iris$Sepal.Length,iris$Petal.Length)

