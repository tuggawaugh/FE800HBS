x <- c(1,3,2,5) # c(...) is a concatenation that creates a vector in R
x               # either <- or = can be used as an assigner

?c     #either can be used to get help with a function
help(norm)

x=c(1,6,2)
x

y=c(1,4,3)

length(x)  #gives the length of a vector
length(y)
x+y       #when addition is used with a vector adds the two together element by element

ls()       #gives a list of declared variables
rm(x,y)    #removes from memory those variables in the argument
ls()

rm(list=ls())   #nukes the memory

x=matrix(data=c(1,2,3,4),nrow=2,ncol=2)  #declares a matrix, fills columns first
x

x=matrix(c(1,2,3,4),2,2)
x=matrix(c(1,2,3,4),2,2,byrow=TRUE)      #fills a matrix by rows

sqrt(x)     #operations on matrices does it to each element, not to the matrix as a whole
x^2


x=rnorm(50)     #generates 50 standard normal random variables
y=x+rnorm(50,mean=50,sd=.1)   #rnorm with arguments for mean and sd change the normal sampled from
cor(x,y)

set.seed(1303)   #for purposes of matching the book, but shouldn't be used otherwise if you desire true normal
rnorm(50)

#[1] -1.1439763145  1.3421293656  2.1853904757  0.5363925179  0.0631929665  0.5022344825 -0.0004167247
#[8]  0.5658198405 -0.5725226890 -1.1102250073 -0.0486871234 -0.6956562176  0.8289174803  0.2066528551
#[15] -0.2356745091 -0.5563104914 -0.3647543571  0.8623550343 -0.6307715354  0.3136021252 -0.9314953177
#[22]  0.8238676185  0.5233707021  0.7069214120  0.4202043256 -0.2690521547 -1.5103172999 -0.6902124766
#[29] -0.1434719524 -1.0135274099  1.5732737361  0.0127465055  0.8726470499  0.4220661905 -0.0188157917
#[36]  2.6157489689 -0.6931401748 -0.2663217810 -0.7206364412  1.3677342065  0.2640073322  0.6321868074
#[43] -1.3306509858  0.0268888182  1.0406363208  1.3120237985 -0.0300020767 -0.2500257125  0.0234144857
#[50]  1.6598706557

set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)


x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")

setwd("E:/Courses/Fall '16/FE 590/Week01") #sets a working directory so you know where to find your files
pdf("Figure.pdf")      
#jpeg("Picture.jpeg") #also works with jpg
plot(x,y,col="green")
dev.off()

x=seq(1,10)
x

x=1:10
x

x=seq(-pi,pi,length=50)
x
w=seq(0.35,58.43,by=.01)
w
length(w)
z=seq(.35,58.43,length=5809)
length(z)
w-z

y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)


A=matrix(1:16,4,4)
A

A[2,3]

A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),] # this means i don't want these rows
A[c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)


url <- "http://www-bcf.usc.edu/~gareth/ISL/Auto.data"
Auto <- read.table(url)
fix(Auto)

Auto=read.table(url,header=T,na.strings="?")
fix(Auto)

url2<-"http://www-bcf.usc.edu/~gareth/ISL/Auto.csv"
Auto=read.csv(url2,header=T,na.strings="?")
fix(Auto)
dim(Auto)
Auto[1:4,]

Auto=na.omit(Auto)
dim(Auto)

names(Auto)


plot(cylinders,mpg)

plot(Auto$cylinders,Auto$mpg)
attach(Auto)
plot(cylinders,mpg)

cylinders=as.factor(cylinders)

plot(cylinders,mpg)
plot(cylinders,mpg,col="red")
plot(cylinders,mpg,col="red",varwidth=T)
plot(cylinders,mpg,col="red",varwidth=T,horizontal=T)
plot(cylinders,mpg,col="red",varwidth=T,xlab="cylinders",ylab="MPG")

hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)


pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration,Auto)

plot(horsepower,mpg)
identify(horsepower,mpg,name)

summary(Auto)

summary(mpg)
summary(as.factor(cylinders))