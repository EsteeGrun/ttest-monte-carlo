
# evaluate standard normal density function 
# f(x)=1/(sqrt(2*pi))*exp(-x^2/2)at x=2
1/(sqrt(2*pi))*exp(-2^2/2)

# use dnorm in R
dnorm(2)

# compare the following two commands
seq(0, 3, 0.5)
x=seq(0, 3, 0.5)

# help
?Syntax

# case-sensitive
x
X

# many symbols are already defined by the R base: T,F,t,q,I

# commonly used fuctions: sqrt, floor, ceiling, log, exp, factorial,
# runif, pnorm, dnorm, qnorm, rank, sort, var, cov, table, NA, is.na
x=c(.3, .1, .6, .05)
sort(x)
rank(x)

# R Syntax and functions for vectors and matrices
# 0 vector
x=numeric(5) 
y=integer(5)
z=rep(0,5)

x1=c(.1,.2,.3,.4)
x1[2]

# 0 matrix
x=matrix(0,2,3)
y=matrix(c(1,2,3,4,5,6),2,3)
y[1,]
y[1,2]

# matrix multiplication
y1=matrix(c(1,2,3,4,5,6),2,3)
y2=matrix(seq(0.1, 0.6, 0.1),2,3)
y1%*%t(y2)

#elementwise multiplication
y1*y2  

#inverse of y3
y3=matrix(c(1,2,3,4),2,2)
solve(y3)

#R online help systerm: ?topic, help(topic), or ?"topic"
?seq

# function in R
# format: fuction(arglist)expr, return

# rolls n fair dice and return the sum
sumdice=function(n)
 {
	k=sample(1:6, size=n, replace=TRUE)
	return(sum(k))
 }
sumdice(1000)/1000

# or
sumdice1=function(n)
{
	sum(sample(1:6, size=n, replace=TRUE))
}
# return the value of the last evaluated expression

# rolls s-sided dice, "sides=6" is the default
sumdice2=function(n, sides=6)
{
	k=sample(1:sides, size=n, replace=TRUE)
	return(sum(k))
}
sumdice2(1000)/1000
sumdice2(1000,2)/1000

# Arrays, data frames, lists
# Four measurements on observations from three species of iris.
attach(iris)
iris             # data frame
names(iris)
table(iris$Species)
x=iris[[2]]      # second column
mean(x)

summary(Petal.Length[51:100])
summary(iris)
by(iris[,1:4], Species, colMeans) 
detach(iris)

# array: a single type of data
x=1:24
matrix(1:24, nrow=4, ncol=6)
matrix(1:24, nrow=4, ncol=6, byrow=TRUE)

#convert iris data to a matrix
x=as.matrix(iris[,1:4])
dim(x)
mean(x[,1])
mean(x[1:50, 1])

#list can be different types
#Wilcox rank sum test

w=wilcox.test(rnorm(10), rnorm(10,2)) 
# The Wilcoxon rank-sum test is a nonparametric alternative to 
# the twosample t-test
w
w$statistic
w$p.value

# creat a list
a=matrix(runif(8),4,2)
dimnames(a)=list(NULL, c("x","y")) # give col names
a
dimnames(a)=list(letters[1:4],c("x","y")) #give row names
a
# or
row.names(a)=list("A","B","C","D")

# read data from external files
# specify web location of the data (data are removed from the website)

fmsURL <- "http://people.umass.edu/foulkes/asg/data/FMS_data.txt" 

# pull the data into R directly, a tab separates each variable, or "," and ""

fms <- read.delim(file=fmsURL, header=T, sep="\t") 
attach(fms) ## can call each variable by its name
fms
dim(fms)
b=data.frame(id, actn3_r577x, actn3_rs540874, actn3_rs1815739, actn3_1671064, Term, Gender, Age, Race, NDRM.CH,DRM.CH)[1:20,]
c=as.matrix(b)
dim(c)

# download the data to the computer in the follwoing direction
fmsdata1 <- read.delim(file="C:\\Users\\Qiuying Sha\\Desktop\\teaching\\MA5750\\2011-fall\\R program\\FMS_data.txt", header=T, sep="\t") ## a tab separates each variable, or "," and ""
attach(fmsdata1) ## can call each variable by its name
names(fmsdata1)
data.frame(fmsdata1)[1:20,1:5]

## check the current working directory
getwd() 

## change the currect working directory
setwd("c:/sqy/teaching/MA5750/2011-fall/R program/")
getwd()

fmsdata1 <- read.delim(file="FMS_data.txt", header=T, sep="\t")
attach(fmsdata1) ## can call each variable by its name
names(fmsdata1)
data.frame(fmsdata1)[1:20,1:5]

