    #######################################################
    ###       Statistical Computing with R              ###          
    ###       Maria L. Rizzo                            ###
    ###       Chapman & Hall / CRC                      ### 
    ###       ISBN 9781584885450                        ###
    ###                                                 ###
    ###       R code for Chapter 3 Examples             ###
    #######################################################

### Example 3.1 (Sampling from a finite population)

# toss some coins
sample(0:1, size=100, replace=TRUE)	# with replacement
table(sample(0:1, 100, replace=TRUE))/100

# choose lottery numbers
sample(1:100, size=6, replace=FALSE) # without replacement

# permutation of letters a-z
sample(letters)
sample(letters, 100) # default is no replacement
sample(letters, 100, replace=TRUE)
table(sample(letters, 100, replace=TRUE))

# sample from a multinomial distribution
x=sample(0:2, size=1000, replace=TRUE, prob=c(.2,.3,.5))
table(x)/1000

# d-density, p-cdf, q-quantile, r-random generator
dbinom(2, size=10, prob=.4)
dbinom(2, 10, p=.4)

#check
9*10/2*.4^2*.6^8

#or  
factorial(10)/(factorial(2)*factorial(8))*.4^2*.6^8
choose(10,2)*.4^2*.6^8

x=pbinom(2, size=10, prob=.4)
qbinom(x, size=10, prob=.4) 
y=rbinom(1000, size=10, prob=.4)
table(y)/1000
hist(y)

#lognormal(x, mu, sigma)
dlnorm(2, 0, 4)      # 4 is sigma
1/(sqrt(2*pi)*4)*exp(-(log(2))^2/(2*16))/2     #checking

### Example 3.2 (Inverse transform method, continuous case)

    n <- 10000
    u <- runif(n)
    x <- u^(1/3)

    hist(x, prob = TRUE, main = bquote(f(x)==3*x^2)) #density histogram of sample
    y <- seq(0, 1, .01)
    lines(y, 3*y^2)    #density curve f(x)

    z=rbeta(n, 3, 1)
    qqplot(x, z)
    abline(a=0, b=1)

mean(x)
var(x)


### Generate a random sample with P(x=0)=0.2, P(x=1)=0.3, and P(x=2)=0.5
### First approach ##########

n=10000
x1=sample(0:2, size=n, replace=TRUE, prob=c(.2,.3,.5))
table(x1)/n

### Second approach ##########
n=10000
x2=numeric(n)
for(i in 1:n)
{
	u=runif(1)

	if (u <= .2) {x2[i]<- 0}
	else if(u<=.5) {x2[i]<- 1}
	else if(u<=1) {x2[i]<- 2}
}
table(x2)/n

### Third apparoach ######

n=1000
u=runif(n)
x3=0*as.integer(u<=0.2)+1*as.integer(u>0.2 & u <=0.5)+2*as.integer(u>0.5)
table(x3)/n

### Example 3.4 (Two point distribution)

    n <- 1000
    p <- 0.4
    u <- runif(n)
    x <- as.integer(u < p)   #(u <p) is a logical vector

    mean(x)
    var(x)

### Other ways to generate a Bernouli sample

x1=rbinom(n,1,pro=p)
x2=sample(0:1, size=n, replace=TRUE, prob=c(1-p,p))
table(x2)/1000

### Example 3.5 (Geometric distribution)

    n <- 100
    p <- 0.25
    u <- runif(n)
    k <- ceiling(log(1-u) / log(1-p)) - 1
    
    # more efficient
    k <- floor(log(u) / log(1-p))


### Example 3.6 (Logarithmic distribution)

    rlogarithmic <- function(n, theta) 
	{
        #returns a random logarithmic(theta) sample size n
        u <- runif(n)
        #set the initial length of cdf vector
        N <- ceiling(-16 / log10(theta))
        k <- 1:N
        a <- -1/log(1-theta)
        fk <- exp(log(a) + k * log(theta) - log(k))
        Fk <- cumsum(fk)
        x <- integer(n)
        for (i in 1:n) 
		{
            x[i] <-sum(u[i] > Fk) #F^{-1}(u)-1
            while (x[i] == N) 
			{
                		#if x==N we need to extend the cdf
                		#very unlikely because N is large
                		logf <- log(a) + (N+1)*log(theta) - log(N+1)
                		fk <- c(fk, exp(logf))
                		Fk <- c(Fk, Fk[N] + fk[N+1])
                		N <- N + 1
                		x[i] <- as.integer(sum(u[i] > Fk))
            	}
        	}
        x + 1
    }

    n <- 1000
    theta <- 0.5
    x <- rlogarithmic(n, theta)
    #compute density of logarithmic(theta) for comparison
    k <- sort(unique(x))
    p <- -1 / log(1 - theta) * theta^k / k
    se <- sqrt(p*(1-p)/n)   #standard error

    round(rbind(table(x)/n, p, se),3)

## notes
x <- pi * c(-1:1,10)
as.integer(x)
## in the above example, as.integer is not needed.

### Example 3.7 (Acceptance-rejection method) using C = 6

    n <- 1000
    k <- 0      #counter for accepted
    j <- 0      #iterations
    y <- numeric(n)

    while (k < n) 
   {
        u <- runif(1)
        j <- j + 1
        x <- runif(1)  #random variable from g
        if (x * (1-x) > u) 
       {
            #we accept x
            k <- k + 1
            y[k] <- x
        }
    }

    j

    #compare empirical and theoretical percentiles
    p <- seq(.1, .9, .1)
    Qhat <- quantile(y, p)   #quantiles of sample
    Q <- qbeta(p, 2, 2)      #theoretical quantiles
    se <- sqrt(p * (1-p) / (n * dbeta(Q, 2, 2)^2)) #see Ch. 2 page 38 formula 2.14
    round(rbind(Qhat, Q, se), 3)

### if we use c = 3/2, 
    n <- 1000
    k <- 0      #counter for accepted
    j <- 0      #iterations
    y <- numeric(n)

    while (k < n) 
   {
        u <- runif(1)
        j <- j + 1
        x <- runif(1)  #random variate from g
        if (4*x * (1-x) > u) 
       {
            #we accept x
            k <- k + 1
            y[k] <- x
        }
    }

    j

    #compare empirical and theoretical percentiles
    p <- seq(.1, .9, .1)
    Qhat <- quantile(y, p)   #quantiles of sample
    Q <- qbeta(p, 2, 2)      #theoretical quantiles
    se <- sqrt(p * (1-p) / (n * dbeta(Q, 2, 2)^2)) #see Ch. 2
    round(rbind(Qhat, Q, se), 3)

### Example 3.8 (Beta distribution)

    n <- 1000
    a <- 3
    b <- 2
    u <- rgamma(n, shape=a, rate=1)
    v <- rgamma(n, shape=b, rate=1)
    x <- u / (u + v)
    q=rbeta(n, a, b)
    qqplot(q, x, cex=0.25, xlab="Beta(3, 2)", ylab="Sample")
    abline(0, 1)

## notes
ppoints(4) # the same as  ppoints(1:4)
ppoints(10)
ppoints(10, a=1/2)
#Generates the sequence of probability points (1:m - a)/(m + (1-a)-a)

### Example 3.9 (Logarithmic distribution, version 2)

    n <- 1000
    theta <- 0.5
    u <- runif(n)  #generate logarithmic sample
    v <- runif(n)
    x <- floor(1 + log(v) / log(1 - (1 - theta)^u))
    k <- 1:max(x)  #calc. logarithmic probs.
    p <- -1 / log(1 - theta) * theta^k / k
    se <- sqrt(p*(1-p)/n)
    p.hat <- tabulate(x)/n

    print(round(rbind(p.hat, p, se), 3))

    # The following function is a simple replacement for
    # rlogarithmic in Example 3.6

    rlogarithmic <- function(n, theta) 
   {
        stopifnot(all(theta > 0 & theta < 1))
        th <- rep(theta, length=n)
        u <- runif(n)
        v <- runif(n)
        x <- floor(1 + log(v) / log(1 - (1 - th)^u))
        return(x)
    }


### Example 3.10 (Chisquare)

    n <- 100000
    nu <- 2
    X <- matrix(rnorm(n*nu), n, nu)^2 #matrix of sq. normals
    #sum the squared normals across each row: method 1
    y <- rowSums(X)
    # or to use
    y <- apply(X, MARGIN=1, FUN=sum)  #a vector length n
    mean(y)
    mean(y^2)
    var(y)	

### Mixture of normals (Convolutions and Mixtures)

    n <- 100000
    x1 <- rnorm(n, 0, 1)
    x2 <- rnorm(n, 3, 1)
    s <- x1 + x2              #the convolution
    u <- runif(n)
    k <- as.integer(u > 0.5)  #vector of 0's and 1's
    x <- k * x1 + (1-k) * x2  #the mixture

    par(mfcol=c(1,2))         #two graphs per page
    hist(s, prob=TRUE)
    hist(x, prob=TRUE)
    #par(mfcol=c(1,1))         #restore display


### Example 3.11 (Convolutions and mixtures)

    n <- 10000
    x1 <- rgamma(n, 2, 2)
    x2 <- rgamma(n, 2, 4)
    s <- x1 + x2              #the convolution
    u <- runif(n)
    k <- as.integer(u > 0.5)  #vector of 0's and 1's
    x <- k * x1 + (1-k) * x2  #the mixture

    par(mfcol=c(1,2))         #two graphs per page
    hist(s, prob=TRUE)
    hist(x, prob=TRUE)
    #par(mfcol=c(1,1))         #restore display


### Example 3.12 (Mixture of several gamma distributions)
    # density estimates are plotted

    n <- 1000
    k <- sample(1:5, size=n, replace=TRUE, prob=(1:5)/15)
    rate <- 1/k
    x <- rgamma(n, shape=3, rate=rate)

    #plot the density of the mixture
    #with the densities of the components
    plot(density(x), xlim=c(0,40), ylim=c(0,.3),
        lwd=3, xlab="x", main="")
    for (i in 1:5)
        lines(density(rgamma(n, 3, 1/i)))
## density estimates, not true densities

### Example 3.13 (Mixture of several gamma distributions)

    n <- 5000
    theta <- c(.1,.2,.2,.3,.2)
    lambda <- c(1,1.5,2,2.5,3)
    k <- sample(1:5, size=n, replace=TRUE, prob=theta)
    rate <- lambda[k]
    x <- rgamma(n, shape=3, rate=rate)

## lambda[k]is a vector, same length as k
k[1:3]
rate[1:3]

### Example 3.14 (Plot density of mixture)

    f <- function(x, lambda, theta) {
        #density of the mixture at the point x
        sum(dgamma(x, 3, lambda) * theta)
    }

    p <- c(.1,.2,.2,.3,.2)
    lambda <- c(1,1.5,2,2.5,3)

    x <- seq(0, 8, length=200)
    dim(x) <- length(x)  
    #need for apply, apply requires a dimension attribute for x

    #compute density of the mixture f(x) along x
    y <- apply(x, 1, f, lambda=lambda, theta=p)

    #plot the density of the mixture
    plot(x, y, type="l", ylim=c(0,.85), lwd=3, ylab="Density")

    for (j in 1:5) 
    {
        #add the j-th gamma density to the plot
        y <- apply(x, 1, dgamma, shape=3, rate=lambda[j])
        lines(x, y)
    }


### Example 3.15 (Poisson-Gamma mixture)

    #generate a Poisson-Gamma mixture
    n <- 10000
    r <- 4
    beta <- 3
    lambda <- rgamma(n, r, beta) #lambda is random

    #now supply the sample of lambda's as the Poisson mean
    x <- rpois(n, lambda)        #the mixture
    #compare with negative binomial
    mix <- tabulate(x+1) / n
    negbin <- round(dnbinom(0:max(x), r, beta/(1+beta)), 3)
    se <- sqrt(negbin * (1 - negbin) / n)

    round(rbind(mix, negbin, se), 3)

##
max(x)
table(x)/n
negbin
table(x+1)/n

### Example 3.16 (Spectral decomposition method)

## eigen function in R
X=cbind(c(1,-1),c(-1,1))
Y1=eigen(X)
Y2=eigen(X, symmetric = TRUE)
Y3=svd(X)

## If TRUE, the matrix is assumed to be symmetric
## and only its lower triangle is used. 
## If symmetric is not specified, the matrix is inspected for symmetry.

## Example 3.16 (Spectral decomposition method)

# mean and covariance parameters
    mu <- c(0, 0)
    Sigma <- matrix(c(1, .9, .9, 1), nrow = 2, ncol = 2)

    rmvn.eigen <-function(n, mu, Sigma) 
   {
        # generate n random vectors from MVN(mu, Sigma)
        # dimension is inferred from mu and Sigma
        d <- length(mu)
        ev <- eigen(Sigma, symmetric = TRUE)
        lambda <- ev$values
        V <- ev$vectors
        R <- V %*% diag(sqrt(lambda)) %*% t(V)
        Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
        X <- Z %*% R + matrix(mu, n, d, byrow = TRUE)
        X
    }

    # generate the sample
    X <- rmvn.eigen(1000, mu, Sigma)

    plot(X, xlab = "x", ylab = "y", pch = 20)
    print(colMeans(X))
    print(cor(X))


### Example 3.17 (SVD method)

    rmvn.svd <-function(n, mu, Sigma) 
   {
        # generate n random vectors from MVN(mu, Sigma)
        # dimension is inferred from mu and Sigma
        d <- length(mu)
        S <- svd(Sigma)
        R <- S$u %*% diag(sqrt(S$d)) %*% t(S$v) #sq. root Sigma
        Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
        X <- Z %*% R + matrix(mu, n, d, byrow=TRUE)
        X
    }
   
    X <- rmvn.svd(1000, mu, Sigma)

    plot(X, xlab = "x", ylab = "y", pch = 20)
    print(colMeans(X))
    print(cor(X))
 

### Example 3.18 (Choleski factorization method)

    rmvn.Choleski <-function(n, mu, Sigma) 
   {
        # generate n random vectors from MVN(mu, Sigma)
        # dimension is inferred from mu and Sigma
        d <- length(mu)
        Q <- chol(Sigma) # Choleski factorization of Sigma
        Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
        X <- Z %*% Q + matrix(mu, n, d, byrow=TRUE)
        X
    }
    
    # generating the samples according to the mean and covariance 
    # structure as the four-dimensional iris virginica data
    str(iris)
    names(iris)
    y <- subset(x=iris, Species=="virginica")[, 1:4]
    mu <- colMeans(y)
    Sigma <- cov(y)
    mu
    Sigma

    #now generate MVN data with this mean and covariance
    X <- rmvn.Choleski(200, mu, Sigma)
    pairs(X) # 2-D view of the bivariate distribution of each pair 
             # of marginal distribution.
    cov(X)

## we can see the structure of iris data
iris
str(iris)
y <- subset(x=iris, Species=="virginica")[, 1:4]
y <- subset(x=iris, Species=="virginica")

## we can check
colMeans(X)-mu
cov(X)-Sigma

### Example 3.19 (Comparing performance of MVN generators)

    install.packages("mvtnorm")
    library(MASS)
    library(mvtnorm)
    n <- 100          #sample size
    d <- 30           #dimension
    N <- 2000         #iterations
    mu <- numeric(d)

    set.seed(100)
    system.time(for (i in 1:N)
        rmvn.eigen(n, mu, cov(matrix(rnorm(n*d), n, d))))
    set.seed(100)
    system.time(for (i in 1:N)
        rmvn.svd(n, mu, cov(matrix(rnorm(n*d), n, d))))
    set.seed(100)
    system.time(for (i in 1:N)
        rmvn.Choleski(n, mu, cov(matrix(rnorm(n*d), n, d))))
    set.seed(100)
    system.time(for (i in 1:N)
        mvrnorm(n, mu, cov(matrix(rnorm(n*d), n, d))))
    set.seed(100)
    system.time(for (i in 1:N)
        rmvnorm(n, mu, cov(matrix(rnorm(n*d), n, d))))
    set.seed(100)
    system.time(for (i in 1:N)
        cov(matrix(rnorm(n*d), n, d)))

    detach(package:MASS)
    detach(package:mvtnorm)

### Example 3.20 (Multivariate normal mixture)

    library(MASS)  #for mvrnorm
    #ineffecient version loc.mix.0 with loops

    loc.mix.0 <- function(n, p, mu1, mu2, Sigma) 
   {
        #generate sample from BVN location mixture  ## not BVN
        X <- matrix(0, n, length(mu1))   ## error in the book: change 2 to length(mu1)

        for (i in 1:n) 
		{
            	k <- rbinom(1, size = 1, prob = p)
            	if (k) X[i,] <- mvrnorm(1, mu = mu1, Sigma) 
			else X[i,] <- mvrnorm(1, mu = mu2, Sigma)
            }
        return(X)
    }
    x <- loc.mix.0(1000, .5, rep(0, 4), 2:5, Sigma = diag(4))
    #more efficient version
    loc.mix <- function(n, p, mu1, mu2, Sigma) 
   {
        #generate sample from BVN location mixture
        n1 <- rbinom(1, size = n, prob = p)
        n2 <- n - n1
        x1 <- mvrnorm(n1, mu = mu1, Sigma)
        x2 <- mvrnorm(n2, mu = mu2, Sigma)
        X <- rbind(x1, x2)            #combine the samples
        return(X[sample(1:n), ])      #mix them
    }

    x <- loc.mix(1000, .5, rep(0, 4), 2:5, Sigma = diag(4))
    r <- range(x) * 1.2
    par(mfrow = c(2, 2))
    for (i in 1:4)
        hist(x[ , i], xlim = r, ylim = c(0, .3), freq = FALSE,
        main = "", breaks = seq(-5, 10, .5))        

    detach(package:MASS)
    par(mfrow = c(1, 1))
##
x=matrix(1:9,3,3)    
x[sample(1:3),]

