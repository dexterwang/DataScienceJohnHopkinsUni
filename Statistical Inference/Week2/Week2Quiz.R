
#1

Var(X¯)=σ2/n

#2

pnorm(70, mean = 80, sd = 10)

#3

qnorm(0.95,mean=1100,sd=75)

#4

qnorm(0.95, mean = 1100, sd = 75/sqrt(100))


#5

pbinom(3, size = 5, prob = 0.5, lower.tail = FALSE)

p <- 0.5
n <- 5
quantile <- 3 # 4 or 5 out of 5, with lower
probPercentage1 <- round(pbinom(quantile, size=n, prob=p, lower.tail = FALSE) * 100)
probPercentage1
## [1] 19
Alternativly use a binomial coefficient and add the seperate chances.

\({n \choose k} = \frac{n!}{k!(n-k)!}\)

combinedProb <- p ^ n
choose4Prob <- choose(n, 4) * combinedProb
choose5Prob <- choose(n, 5) * combinedProb

probPercentage2 <- round((choose4Prob + choose5Prob) * 100)
probPercentage2

#6

pnorm(16, mean = 15, sd = 1) - pnorm(14, mean = 15, sd = 1)

μ <- 15
σ <- 10
n <- 100
SE <- σ/sqrt(n)

left <- 14
right <- 16

percentageLeft <- pnorm(left, mean = μ, sd = SE) * 100
percentageRight <- pnorm(right, mean = μ, sd = SE) * 100

probPercentage <- round(percentageRight - percentageLeft)
probPercentage


#7

Via the LLN it should be near .5.


#8

ppois(10, lambda = 15)

ppois(10,3*5)

