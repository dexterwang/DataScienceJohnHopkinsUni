question 1 

x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

sum(x*w)/sum(w)

[1] 0.1471429


question 2 

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

sum(y*x)/sum(x^2)



coef(lm(y ~ x - 1))
##      x 
## 0.8263
sum(y * x)/sum(x^2)
## [1] 0.8263



question 3

summary(lm(mtcars$mpg ~ mtcars$wt))


Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
mtcars$wt    -5.3445     0.5591  -9.559 1.29e-10 ***


question 4 

βˆ1=Cor(Y,X)Sd(Y)/Sd(X)

Sd(X)=Sd(Y)/2

Cor(Y,X)=0.5

βˆ1=1

question 5

Zi=Xi−Xˉ/s
Xˉ = 0
s = 1
Cor(Y,X) = 0.4
X=1.5

slope = Cor(Y,X)Sd(Y)/Sd(X) = 0.4
Y=(X * Slope) = 0.6


question 6

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)

xc <- (x-mean(x))/sd(x)
xc 



question 7 

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)


summary(lm(y ~ x))

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)    1.567      1.252   1.252    0.246
x             -1.713      2.105  -0.814    0.439


question 9 

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)

mean(x)
[1] 0.573

question 10 

β1 = Cor(Y,X)Sd(Y)/Sd(X)

γ1 = Cor(X,Y)Sd(X)/Sd(Y)

β1/γ1 = Cor(Y,X)Sd(Y)^2 / Cor(X,Y)Sd(X)^2         

Correct Response 
The β1=Cor(Y,X)SD(Y)/SD(X) and γ1=Cor(Y,X)SD(X)/SD(Y).

Thus the ratio is then Var(Y)/Var(X).
