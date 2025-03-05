#Nicolas Hernandez
#Week 7 Assignment A

setwd("C:/Users/Nicolas/Desktop/Code/R/")

#sapply

df <-data.frame(
  x=c(1,2,3,4,5),
  y=c(1,4,9,16,25)
)


df

# f: if x<5. f(x) =-3, 5 and 20, f(x) = 0, grater than 20 == 1
#x = f(y)

f <- function(x){
  if(x<5)return(-3)
  if(x<21)return (0)
  return (1)
  
}

f(21)

#apply function to data frame, what function to apply to \
#sapply aplies function to a collumn

sapply(df$y,f)

df$z <- sapply(df$y,f)
View(df)

#Hypothesis testing and confidence intervals


mtcars$mpg
summary(mtcars$mpg)
sd(mtcars$mpg)

#Confidence intervaks
#say I want a 90% confidence interval of the mpg variable

p <- 99

#confi.int will give each of our values, print out to view values
#Block of codes in R
Q <- t.test(mtcars$mpg,conf.level = p/100,
       alternative = 'two.sided'
)
Q
sprintf('Confidence intervcal is between %f and %f',Q$conf.int[1],Q$conf.int[2])

#Hypothesis testing

#Test whether the mean mpg of all cars is less than 21 at significance level of 10%
#Left tail test

#Null hypothesis H0: mu=21
#Alternative Ha: mu<21

alpha <- 10
t.test(mtcars$mpg,
       mu = 21,
       conf.level = (100-alpha)/100, #do 1-alpha
       alternative = 'less'
)

#Since the p-value is not less than alpha which is .1,
# the data does not provude sufficient to show that mean mpg is 21.
       
#in a case where P is less than alpha
#it is sufficient to show that the mean mpg is less than 22


#Test whether the mean mpg is 20.5
#This one works
#1-p
alpha <- 10
t.test(mtcars$mpg,
       mu = 20.05,
       conf.level = .95, 
       alternative = 'two.sided'
)


# Is the horsepower of the car a good predictor of the number
# of miles per gallon the car can drive


Q <- lm(mtcars$mpg ~ mtcars$hp)
#another way.
lm(data = mtcars,mpg ~ hp)
summary(Q)

# this is te equation of the line 93, mpg = 30.09 + (-.06828)*hp


plot(mtcars$hp, mtcars$mpg)
abline(Q,col='red')







