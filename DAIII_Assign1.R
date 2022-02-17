---
title: "Assignment 1"
author: "Josh"
date: "2/14/2022"
output: html_document
---
## Question 1: Theoretical Questions about OLS

We will work a simple linear regression so that you understand all of the mechanics associated with regression analysis. In this example below, x is how much you `spend` on advertisement and y is the number of `sales` generated. For each part below, please, show your code.

```{r echo=TRUE, eval=TRUE}
sales <- c(9914,40487,54324,50044,34719,42551,94871,118914,158484,131348,78504,36284)
spend <- c(1000,4000,5000,4500,3000,4000,9000,11000,15000,12000,7000,3000)

mydata<-data.frame(sales,spend)
```

- Calculate average of your X variable and save the variable as `mean_spend`. Report the mean of X.
```{r echo = FALSE}
options(scipen = 100, digits = 4)
mean_spend <- mean(mydata$spend)
```
The mean of the spend variable is `r mean_spend`.

- Calculate the difference between each X and the average X and save it as a new column in your data called `dev_x`.
```{r echo = FALSE}
mydata$dev_x <- mydata$spend - mean_spend
```


- Square the differences (i.e. square each row of `dev_x`) and add it all up and save as a new variable called SSxx. (SS stands for Sum of Squares). Report SSxx.
```{r echo= FALSE}
SSxx <- sum(mydata$dev_x^2)
```
The sum of squared differences is `r SSxx`

- Calculate average of your Y variable and save the variable as `mean_sales`. Report the mean of Y.
```{r echo = FALSE}
mean_sales <- mean(mydata$sales)
```
The mean of sales is `r mean_sales`

- Calculate the difference between each Y and the average Y and save it as a new column in your data called `dev_y`.
```{r echo = FALSE}
mydata$dev_y <- mydata$sales - mean_sales
```


- Multiply the differences (`dev_y` * `dev_x`) and add them all together.  Call this new variable SSxy.
```{r echo = FALSE}
SSxy <- sum(mydata$dev_x * mydata$dev_y)
```

The SSxy is equal to `r SSxy`

- Calculate the regression slope as SSxy / SSxx. Report the slope
```{r echo = FALSE}
b1 <- SSxy / SSxx

```

The slope is equal to `r b1`


- Calculate the intercept by subtracting (SSxy / SSxx) * AVG(X) from AVG(Y). Report the intercept
```{r echo = FALSE}
b0 <- mean_sales - b1*mean_spend

```

- Using the regression intercept and slope you have found, create a new column called `y_hat` which will include the predicted values of y given the values of x in each row.
```{r echo = FALSE}
mydata$y_hat <- b0 + b1*mydata$spend
```



- Calculate a new column called resid_1 and calculate the difference between `sales` - `y_hat`.
```{r echo = FALSE}
mydata$resid_1 <- mydata$sales - mydata$y_hat
```


- Let's find the variance of the error term. The resid_1 column contains the residuals of the regression. We can calculate the variance by first squaring each residual, next summing up all of the values, and finally dividing by the number of observations minus the number of betas estimated, which is two in this case (constant and slope). Save the variance as var_resid. Report the square root of var_resid.
```{r echo = FALSE}
var_resid <- sum(mydata$resid_1 * mydata$resid_1) / (12-2)
```

The sqaure root of the var_resid is `r var_resid`

- Next, let's find the $R^2$. The residuals tell us what the regression line doesn't explain. Calculate the sum of squares for the `resid_1` column and save as SSresid. Calculate the sum of squares for the `sales` column. **Hint: you already have dev_y.**. Call this value SSyy. We can then calculate $R^2$ as 1 - SSresid/SSyy. Report the $R^2$.
```{r echo = FALSE}
r2 <- 1 - sum(mydata$resid_1 * mydata$resid_1) / sum(mydata$dev_y^2)

```

The $R^2$ is `r r2`

- Now let's calculate the coefficients and standard error using matrix operations.
```{r echo=TRUE, echo = FALSE , eval=FALSE}
# We need to include a column of ones for the constant
X<-matrix(c(rep(1,length(sales)),spend),12,2)
# This is equivalent to COV(X,Y)/VAR(X)
beta<-solve(t(X)%*%X)%*%t(X)%*%sales
# These are the predicted values of Y
y_hat <- X%*%beta
# These are the calculated residuals
resid_2 <- sales - y_hat
# We are calculating the variance of the residuals
var_reg <-(t(resid_2)%*%resid_2)/10
# Standard errors of the coefficients
b_se <- diag(solve(t(X)%*%X)*var_reg[1])
# check your work using lm
summary(lm(sales~spend))
```
You should check your work for question 1. Everyone should get this first question completely correct.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(stargazer)
```

```{r, results = 'asis', message=FALSE, warning = FALSE}
reg1 <- lm(sales ~ spend, data = mydata)
stargazer(reg1, type = 'html', omit.table.layout = 'n', single.row = TRUE)
```


## Question 2
In this question, you will use linear regression to study speeding tickets. Each question builds on the previous question. Your regressions should have more controls as you move through the assignment. Try to capture all of these regression in one nicely formatted table.

What determines how much drivers are fined if they are stopped for speeding? Do demographics like age, gender, and race matter? To answer this question, we'll investigate traffic stops and citations in Massachusetts using data from Makowsky and Stratmann (2009). Even though state law sets a formula for tickets based on how fast a person was driving, police officers in practice often deviate from the formula. An amount for the fine is given only for observations in which the police officer decided to assess a fine.

a) Plot a histogram of fines. Does it looked normally distributed or skewed? Keep in mind to remove missing fine amounts.
```{r echo = FALSE}
library(tidyverse)
library(skimr)
speeding_data <- read.csv("speeding_tickets_text.csv")
ggplot(speeding_data) +
  geom_histogram(mapping = aes(x = Amount), col = 'grey')+
  labs(title = 'Histogram of Speeding Tickets')
```

b) Estimate a simple linear regression model in which the ticket amount is the dependent variable as a function of age. Is age statistically significant? 
```{r echo = FALSE, results='asis'}
ticket_age_lm <- lm(Amount ~ Age, data = speeding_data)
stargazer(ticket_age_lm, type="html", omit.table.layout = 'n', single.row = TRUE)
```

Yes, age is statistically significant at the 1 percent level.
One additional year of age decreases fines by 29 cents in our model. 

c) What does it mean for a variable to be endogenous? Is it possibly the variable "age" is endogenous? Please explain your answer.

Yes, Age is endogenous, because it is capturing other factors beyond just the age of the driver. Age could be explaining experience with poor driving conditions and other drivers local driving habits.  

d) Are miles per hour over the speed limit correlated with age? Report the correlation coefficient.
```{r echo = FALSE, results='asis'}
MPHOver_age_lm <- lm(Amount ~ Age + MPHover,data = speeding_data)
stargazer(MPHOver_age_lm, type="html", omit.table.layout = 'n', single.row = TRUE)
```

The correlation coefficient between miles per hour over and age is `r cor(speeding_data$MPHover,speeding_data$Age)`.

e) Estimate the model from part b), also controlling for miles per hour over the speed limit. Explain what happens to the coefficient on age and why. 
```{r echo = FALSE, results='asis'}
age_MPHOver_amount_lm <- lm(Amount ~ Age + MPHover, data = speeding_data, na.action = na.omit)
stargazer(ticket_age_lm, MPHOver_age_lm, type="html", omit.table.layout = 'n', single.row = TRUE)
```

The coefficient of age is now different compared to the model that did not include miles per hour over the speed limit. Perhaps what is more important is that Age no longer has a statistically significant impact on fine amount even though it was before and the age coefficient was negative in the previous model and now it is positive with the inclusion of miles per hour over in the model. This may present a non-linear relationship that was not addressed in the model. 

f) Is the effect of age on fines linear or non-linear? Assess this question by estimating a model that includes both age and a quadratic age term. Also, control for _MPHover, Female, Black, and Hispanic_. Interpret the coefficients on the age variables. Remember it is non-linear so you can't just read the coefficient.
```{r echo = FALSE, results='asis'}
speeding_data$AgeQuad <- (speeding_data$Age)^2
part_f_lm<-lm(Amount ~ Age + AgeQuad + MPHover + Female + Black + Hispanic, data = speeding_data, na.action = na.omit)
stargazer(ticket_age_lm, MPHOver_age_lm, part_f_lm, type="html", omit.table.layout = 'n', single.row = TRUE)

```

The Age coefficient curve starts higher for fines and decreases to a smooth floor at which point it begins to increase in a curved pattern. Starting a slow decrease at younger ages reveals improved driving habits, then smoothing out at middle age, then slowly increasing fine amounts at older ages where drivers may be experiencing effects of old age (trouble with sight, reaction time, etc). This exhibits driver riskiness behavior at different ages. This is non-linear so we cannot interpret the straight figure. 

g) Plot the relationship between age and ticket amount using the coefficients on age that you found in step f. Calculate the fitted value for a white male with 0 _MPHover_ (probably not many people going zero miles over the speed limit got a ticket, but this simplifies calculations a lot) for ages equal to 20, 25, 30, 35, 40, and 70. Use R to calculate these values and plot them. 
```{r echo = FALSE, message=FALSE, warning=FALSE}
predict_age <- c(20,25,30,35,40,70)
predict_amount <- -.265*predict_age + .004*predict_age^2 + 9.867
predictions <- data.frame(predict_age, predict_amount)
ggplot(predictions)+ 
  geom_point(mapping = aes(x = predict_age, y = predict_amount))+
  geom_smooth(mapping = aes(x = predict_age, y = predict_amount))+
  labs(x = 'Age',
       y = 'Fine Amount',
       title = 'Ticket Amounts Predicted by Age')
```


h) Calculate the age that is associated with the lowest predicted fines. __Hint: You can use calculus or an algebraic formula used to find the minimum and maximum of quadratic functions.__ $$y =ax^2 + bx +c \\ x = -\frac{b}{2a}$$
```{r}
min_point <- -(-.265)/(2*.004)
min_point <- round(min_point, 0)

```

The age with the lowest predicted fines is `r min_point`. 

i) Do drivers from out of town  and out of state get treated differently? Do state police and local police treat nonlocals differently? Estimate a model that allows us to assess whether out of towners and out of staters are treated differently and whether state police respond differently to out of towners and out of staters. Interpret the coefficients on the relevant variables. __Hint: you have to do something more than just including the dummy variables.__
```{r echo = FALSE, results='asis'}
fullModelLm <- lm(Amount ~ ., data = speeding_data)
nonLocalLm <- lm(Amount ~ . + OutTown:StatePol + OutState:StatePol, data = speeding_data)
stargazer(fullModelLm, nonLocalLm,type = 'html', omit.table.layout = 'n', single.row = TRUE, model.names = TRUE, multicolumn = FALSE, object.names = TRUE)
```
Yes, to both questions. Both coefficients are positive here, meaning that as opposed to our baseline (locals, in state) fines increase by $6.17 dollars for non locals and .29 cents for out of state individuals when pulled over the state police. 

j) Test whether the two state police interaction terms are jointly significant. Briefly explain your results. __Hint: it says `jointly` so it is not a T-test.__
```{r echo = FALSE, results='asis'}
stargazer(anova(fullModelLm, nonLocalLm, test = "F"), type = 'html', summary = FALSE, digits = 1, flip = TRUE)
```
The F-Test p-value is less than 0.05, so we reject the null hypothesis that the two added terms for involving state police are insignificant when they pull over non locals and individuals from out of state. The state police interaction terms are significant, thus non local and out of state discrimination is present in this model. 

\newpage

Variable Name | Description
--------------|-----------------------
MPHover       | Miles per hour over the speed limit
Amount        | Assessed fine for the ticket
Age | Age of driver
Female | Equals 1 for women and 0 for men
Black | Equals 1 for African-American and 0 otherwise
Hispanic | Equals 1 for Hispanics and 0 otherwise
State Pol | Equals 1 if ticketing officer was state patrol officer and 0 otherwise
OutTown | Equals 1 if driver from out of town and 0 otherwise
OutState | Equals 1 if driver from out of state and 0 otherwise
