## Exercise 3
# Setup
library(glue)
library(patchwork)
library(tidyverse)
signif <- read_csv("A3_Ex3_signif.csv")
not_signif <- read_csv("A3_Ex3_not_signif.csv")

# The function `extract` makes a copy of all the functions that it 
# encloses in its runtime environment into the Global Environment
extract <- function() {
  
  # Function for inputting a data frame for linear regression modeling
  input <- function(df, X = "X", Y = "Y") {
    X <- df[[X]]
    Y <- df[[Y]]
    assign("lm1", lm(Y ~ X), envir = parent.env(environment())) #the linear model is saved as the object `lm1` in the environment enclosed by function `extract`
    assign("df", tibble(X = X, Y = Y), envir = parent.env(environment())) #the tibble containing the X and Y values for linear regression analysis is saved as the object `df` in the environment enclosed by function `extract`
  }
  
  # Functions for testing linear regression
  # Part 1
  hypothesis <- function() {
    glue("testing H_0: beta = 0 against H_1: beta != 0")
  }
  
  # Part 2
  assumptions <- function() {
    #y vs. x
    (ggplot(df, aes(X, Y)) + geom_point(size = 0.5) + 
       geom_smooth(method = lm, formula = y ~ x) + ggtitle("y vs. x") +
    #e_i vs y_hat
    ggplot(data.frame(x = lm1$fitted.values, y = lm1$residuals), 
           aes(x, y)) + geom_point(size = 0.5) + 
       geom_smooth(method = lm, se = FALSE, formula = y ~ x) + 
       labs(title = "Regression residuals vs. fitted values", 
            x = "fitted values", y = "residuals")) /
    #histogram of e_i
    ggplot(data.frame(x = lm1$residuals), aes(x)) +
      geom_histogram(bins = 30) + 
      labs(title = "Histogram of regression residuals", 
           x = "residual bins")
  }
  
  # Part 3
  fit <- function() {
    x <- summary(lm1)$coefficients[2, 4] #p-value is saved as `x`
    #report important values
    cat(glue("beta_hat = {summary(lm1)$coefficients[2, 1]}"), 
        glue("95% confindence interval for beta: [{confint(lm1)[2, 1]}, {confint(lm1)[2, 2]}]"), 
        glue("t value: {summary(lm1)$coefficients[2, 3]}"), 
        glue("degrees of freedom: {summary(lm1)$df[2]}"), 
        glue("p-value: {summary(lm1)$coefficients[2, 4]}"), sep = "\n")
    class(x) <- "mylm"
    invisible(x) #return p-value without printing so that it can be used in parts 4 and 5
  }
  
  # Part 4
  decision.mylm <- function(fit) {
    #report the decision of the test using the p-value
    glue(c("H_0 is retained at a 5% significance level", 
           "H_0 is rejected at a 5% significance level")[as.numeric(fit < 0.05) + 1])
  }
  decision.default <- function(fit) {
    #error message
    stop("Class of argument is not 'mylm'. Have you tried turning it off and on again?")
  }
  decision <- function(fit) {
    UseMethod("decision")
  }
  
  # Part 5
  conclusion.mylm <- function(fit) {
    #report the conclusion of the test using the p-value
    glue(c("Conclusion: since the p-value is greater than 0.05, at a 5% significance level, the null hypothesis is retained since there is insufficient evidence to conclude that there is a linear relationship between variables X and Y", 
           "Conclusion: since the p-value is less than 0.05, at a 5% significance level, there is sufficient evidence to reject the null hypothesis in favour of the alternative and conclude that there is a linear relationship between variables X and Y")[as.numeric(fit < 0.05) + 1])
  }
  conclusion.default <- function(fit) {
    #error message
    stop("Class of argument is not 'mylm'. Maybe try using more brain cells.")
  }
  conclusion <- function(fit) {
    UseMethod("conclusion")
  }
  
  # Full test
  mytest <- function() {
    cat(hypothesis(), "\n \n", sep = "")
    print(assumptions())
    f <- fit()
    cat("\n", decision(f), "\n \n",  conclusion(f), sep = "")
  }
  
  # List containing all the required functions
  functions <- list(input = input, hypothesis = hypothesis, 
                    assumptions = assumptions, fit = fit, 
                    decision = decision, conclusion = conclusion, 
                    mytest = mytest, decision.mylm = decision.mylm, 
                    decision.default = decision.default, 
                    conclusion.mylm = conclusion.mylm, 
                    conclusion.default = conclusion.default)
  
  # Extract all functions into the Global Environment
  for (i in names(functions)) {
    assign(i, functions[[i]], envir = globalenv())
  }
}

# Extract functions
extract()

# Input `signif` data frame into the environment enclosed by the function `extract`
input(signif)
# Part 1
hypothesis()
# Part 2
assumptions()
# Part 3
f <- fit()
# Part 4
decision(f)
# Part 5
conclusion(f)
# Full test
mytest()

# Input `not_signif` data frame into the environment enclosed by the function `extract`
input(not_signif)
# Part 1
hypothesis()
# Part 2
assumptions()
# Part 3
f <- fit()
# Part 4
decision(f)
# Part 5
conclusion(f)
# Full test
mytest()
