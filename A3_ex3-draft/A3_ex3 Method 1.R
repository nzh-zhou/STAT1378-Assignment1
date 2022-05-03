## Exercise 3
# Setup
library(glue)
library(patchwork)
library(tidyverse)
signif <- read_csv("A3_Ex3_signif.csv")
not_signif <- read_csv("A3_Ex3_not_signif.csv")

# The function `input` inputs a data frame with columns X and Y into an
# enclosing environment and exports/saves a copy of the necessary 
# functions for testing linear regression into the Global Environment.
input <- function(df) {
  
  # The function `functions` returns a list of functions for testing
  # linear regression
  functions <- function() {
    
    # Linear regression modeling
    lm1 <- lm(Y ~ X, df)
    
    # Functions for testing linear regression
    # Part 1
    hypothesis <- function() {
      glue("testing H_0: beta = 0 against H_1: beta != 0")
    }
    
    # Part 2
    assumptions <- function() {
      (ggplot(df, aes(X, Y)) + geom_point(size = 0.5) + 
         geom_smooth(method = lm, formula = y ~ x) + ggtitle("X vs. Y") +
         ggplot(data.frame(x = lm1$fitted.values, y = lm1$residuals), 
                aes(x, y)) + geom_point(size = 0.5) + 
         geom_smooth(method = lm, se = FALSE, formula = y ~ x) + 
         labs(title = "Regression residuals vs. fitted values", 
              x = "fitted values", y = "residuals")) /
        ggplot(data.frame(x = lm1$residuals), aes(x)) +
        geom_histogram(bins = 30) + 
        labs(title = "Histogram of regression residuals", 
             x = "residual bins")
    }
    
    # Part 3
    fit <- function() {
      x <- summary(lm1)$coefficients[2, 4]
      cat(glue("beta_hat = {summary(lm1)$coefficients[2, 1]}"), 
          glue("95% confindence interval for beta: [{confint(lm1)[2, 1]}, {confint(lm1)[2, 2]}]"), 
          glue("t value: {summary(lm1)$coefficients[2, 3]}"), 
          glue("degrees of freedom: {summary(lm1)$df[2]}"), 
          glue("p-value: {summary(lm1)$coefficients[2, 4]}"), sep = "\n")
      class(x) <- "mylm"
      invisible(x)
    }
    
    # Part 4
    decision.mylm <- function(fit) {
      glue(c("H_0 is retained at a 5% significance level", 
             "H_0 is rejected at a 5% significance level")[as.numeric(fit < 0.05) + 1])
    }
    decision.default <- function(fit) {
      stop("Class of argument is not 'mylm'. Have you tried turning it off and on again?")
    }
    decision <- function(fit) {
      UseMethod("decision")
    }
    
    # Part 5
    conclusion.mylm <- function(fit) {
      glue(c("Conclusion: since the p-value is greater than 0.05, at a 5% significance level, the null hypothesis is retained since there is insufficient evidence to conclude that there is a linear relationship between variables X and Y", 
             "Conclusion: since the p-value is less than 0.05, at a 5% significance level, there is sufficient evidence to reject the null hypothesis in favour of the alternative and conclude that there is a linear relationship between variables X and Y")[as.numeric(fit < 0.05) + 1])
    }
    conclusion.default <- function(fit) {
      stop("Class of argument is not 'mylm'. Maybe try using more brain cells.")
    }
    conclusion <- function(fit) {
      UseMethod("conclusion")
    }
    
    # Full test
    mytest <- function() {
      cat(hypothesis(), "\n \n", sep = "")
      print(invisible(assumptions()))
      f <- fit()
      cat("\n", decision(f), "\n \n",  conclusion(f), sep = "")
    }
    
    # List containing all the required functions
    list(hypothesis = hypothesis, assumptions = assumptions, fit = fit, 
         decision = decision, conclusion = conclusion, mytest = mytest, 
         decision.mylm = decision.mylm, 
         decision.default = decision.default, 
         conclusion.mylm = conclusion.mylm, 
         conclusion.default = conclusion.default)
  }
  
  # Extract the functions into the Global Environment
  functions <- functions()
  for (i in names(functions)) {
    assign(i, functions[[i]], envir = globalenv())
  }
}

# Input `signif` data frame into the enclosing environment
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

# Input `not_signif` data frame into the enclosing environment
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