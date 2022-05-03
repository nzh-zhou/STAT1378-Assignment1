## Exercise 1
# Part 1
library(tidyverse)
X <- read_csv("A3_Ex1.csv")

# Part 2
str(X)

# Part 3
sum(X$expenditure)

# Part 4
X %>% filter(month == "Mar") %>% #subset required cases
  select(expenditure) %>% #select required column (returns a numeric vector)
  sum()

# Part 5
X %>% filter(month == "Mar", weather == "cloudy") %>% #subset required cases
  select(expenditure) %>% #select required column (returns a numeric vector)
  sum()

# Part 6
X %>% group_by(month) %>% 
  summarise(sum = sum(expenditure)) %>% #sum expenditure grouped by month
  filter(sum == max(sum)) #extract the month with the highest expenditure