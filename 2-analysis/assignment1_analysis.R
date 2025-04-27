# clear environment 
rm(list = ls())
# load libraries 
library(tidyverse)
library(GGally)
library(ggplot2)

# read in cohort data 
d <- read.csv("1-data/raw-data/cohort.csv")

# class(d$smoke)
# class(d$female)
# class(d$age)
# class(d$cost)
# class(d$cardiac)

# check if any rows have missing values 
missing <- d[rowSums(is.na(d)) > 0,] # no missing values 

###############################################################################
# plot histogram of cost and cardiac events   
###############################################################################

# create categorical variable for cardiac status 
d <- d %>%
  mutate(cardiac_cat = case_when(cardiac == 0 ~ "No cardiac event",
                                 cardiac == 1 ~ "Cardiac event"))
                            
# create density plot of cost by cardiac event status 
plot <- ggplot(d, aes(x=cost, fill=cardiac_cat)) + 
  geom_density(alpha = 0.7) +
  ggtitle("Density plot of cost by cardiac event status") +
  labs(fill="") +
  xlab("Cost") + 
  ylab("Density") +
  theme_minimal()

# save density plot 
ggsave(filename = "density_plot_cost_cardiac.jpg", plot = plot, path = "3-output")

# remove categorical variable for cardiac event 
d <- d %>% select(-cardiac_cat)

###############################################################################
# create table to describe variables 
###############################################################################

# calculate stats for those with cardiac events 
d_cardiac <- d %>% filter(cardiac == 1) %>% 
  select(-cardiac) %>% 
  summarise(across(everything(), mean)) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean/% (In those with cardiac events)")
  
d_cardiac <- d_cardiac %>% mutate("N (Cardiac events)" = nrow(d %>% filter(cardiac == 1)))

# variables that should be percentages 
percentage_vars <- c("smoke", "female")

# convert Mean/% column values from mean to percentage if relevant
d_cardiac <- d_cardiac %>% 
  mutate("Mean/% (In those with cardiac events)" = ifelse(Variable %in% percentage_vars, 
                           paste0(sprintf("%.1f", `Mean/% (In those with cardiac events)` * 100), "%"), 
                           sprintf("%.1f", `Mean/% (In those with cardiac events)`)))

# calculate stats for those without cardiac events 
d_control <- d %>% filter(cardiac == 0) %>% 
  select(-cardiac) %>% 
  summarise(across(everything(), mean)) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean/% (In those without cardiac events)")

d_control <- d_control %>% mutate("N (No cardiac events)" = nrow(d %>% filter(cardiac == 0)))

# convert Mean/% column values from mean to percentage if relevant
d_control <- d_control %>% 
  mutate("Mean/% (In those without cardiac events)" = ifelse(Variable %in% percentage_vars, 
                                                          paste0(sprintf("%.1f", `Mean/% (In those without cardiac events)` * 100), "%"), 
                                                          sprintf("%.1f", `Mean/% (In those without cardiac events)`)))
# join columns 
stats <- d_control %>% 
  left_join(d_cardiac, by = "Variable")

# label variables 
labels <- c("Smoker", "Is female", "Age in years", "Cost of treatment (in dollars)")

stats <- stats %>%
  mutate(Variable = labels) 

# save table 1 
write.csv(stats, "~/Documents/BIOMEDIN251/Assignment2/3-output/table1.csv", row.names = FALSE)

###############################################################################
# regression analysis  
###############################################################################

# run logistic regression 
glm.fit <- glm(cardiac ~ smoke + female + age, data = d, family = "binomial")

summary(glm.fit)

# print OR and CI for cardiac events by smoking status 
print(OR_smoke <- exp(glm.fit$coefficients["smoke"]))

print(CI <- exp(confint(glm.fit))) 


