library(glmnet)
library(pROC)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(data.table)
library(doMC)

registerDoMC(cores = 3)
cv <- cv.glmnet(train_woe %>% 
                  select(-match_date, -team, -is_home, 
                         -n_goals, -match_result) %>%
                  as.matrix(),
                
                train_woe %>% 
                  mutate(target_goals = ifelse(n_goals >= 2.5, 
                                               "Over 2.5", "Under 2.5")) %>%
                  mutate_if(is.character, as.factor) %>% 
                  select(target_goals) %>% as.matrix(),
                
                family = "binomial", 
                nfold = 50,
                type.measure = "auc", 
                parallel = TRUE,
                lambda = NULL,
                gamma = c(0, 0.25, 0.5, 0.75, 1))

lambda_opt <- cv$lambda[cv$cvm == max(cv$cvm)]
md3 <- glmnet(mdlX, mdlY, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)

coef(md3)
roc(newY, as.numeric(predict(md3, newX, type = "response")))

 
table(train_woe %>% 
  mutate(target_goals = ifelse(n_goals >= 2.5, 
                               "Over 2.5", "Under 2.5")) %>%
  mutate_if(is.character, as.factor) %>% 
  select(target_goals) %>% as.data.frame())
