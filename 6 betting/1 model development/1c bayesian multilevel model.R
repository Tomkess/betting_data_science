library(tidyverse)
library(brms)

load("C:/Users/Peter/Desktop/ds_projects/betting_data_science/6 betting/1 model development/preliminary_data/1b elastic net modelling.RData")

# ----- Setting Priors -----
prior_norm <- prior(normal(0, 0.5), class = "b")

# ----- Model Setting -----
model_bhm <- 
  brm(n_goals ~ is_home + league +
        r_ah_advantage_last_10 + n_shots_ontarget_last_10 + n_goals_last_20 +
        n_goals_last_10 + r_ah_advantage_season_last_10 + 
        r_team_odds_last_20 + r_team_odds_last_10 + match_results_last_20 + 
        r_ah_advantage_last_20 + r_season_strength_ah_last_10 + 
        (1|league) + (1|team),
      data = train_data_bhm %>%
        mutate(is_home = ifelse(is_home == 1, "yes", "no")) %>%
        rename_all(~stringr::str_replace_all(., "__", "_")) %>%
        as.data.frame(), 
      cores = 3,
      chains = 3,
      family = hurdle_poisson(),
      prior = prior_norm,
      control = list(adapt_delta = 0.99))

# ----- Get Summary of Model -----
summary(model_bhm)

# ----- Get Predictions of Model -----
train_data_pred <- 
  predict(model_bhm, 
          newdata = prep(rec_normalization, train_data_bhm) %>% 
            bake(new_data = train_data_bhm) %>%
            mutate(is_home = ifelse(is_home == 1, "yes", "no")) %>%
            rename_all(~stringr::str_replace_all(., "__", "_")) %>%
            as.data.frame(), 
          allow_new_levels = T)

season_1819_pred <- 
  predict(model_bhm, 
          newdata = prep(rec_normalization, season_1819_bhm) %>%
            bake(new_data = season_1819_bhm) %>%
            mutate(is_home = ifelse(is_home == 1, "yes", "no")) %>%
            rename_all(~stringr::str_replace_all(., "__", "_")) %>%
            as.data.frame(), 
          allow_new_levels = T)

season_1920_pred <- 
  predict(model_bhm, 
          newdata = prep(rec_normalization, season_1920_bhm) %>%
            bake(new_data = season_1920_bhm) %>%
            mutate(is_home = ifelse(is_home == 1, "yes", "no")) %>%
            rename_all(~stringr::str_replace_all(., "__", "_")) %>%
            as.data.frame(), 
          allow_new_levels = T)


save.image("C:/Users/Peter/Desktop/ds_projects/betting_data_science/6 betting/1 model development/preliminary_data/1c bayesian multilevel model.RData")
