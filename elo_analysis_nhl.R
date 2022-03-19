# *************************************************************
# This script runs the analysis to evaluate the 538 NHL Elo 
# model. 
#
#
#
# *************************************************************

library(pacman)
p_load(tidyverse, skimr, scales, rmarkdown, magrittr, lubridate, janitor, htmltab, ggrepel, viridis,
       ggthemes, knitr, rvest, reactable, stringr.plus, htmltools, ggridges, glue)
options(scipen=999)

# Reads in all NHL games all-time
game_data <- read_csv("https://projects.fivethirtyeight.com/nhl-api/nhl_elo.csv")
                    
str(game_data)

# Check out random row
game_data[59000, ]

# Get only games that have already been played
game_data <- game_data %>% filter(date < Sys.Date())


# Who was the favorite for each game and did that team win?
favorite_win_prob <- game_data %>% 
  mutate(fav_538_won = ifelse(home_team_winprob > away_team_winprob,
                              home_team_score > away_team_score, 
                              away_team_score > home_team_score),
         # Creates a TRUE/FALSE column to see if the favorite won
         fav_538_prob = ifelse(home_team_winprob > away_team_winprob, 
                               home_team_winprob,
                               away_team_winprob), 
         # Creates a column that has the prob of the favorite
         favorite = ifelse(home_team_winprob > away_team_winprob, 
                           home_team_abbr,
                           away_team_abbr)) %>% 
  # Creates a column that identifies the favorite
  select(season, date, home_team, away_team, favorite, fav_538_prob, fav_538_won)

# We want a line in our plot to show the average win prob of favorites for all time
# so we are going to get that value with this...
overall_win_prob <- mean(favorite_win_prob$fav_538_won)

# Plot the fraction of games the favorite has won over time
favorite_win_prob %>% group_by(season) %>% 
  summarize(fraction_fav_won = mean(fav_538_won)) %>% 
  ggplot(aes(season, fraction_fav_won)) +
  geom_hline(aes(yintercept = overall_win_prob), color = "red") +
  geom_line(show.legend = FALSE) +
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  labs(x ="Season", y = "Fraction of games favorite won", 
       title = "The 538 model does a better than average job at predicting NHL games", 
       subtitle = paste0("Since 1917, the favorite has won ", 
                        round(overall_win_prob*100, digits = 2), 
                        "% of their games"))


# Plot the observed vs. expected fraction of games won by the favorite. 

all_predicted_observed <- favorite_win_prob %>% 
  mutate(fav_538_prob = round(fav_538_prob, digits = 2)) %>% 
  group_by(fav_538_prob) %>% 
  summarize(games = n(), 
            wins = sum(fav_538_won), 
            observed = wins/games) 
# You make a games played (trials) so you can use it in the binomial distribution later
# You make wins and observed so you can plot them in relation to what it should
# be according to binomial distribution.


# We want to see the 95% confidence interval in the wins
library(broom)

binomial_fit_validation <- all_predicted_observed %>% 
  mutate(prob = fav_538_prob) %>% 
  # Need to put prob in the nested tibbled rows
  group_by(fav_538_prob) %>% 
  nest() %>% # This puts each row in its own tibble
  mutate(binomial = map(data, function(df)
                        tidy(binom.test(x = as.integer(df$games * df$prob), 
                                        n = df$games, 
                                        p = df$prob)
                             )
                        )
         ) %>% 
  unnest() %>% 
  select(fav_538_prob, games, wins, observed, conf.low, conf.high)

binomial_fit_validation %>% 
  ggplot(aes(fav_538_prob, observed)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightgray")+
  geom_abline(aes(intercept= 0, slope=1)) +
  geom_point() +
  theme_classic()+
  coord_cartesian(ylim = c(0,1))
  


# Reads in all NHL games for just 2021-2022 season
game_data_2122 <- read_csv("https://projects.fivethirtyeight.com/nhl-api/nhl_elo_latest.csv")

str(game_data_2122)

# Check out random row
game_data_2122[1000, ]

# Get only games that have already been played
game_data_2122 <- game_data_2122 %>% filter(date < Sys.Date())


# Who was the favorite for each game and did that team win?
favorite_win_prob_2122 <- game_data_2122 %>% 
  mutate(fav_538_won = ifelse(home_team_winprob > away_team_winprob,
                              home_team_score > away_team_score, 
                              away_team_score > home_team_score),
         # Creates a TRUE/FALSE column to see if the favorite won
         fav_538_prob = ifelse(home_team_winprob > away_team_winprob, 
                               home_team_winprob,
                               away_team_winprob), 
         # Creates a column that has the prob of the favorite
         favorite = ifelse(home_team_winprob > away_team_winprob, 
                           home_team_abbr,
                           away_team_abbr)) %>% 
  # Creates a column that identifies the favorite
  select(season, date, home_team, away_team, favorite, fav_538_prob, fav_538_won)

# We want a line in our plot to show the average win prob of favorites for all time
# so we are going to get that value with this...
overall_win_prob <- mean(favorite_win_prob_2122$fav_538_won)

# Plot the fraction of games the favorite has won over time
favorite_win_prob_2122 %>% group_by(date) %>% 
  summarize(fraction_fav_won = mean(fav_538_won)) %>% 
  ggplot(aes(date, fraction_fav_won)) +
  geom_hline(aes(yintercept = overall_win_prob), color = "red") +
  geom_line(show.legend = FALSE) +
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  labs(x ="Season", y = "Fraction of games favorite won", 
       title = "The 538 model does a better than average job at predicting NHL games", 
       subtitle = paste0("Since 1917, the favorite has won ", 
                         round(overall_win_prob*100, digits = 2), 
                         "% of their games"))


# Plot the observed vs. expected fraction of games won by the favorite. 

all_predicted_observed_2122 <- favorite_win_prob_2122 %>% 
  mutate(fav_538_prob = round(fav_538_prob, digits = 2)) %>% 
  group_by(fav_538_prob) %>% 
  summarize(games = n(), 
            wins = sum(fav_538_won), 
            observed = wins/games) 
# You make a games played (trials) so you can use it in the binomial distribution later
# You make wins and observed so you can plot them in relation to what it should
# be according to binomial distribution.


# We want to see the 95% confidence interval in the wins

binomial_fit_validation_2122 <- all_predicted_observed_2122 %>% 
  mutate(prob = fav_538_prob) %>% 
  # Need to put prob in the nested tibbled rows
  group_by(fav_538_prob) %>% 
  nest() %>% # This puts each row in its own tibble
  mutate(binomial = map(data, function(df)
    tidy(binom.test(x = as.integer(df$games * df$prob), 
                    n = df$games, 
                    p = df$prob)
    )
  )
  ) %>% 
  unnest() %>% 
select(fav_538_prob, games, wins, observed, conf.low, conf.high)

binomial_fit_validation_2122 %>% 
  ggplot(aes(fav_538_prob, observed)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightgray")+
  geom_abline(aes(intercept= 0, slope=1)) +
  geom_point() +
  theme_classic()+
  coord_cartesian(ylim = c(0,1))
