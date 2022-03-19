# Reads in all MLB games all-time
mlb_data <- read_csv("https://projects.fivethirtyeight.com/mlb-api/mlb_elo.csv")

str(mlb_data)

# Check out random row
mlb_data[59000, ]

# Get only games that have already been played
game_data <- game_data %>% filter(date < Sys.Date())


# Who was the favorite for each game and did that team win?
favorite_win_prob_mlb <- mlb_data %>% 
  mutate(fav_538_won = ifelse(elo_prob1 > elo_prob2,
                              score1 > score2, 
                              score2 > score1),
         # Creates a TRUE/FALSE column to see if the favorite won
         fav_538_prob = ifelse(elo_prob1 > elo_prob2, 
                               elo_prob1,
                               elo_prob2), 
         # Creates a column that has the prob of the favorite
         favorite = ifelse(elo_prob1 > elo_prob2, 
                           team1,
                           team2)) %>% 
  # Creates a column that identifies the favorite
  select(season, date, team1, team2, favorite, fav_538_prob, fav_538_won)

# We want a line in our plot to show the average win prob of favorites for all time
# so we are going to get that value with this...
overall_win_prob_mlb <- mean(favorite_win_prob_mlb$fav_538_won)

# Plot the fraction of games the favorite has won over time
favorite_win_prob_mlb %>% group_by(season) %>% 
  summarize(fraction_fav_won = mean(fav_538_won)) %>% 
  ggplot(aes(season, fraction_fav_won)) +
  geom_hline(aes(yintercept = overall_win_prob_mlb), color = "red") +
  geom_line(show.legend = FALSE) +
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  labs(x ="Season", y = "Fraction of games favorite won", 
       title = "The 538 model does a better than average job at predicting NHL games", 
       subtitle = paste0("Since 1917, the favorite has won ", 
                         round(overall_win_prob_mlb*100, digits = 2), 
                         "% of their games"))


# Plot the observed vs. expected fraction of games won by the favorite. 

all_predicted_observed_mlb <- favorite_win_prob_mlb %>% 
  mutate(fav_538_prob = round(fav_538_prob, digits = 2)) %>% 
  group_by(fav_538_prob) %>% 
  summarize(games = n(), 
            wins = sum(fav_538_won), 
            observed = wins/games) 
# You make a games played (trials) so you can use it in the binomial distribution later
# You make wins and observed so you can plot them in relation to what it should
# be according to binomial distribution.


# We want to see the 95% confidence interval in the wins

binomial_fit_validation_mlb <- all_predicted_observed_mlb %>% 
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

binomial_fit_validation_mlb %>% 
  ggplot(aes(fav_538_prob, observed)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightgray")+
  geom_abline(aes(intercept= 0, slope=1)) +
  geom_point() +
  theme_classic()+
  coord_cartesian(ylim = c(0,1))



