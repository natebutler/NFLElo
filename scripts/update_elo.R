library(nflfastR)
library(dplyr)

# --- Your Elo function ---
run_nfl_elo <- function(season = 2025, k = 20, home_field = 65) {
  
  games <- load_schedules(season) %>%
    filter(game_type == "REG", !is.na(home_score)) %>%
    select(week, home_team, away_team, home_score, away_score) %>%
    arrange(week)
  
  teams <- unique(c(games$home_team, games$away_team))
  elo <- data.frame(team = teams, rating = 1500)
  
  for (w in sort(unique(games$week))) {
    week_games <- games %>% filter(week == w)
    
    for (i in 1:nrow(week_games)) {
      
      home <- week_games$home_team[i]
      away <- week_games$away_team[i]
      
      r_home <- elo$rating[elo$team == home]
      r_away <- elo$rating[elo$team == away]
      
      home_win <- ifelse(week_games$home_score[i] > week_games$away_score[i], 1, 0)
      
      expected_home <- 1 / (1 + 10^((r_away - (r_home + home_field)) / 400))
      expected_away <- 1 - expected_home
      
      elo$rating[elo$team == home] <- r_home + k * (home_win - expected_home)
      elo$rating[elo$team == away] <- r_away + k * ((1 - home_win) - expected_away)
    }
  }
  
  elo %>% arrange(desc(rating))
}

# --- Run and save ---
final_elo <- run_nfl_elo(season = 2025, k = 25, home_field = 40)

saveRDS(final_elo, "final_elo.rds")