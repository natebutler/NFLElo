library(nflfastR)
library(dplyr)

run_nfl_elo <- function(seasons = c(2025, 2026),
                        k = 20,
                        home_field = 65,
                        regress = 1/3,     
                        mean_elo = 1500) {
  
  games <- load_schedules(seasons) %>%
    filter(game_type == "REG", !is.na(home_score)) %>%
    select(season, week, home_team, away_team, home_score, away_score) %>%
    arrange(season, week)
  
  teams <- unique(c(games$home_team, games$away_team))
  elo <- data.frame(team = teams, rating = mean_elo)
  
  for (s in sort(unique(games$season))) {
    
    # Offseason regression: skip the first season in the data
    if (s != min(games$season)) {
      elo$rating <- elo$rating + regress * (mean_elo - elo$rating)
    }
    
    season_games <- games %>% filter(season == s)
    
    for (w in sort(unique(season_games$week))) {
      week_games <- season_games %>% filter(week == w)
      
      for (i in seq_len(nrow(week_games))) {
        home <- week_games$home_team[i]
        away <- week_games$away_team[i]
        
        r_home <- elo$rating[elo$team == home]
        r_away <- elo$rating[elo$team == away]
        
        home_win <- as.numeric(week_games$home_score[i] > week_games$away_score[i])
        
        expected_home <- 1 / (1 + 10^((r_away - (r_home + home_field)) / 400))
        
        elo$rating[elo$team == home] <- r_home + k * (home_win - expected_home)
        elo$rating[elo$team == away] <- r_away + k * ((1 - home_win) - (1 - expected_home))
      }
    }
  }
  
  elo %>% arrange(desc(rating))
}

# --- Run and save ---
final_elo <- run_nfl_elo(season = c(2025, 2026), k = 25, home_field = 40)

saveRDS(final_elo, "final_elo.rds")
