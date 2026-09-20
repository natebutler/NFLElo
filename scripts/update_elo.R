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
  elo <- data.frame(team = teams, rating = mean_elo,
                    wins = 0, losses = 0, ties = 0)
  
  for (s in sort(unique(games$season))) {
    
    # Offseason regression (skip first season)
    if (s != min(games$season)) {
      elo$rating <- elo$rating + regress * (mean_elo - elo$rating)
    }
    
    # Reset records each season
    elo$wins <- 0
    elo$losses <- 0
    elo$ties <- 0
    
    season_games <- games %>% filter(season == s)
    
    for (w in sort(unique(season_games$week))) {
      week_games <- season_games %>% filter(week == w)
      
      for (i in seq_len(nrow(week_games))) {
        h <- which(elo$team == week_games$home_team[i])
        a <- which(elo$team == week_games$away_team[i])
        
        home_pts <- week_games$home_score[i]
        away_pts <- week_games$away_score[i]
        
        home_result <- if (home_pts > away_pts) 1 else if (home_pts < away_pts) 0 else 0.5
        
        expected_home <- 1 / (1 + 10^((elo$rating[a] - (elo$rating[h] + home_field)) / 400))
        
        elo$rating[h] <- elo$rating[h] + k * (home_result - expected_home)
        elo$rating[a] <- elo$rating[a] + k * ((1 - home_result) - (1 - expected_home))
        
        # Update records
        if (home_result == 1) {
          elo$wins[h]   <- elo$wins[h] + 1
          elo$losses[a] <- elo$losses[a] + 1
        } else if (home_result == 0) {
          elo$losses[h] <- elo$losses[h] + 1
          elo$wins[a]   <- elo$wins[a] + 1
        } else {
          elo$ties[h] <- elo$ties[h] + 1
          elo$ties[a] <- elo$ties[a] + 1
        }
      }
    }
  }
  
  elo %>%
    mutate(record = ifelse(ties > 0,
                           paste0(wins, "-", losses, "-", ties),
                           paste0(wins, "-", losses))) %>%
    select(team, rating, record) %>%
    arrange(desc(rating))
}

# --- Run and save ---
final_elo <- run_nfl_elo(season = c(2025,2026), k = 25, home_field = 40)

saveRDS(final_elo, "final_elo.rds")
