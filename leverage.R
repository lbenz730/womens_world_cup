source("helpers.R")
library(ggplot2)
library(tidyr)
library(forcats)

x <- read.csv("womens_soccer_scores.csv", as.is = T) %>%
  mutate("date" = as.Date(date))
fixtures <- read.csv("fixtures.csv", as.is = T)
x <- x %>% 
  bind_rows(x, 
            select(fixtures, date, team, opponent, team_score, location) %>%
              mutate("match_weight" = 4,
                     "date" = as.Date(date, "%m/%d/%y")) %>%
              rename("goals_scored" = team_score),
            select(fixtures, date, opponent, team, opp_score, location) %>%
              mutate("match_weight" = 4,
                     "date" = as.Date(date, "%m/%d/%y"),
                     "location" = case_when(
                       location == "H" ~ "A",
                       location == "N" ~ "N",
                       location == "A" ~ "H")) %>%
              rename("goals_scored" = opp_score,
                     "team" = opponent,
                     "opponent" = team))
### Fit Poisson Model
glm.futbol <- glm(goals_scored ~ team + opponent + location, 
                  family = "poisson",
                  data = x, 
                  weights = match_weight)


### Match Predictions
fixtures <- mutate(fixtures, "win" = NA, "tie" = NA, "loss" = NA,
                   "goal_diff" = team_score - opp_score,
                   "date" = as.Date(date, "%m/%d/%y"))
index <- !is.na(fixtures$goal_diff)
fixtures[index & fixtures$goal_diff > 0, c("win", "tie", "loss")] <- rep(c(1,0,0), rep(sum(index & fixtures$goal_diff > 0), 3))
fixtures[index & fixtures$goal_diff == 0, c("win", "tie", "loss")] <- rep(c(0,1,0), rep(sum(index & fixtures$goal_diff == 0), 3))
fixtures[index & fixtures$goal_diff < 0, c("win", "tie", "loss")] <- rep(c(0,0,1), rep(sum(index & fixtures$goal_diff < 0), 3))
fixtures$team_score[is.na(fixtures$team_score)]<- 
  suppressWarnings(predict(glm.futbol, newdata = fixtures[is.na(fixtures$team_score),], type = "response"))
fixtures$opp_score[is.na(fixtures$opp_score)]<- 
  suppressWarnings(predict(glm.futbol, newdata = invert(fixtures[is.na(fixtures$opp_score),]), type = "response"))
fixtures$goal_diff <- fixtures$team_score - fixtures$opp_score


sim_group_stage <- function(fixtures) {
  groups <- LETTERS[1:6]
  wc_sims <- data.frame("country" = unique(c(fixtures$team, fixtures$opponent)),
                        "group" = NA,
                        "first_in_group" = 0,
                        "second_in_group" = 0,
                        "third_in_group" = 0,
                        "r16" = 0)
  wc_sims$group <- unname(sapply(wc_sims$country, find_group))
  
  third_place <- data.frame("country" = rep(NA, 6),
                            "group" = groups,
                            "pts" = NA,
                            "goals_forced" = NA,
                            "goal_diff" = NA,
                            stringsAsFactors = F)
  winners <- rep(NA, 6)
  runners_up <- rep(NA, 6)
  third <- rep(NA, 4)
  
  nsims <- 500
  for(j in 1:nsims) {
    if(j %% 100 == 0) {
      cat("Sim", j, "of", nsims, "\n")
    }
    ### Simulate Group Stage
    for(i in 1:6) {
      sim_ladder <- sim_group_lev(groups[i])
      
      ### Aggregate E(pts), E(GF), and E(GD)
      index <- unname(sapply(sim_ladder$country, grep, wc_sims$country))
      wc_sims$exp_pts[index] <- wc_sims$exp_pts[index] + sim_ladder$pts/nsims
      wc_sims$exp_gf[index] <- wc_sims$exp_gf[index] + sim_ladder$goals_forced/nsims
      wc_sims$exp_gd[index] <- wc_sims$exp_gd[index] + sim_ladder$goal_diff/nsims
      
      ### Top 3 in Group
      index_1 <- wc_sims$country == sim_ladder$country[1]
      index_2 <- wc_sims$country == sim_ladder$country[2]
      index_3 <- wc_sims$country == sim_ladder$country[3]
      wc_sims$first_in_group[index_1] <- wc_sims$first_in_group[index_1] + 1/nsims
      wc_sims$second_in_group[index_2] <- wc_sims$second_in_group[index_2] + 1/nsims
      wc_sims$third_in_group[index_3] <- wc_sims$third_in_group[index_3] + 1/nsims
      winners[i] <- sim_ladder$country[1]
      runners_up[i] <- sim_ladder$country[2]
      third_place[i,-2] <- sim_ladder[3,]
    }
    
    ### Find 4 best 3rd place teams
    third <- order_thirds(third_place)
    ko_winners <- c(winners, runners_up, third)
    wc_sims$r16[wc_sims$country %in% ko_winners] <- 
      wc_sims$r16[wc_sims$country %in% ko_winners] + 1/nsims
    
  }
  return(wc_sims)
}


m <- min(which(is.na(fixtures$win)))

for(i in m:36) {
  cat("Game:", 1 - m + i, "of", 36-m+1, "\n")
  sim_team <- fixtures$team[i]
  sim_opp <- fixtures$opponent[i]
  for(j in 1:3) {
    if(j == 1) {
      fixtures[i, c("win", "tie", "loss")] <- c(1,0,0)
      tmp <- mutate(sim_group_stage(fixtures), "result" = paste(sim_team, "win"))
    } else if(j == 2) {
      fixtures[i, c("win", "tie", "loss")] <- c(0,1,0)
      tmp <- bind_rows(tmp, mutate(sim_group_stage(fixtures), "result" = "tie"))
      
    } else if(j == 3) {
      fixtures[i, c("win", "tie", "loss")] <- c(0,0,1)
      tmp <- bind_rows(tmp, mutate(sim_group_stage(fixtures), "result" = paste(sim_team, "loss")))
    }
  }
  
  fixtures[i, c("win", "tie", "loss")] <- rep(NA, 3)
  
  if(i == m) {
    lev <- filter(tmp, country %in% c(sim_team, sim_opp)) %>%
      mutate("result" = case_when(
        country == sim_team & result == paste(sim_team, "win") ~ "Win",
        country == sim_team & result == paste(sim_team, "loss") ~ "Loss",
        country == sim_opp & result == paste(sim_team, "win") ~ "Loss",
        country == sim_opp & result == paste(sim_team, "loss") ~ "Win",
        T ~ "Tie"),
        "match" = paste(sort(c(sim_team, sim_opp)), collapse = " vs. "))
  } else {
    lev <- lev %>% bind_rows(filter(tmp, country %in% c(sim_team, sim_opp)) %>%
                               mutate("result" = case_when(
                                 country == sim_team & result == paste(sim_team, "win") ~ "Win",
                                 country == sim_team & result == paste(sim_team, "loss") ~ "Loss",
                                 country == sim_opp & result == paste(sim_team, "win") ~ "Loss",
                                 country == sim_opp & result == paste(sim_team, "loss") ~ "Win",
                                 T ~ "Tie"),
                             "match" = paste(sort(c(sim_team, sim_opp)), collapse = " vs. ")))
    
  }
  
}

group_by(lev, country, match) %>%
  summarise("Win" = r16[result == "Win"] - r16[result == "Tie"],
            "Tie" = r16[result == "Tie"] - r16[result == "Loss"],
            "Loss" = r16[result == "Loss"]) %>%
  ungroup() %>%
  gather(result, r16, -country, -match) %>%
  mutate("result" = fct_relevel(result, "Win", "Tie", "Loss")) %>%
ggplot(aes(x = country, y = r16, group = result, alpha = result)) +
  geom_bar(aes(fill = country), stat = "identity", position = "stack") + 
  facet_wrap(~match, scales = "free_x") +
  scale_alpha_manual(values=c(0.3, 0.6, 1)) +
  scale_fill_manual(values = c("skyblue", "forestgreen", "yellow", "forestgreen",
                               "red", "red", "red", "red", "dodgerblue2", "black",
                               "blue", "forestgreen", "navy", "navy", "orange", 
                               "navy", "forestgreen", "red", "blue", "forestgreen", 
                               "red3", "dodgerblue", "red", "navy"),
                    guide = F) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Country", y = "Probability of Reaching Knockout Round",
       title = "Leverage for 2019 FIFA Women's World Cup",
       subtitle = "Remaining Group Stage Matches",
       alpha = "Result")
       
  

