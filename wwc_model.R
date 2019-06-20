library(dplyr)
library(XML)
library(RCurl)
source("helpers.R")

### Read in Women's Soccer Scores
x <- bind_rows(
  read.csv("https://raw.githubusercontent.com/octonion/soccer/master/fifa/csv/results_women_2013.csv", 
           as.is = T, header = F),
  read.csv("https://raw.githubusercontent.com/octonion/soccer/master/fifa/csv/results_women_2014.csv", 
           as.is = T, header = F),
  read.csv("https://raw.githubusercontent.com/octonion/soccer/master/fifa/csv/results_women_2015.csv", 
           as.is = T, header = F),
  read.csv("https://raw.githubusercontent.com/octonion/soccer/master/fifa/csv/results_women_2016.csv", 
           as.is = T, header = F),
  read.csv("https://raw.githubusercontent.com/octonion/soccer/master/fifa/csv/results_women_2017.csv", 
           as.is = T, header = F))

### Clean Dataset
x <- x[,c(4, 8, 12, 13, 15, 16, 18, 19)]
names(x) <- c("competition", "date", "team_id", "team", 
              "opp_id", "opponent", "score", "location_played")

x <- filter(x, !grepl("Futsal", competition) & !grepl("Beach", competition)) %>%
  mutate("location" = case_when(
    gsub("^.*\\s,\\s", "", location_played) == team ~ "H",
    gsub("^.*\\s,\\s", "", location_played) == opponent ~ "A",
    T ~ "N"),
    "score" = gsub("\\(.*", "", score),
    "team_score" = as.numeric(gsub("-.*", "", score)),
    "opp_score" = as.numeric(gsub("AET", "", gsub(".*-", "", score))), 
    "date" = as.Date(gsub("[A-z]", "", date), "%d/%m/%Y"))

### Historical FIFA Rankings (to get Top-10)
dates <- c("20190329", "20181207", "20180928", "20180622",
           "20180323", "20171215", "20170901", "20170623",
           "20170324", "20161223", "20160826","20160624", 
           "20160325", "20151218", "20150925", "20150710",
           "20150327", "20141219", "20140919", "20140620",
           "20140328", "20131213", "20130802", "20130621",
           "20130322", "20121207")

for(d in dates) {           
  url <- paste0("https://www.fifa.com/fifa-world-ranking/ranking-table/women/rank/ranking_", d, "/")
  y <- as.data.frame(readHTMLTable(getURL(url))[[1]])
  y <- y[1:10,]
  df <- data.frame("team" = stripwhite(substring(y[,2], 1, nchar(as.character(y[,2])) - 3)),
                   "rank" = 1:10,
                   date = as.Date(d, "%Y%m%d"),
                   stringsAsFactors = F)
  if(exists("rankings")) {
    rankings <- bind_rows(rankings, df) 
  } else {
    rankings <- df 
  }
}

### Games Between Top-10 Teams
x$top_10 <- NA
for(i in 1:nrow(x)) {
  top10 <- filter(rankings, date < x$date[i]) %>%
    filter(date == max(date)) %>%
    pull(team)
  x$top_10[i] <- x$team[i] %in% top10 & x$opponent[i] %in% top10
}

### Match Weights From https://en.wikipedia.org/wiki/FIFA_Women's_World_Rankings
### 2016 Olympics seems to be missing
x <- mutate(x, "match_weight" = 
              case_when(
                competition == "FIFA Women's World Cup Final" ~ 4,
                competition == "Olympic Football Tournament Women Qualifier" ~ 3,
                competition == "Womens Continental Final" ~ 3,
                competition == "FIFA Women's World Cup Qualifier" ~ 3,
                competition == "Womens Continental Qualifier" ~ 2,
                (competition == "Friendly Women" & top_10) ~ 2,
                competition == "Friendly Women" ~ 1
                
              ))


x <- select(x, date, team, opponent, team_score, location, match_weight) %>%
  rename("goals_scored" = team_score) %>% 
  bind_rows(
    select(x, date, team, opponent, opp_score, location, match_weight) %>%
      mutate("location" = case_when(
        location == "H" ~ "A",
        location == "N" ~ "N",
        location == "A" ~ "H")) %>%
      rename("goals_scored" = opp_score,
             "team" = opponent,
             "opponent" = team)
  )
write.csv(x, "womens_soccer_scores.csv", row.names = F)

### Update w/ Current WC Scores
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

### Rankings
team_num <- (length(glm.futbol$coefficients) - 1)/ 2
rankings <- data.frame("team" = sort(unique(x$team)),
                       "offense" = rep(NA, team_num),
                       "defense" = rep(NA, team_num),
                       stringsAsFactors = F)
off_scale_factor <- mean(glm.futbol$coefficients[2:team_num])
def_scale_factor <- mean(glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)], na.rm = T)
rankings$offense <- c(0, glm.futbol$coefficients[2:team_num]) - off_scale_factor
rankings$defense <- c(0, glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)]) - def_scale_factor
rankings$net_rating <- rankings$offense - rankings$defense

rankings <- arrange(rankings, desc(net_rating)) %>%
  mutate(rank = 1:team_num)
write.csv(rankings, "rankings.csv", row.names = F)
read.csv("rankings_history.csv", as.is = T) %>%
  mutate("date" = as.Date(date)) %>%
  filter(date < Sys.Date()) %>%
  bind_rows(mutate(rankings, "date" = Sys.Date())) %>%
  write.csv(., "rankings_history.csv", row.names = F)

########################## World Cup Simulations ##############################
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

for(i in 1:nrow(fixtures)) {
  if(is.na(fixtures$win[i])) {
    fixtures[i, c("win", "tie", "loss")] <- match_probs(lambda_1 = fixtures$team_score[i],
                                                        lambda_2 = fixtures$opp_score[i])
  }
}

read.csv("pred_history.csv", as.is = T) %>%
  mutate("date" = as.Date(date)) %>%
  bind_rows(filter(fixtures, date == Sys.Date() + 1)) %>%
  filter(!duplicated(paste(date, team))) %>%
  write.csv(., "pred_history.csv", row.names = F)

### Monte Carlo Sims
### See helpers.R for group_sim() function
groups <- c("A", "B", "C", "D", "E", "F")
wc_sims <- data.frame("country" = unique(c(fixtures$team, fixtures$opponent)),
                      "group" = NA,
                      "exp_pts" = 0,
                      "exp_gf" = 0,
                      "exp_gd" = 0,
                      "first_in_group" = 0,
                      "second_in_group" = 0,
                      "third_in_group" = 0,
                      "r16" = 0,
                      "qtrs" = 0,
                      "semis" = 0,
                      "finals" = 0,
                      "champ" = 0,
                      stringsAsFactors = F)
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

nsims <- 10000
for(j in 1:nsims) {
  if(j %% 100 == 0) {
    cat("Sim", j, "of", nsims, "\n")
  }
  ### Simulate Group Stage
  for(i in 1:6) {
    sim_ladder <- sim_group(groups[i])
    
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
  
  if(j == 1) {
    third_history <- filter(third_place, country %in% third) %>%
      mutate("place" = 1:4)
  } else {
    third_history <- bind_rows(third_history,
                               filter(third_place, country %in% third) %>%
                                 mutate("sim" = j))
  }
  
  ###  ### Knock-Out Stage
  teams_left <- 16
  ko_round <- 1
  ko_winners <- c(runners_up[1], winners[4], winners[1], runners_up[2],
                  winners[3], winners[5], winners[2], runners_up[6],
                  runners_up[3], third[1], third[2], winners[6],
                  third[3], runners_up[4], third[4], runners_up[5])
  
  
  wc_sims$r16[wc_sims$country %in% ko_winners] <- 
    wc_sims$r16[wc_sims$country %in% ko_winners] + 1/nsims
  
  while(teams_left > 1) {
    knockout <- data.frame("team" = ko_winners[1:(teams_left/2)],
                           "opponent" = ko_winners[(1 + teams_left/2):teams_left],
                           "team_goals" = rep(NA, teams_left/2),
                           "opp_goals" = rep(NA, teams_left/2),
                           "winner" = rep(NA, teams_left/2),
                           "location" = rep("N", teams_left/2),
                           stringsAsFactors = F)
    
    knockout$location[knockout$team == "France"] <- "H"
    knockout$location[knockout$opponent == "France"] <- "A"
    
    knockout$team_goals <- 
      suppressWarnings(predict(glm.futbol, newdata = knockout, type = "response"))
    knockout$opp_goals <- 
      suppressWarnings(predict(glm.futbol, newdata = invert(knockout), type = "response"))
    
    
    ko_winners <- rep(NA, teams_left/2)
    
    for(i in 1:nrow(knockout)) {
      team_goals <- rpois(1, knockout$team_goals[i])
      opp_goals <- rpois(1, knockout$opp_goals[i])
      if(team_goals > opp_goals) {
        knockout$winner[i] <- knockout$team[i]
      } else if(team_goals < opp_goals) {
        knockout$winner[i] <- knockout$opponent[i]
      } else { 
        ## Penalty Shoutout 50-50
        knockout$winner[i] <- sample(c(knockout$team[i], knockout$opponent[i]), 1)
      }
    }
    
    if(teams_left > 2) {
      ko_winners <- knockout$winner[c(seq(1, teams_left/2, 2), seq(2, teams_left/2, 2))]
    } else{
      ko_winners <- knockout$winner
    }
    index <- wc_sims$country %in% knockout$winner
    teams_left <- teams_left/2
    if(teams_left >= 1) {
      wc_sims[index, 9 + ko_round] <- wc_sims[index, 9 + ko_round] + 1/nsims
    }
    ko_round <- ko_round + 1
  }
}
write.csv(wc_sims, "wc_sims.csv", row.names = F)
write.csv(third_history, "third_place.csv", row.names = F)
read.csv("wc_sims_history.csv", as.is = T) %>%
  mutate("date" = as.Date(date)) %>%
  filter(date < Sys.Date()) %>%
  bind_rows(mutate(wc_sims, "date" = Sys.Date())) %>%
  write.csv(., "wc_sims_history.csv", row.names = F)
