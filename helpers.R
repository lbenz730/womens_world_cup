### Helper Functions
### Inverts Perspective of Data Frame from Team to Opponent
invert <- function(data, score = F) {
  data <- mutate(data, tmp = team, team = opponent, opponent = tmp)
  data$tmp[data$location == "H"] <- "A"
  data$tmp[data$location == "A"] <- "H"
  data$tmp[data$location == "N"] <- "N"
  data$location <- data$tmp
  if(score) {
    data$tmp <- data$team_score
    data$team_score <- data$opp_score
    data$opp_score <- data$tmp
  }
  return(select(data,-tmp))
}

stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

### Obtain W, L, T probabilities
match_probs <- function(lambda_1, lambda_2, group = "Z") {
 
  max_goals <- 20
  score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  
  if(group %in% LETTERS[1:6]) {
    return(c(win_prob, tie_prob, loss_prob))
  }
  
  probs <- match_probs(lambda_1/3, lambda_2/3, "A")
  
  win_prob <- win_prob + tie_prob * probs[1] + 1/2 * tie_prob * probs[2]
  loss_prob <- loss_prob + tie_prob * probs[3] + 1/2 * tie_prob * probs[2]
  tie_prob <- 0
  
  return(c(win_prob, tie_prob, loss_prob))
  
}


### Simulate a group at the World Cup
sim_group <- function(group_name) {
  games <- filter(fixtures, group == group_name) %>% 
    mutate("team_goals" = NA, "opp_goals" = NA, "team_gd" = NA, "opp_gd" = NA,
           "team_points" = 0, "opp_points" = 0)
  for(i in 1:nrow(games)) {
    if(games$win[i] == 1 | games$loss[i] == 1 | games$tie[i] == 1) {
      games$team_goals[i] <- games$team_score[i]
      games$opp_goals[i] <- games$opp_score[i]
      games$team_gd[i] <- games$goal_diff[i]
      games$opp_gd[i] <- -games$goal_diff[i]
      games$team_points[i] <- 3*games$win[i] + games$tie[i]
      games$opp_points[i] <- 3*games$loss[i] + games$tie[i]
    } else{
      lambda_1 <- games$team_score[i]
      lambda_2 <- games$opp_score[i]
      games$team_goals[i] <- rpois(1, lambda_1)
      games$opp_goals[i] <- rpois(1, lambda_2)
      games$team_gd[i] <- games$team_goals[i] - games$opp_goals[i]
      games$opp_gd[i] <- games$opp_goals[i] - games$team_goals[i]
      if(games$team_gd[i] > 0) {
        games$team_points[i] <- 3
      }
      if(games$opp_gd[i] > 0) {
        games$opp_points[i] <- 3
      }
      if(games$team_gd[i] == 0) {
        games$team_points[i] <- 1
        games$opp_points[i] <- 1
      }
    }
  }
  ladder <- data.frame("country" = unique(c(games$team, games$opponent)),
                       "pts" = rep(NA, 4),
                       "goals_forced" = rep(NA, 4),
                       "goal_diff" = rep(NA, 4),
                       stringsAsFactors = F)
  for(i in 1:4) {
    ladder$pts[i] <- sum(games$team_points[games$team == ladder$country[i]]) + 
      sum(games$opp_points[games$opponent == ladder$country[i]])
    ladder$goal_diff[i] <- sum(games$team_gd[games$team == ladder$country[i]]) + 
      sum(games$opp_gd[games$opponent == ladder$country[i]])
    ladder$goals_forced[i] <- sum(games$team_goals[games$team == ladder$country[i]]) + 
      sum(games$opp_goals[games$opponent == ladder$country[i]])
  }
  
  ladder <- arrange(ladder, desc(pts), desc(goal_diff), desc(goals_forced))
  return(ladder)
}

### Simulate a group at the World Cup
sim_group_lev <- function(group_name) {
  games <- filter(fixtures, group == group_name) %>% 
    mutate("team_goals" = NA, "opp_goals" = NA, "team_gd" = NA, "opp_gd" = NA,
           "team_points" = 0, "opp_points" = 0)
  for(i in 1:nrow(games)) {
    if(!is.na(games$win[i])) {
      games$team_goals[i] <- games$team_score[i]
      games$opp_goals[i] <- games$opp_score[i]
      games$team_gd[i] <- games$goal_diff[i]
      games$opp_gd[i] <- -games$goal_diff[i]
      games$team_points[i] <- 3*games$win[i] + games$tie[i]
      games$opp_points[i] <- 3*games$loss[i] + games$tie[i]
    } else{
      lambda_1 <- games$team_score[i]
      lambda_2 <- games$opp_score[i]
      games$team_goals[i] <- rpois(1, lambda_1)
      games$opp_goals[i] <- rpois(1, lambda_2)
      games$team_gd[i] <- games$team_goals[i] - games$opp_goals[i]
      games$opp_gd[i] <- games$opp_goals[i] - games$team_goals[i]
      if(games$team_gd[i] > 0) {
        games$team_points[i] <- 3
      }
      if(games$opp_gd[i] > 0) {
        games$opp_points[i] <- 3
      }
      if(games$team_gd[i] == 0) {
        games$team_points[i] <- 1
        games$opp_points[i] <- 1
      }
    }
  }
  ladder <- data.frame("country" = unique(c(games$team, games$opponent)),
                       "pts" = rep(NA, 4),
                       "goals_forced" = rep(NA, 4),
                       "goal_diff" = rep(NA, 4),
                       stringsAsFactors = F)
  for(i in 1:4) {
    ladder$pts[i] <- sum(games$team_points[games$team == ladder$country[i]]) + 
      sum(games$opp_points[games$opponent == ladder$country[i]])
    ladder$goal_diff[i] <- sum(games$team_gd[games$team == ladder$country[i]]) + 
      sum(games$opp_gd[games$opponent == ladder$country[i]])
    ladder$goals_forced[i] <- sum(games$team_goals[games$team == ladder$country[i]]) + 
      sum(games$opp_goals[games$opponent == ladder$country[i]])
  }
  
  ladder <- arrange(ladder, desc(pts), desc(goal_diff), desc(goals_forced))
  return(ladder)
}

### Find Group for a team
find_group <- function(country) {
  group <- filter(fixtures, team == country | opponent == country) %>%
    slice(1) %>% 
    pull(group)
  return(group)
}

### Find ordering of 3rd place teams for knockout stage
order_thirds <- function(third_place) {
  tp <- arrange(third_place, desc(pts), desc(goal_diff), desc(goals_forced)) %>%
    slice(1:4) %>%
    arrange(group)
  group <- tp$group
  ordering <- case_when(
    group[1] == "A" & group[2] == "B" & group[3] == "C" & group[4] == "D" ~ "CDAB",
    group[1] == "A" & group[2] == "B" & group[3] == "C" & group[4] == "E" ~ "CABE",
    group[1] == "A" & group[2] == "B" & group[3] == "C" & group[4] == "F" ~ "CABF",
    group[1] == "A" & group[2] == "B" & group[3] == "D" & group[4] == "E" ~ "DABE",
    group[1] == "A" & group[2] == "B" & group[3] == "D" & group[4] == "F" ~ "DABF",
    group[1] == "A" & group[2] == "B" & group[3] == "E" & group[4] == "F" ~ "EABF",
    group[1] == "A" & group[2] == "C" & group[3] == "D" & group[4] == "E" ~ "CDAE",
    group[1] == "A" & group[2] == "C" & group[3] == "D" & group[4] == "F" ~ "CDAF",
    group[1] == "A" & group[2] == "C" & group[3] == "E" & group[4] == "F" ~ "CAFE",
    group[1] == "A" & group[2] == "D" & group[3] == "E" & group[4] == "F" ~ "DAFE",
    group[1] == "B" & group[2] == "C" & group[3] == "D" & group[4] == "E" ~ "CDBE",
    group[1] == "B" & group[2] == "C" & group[3] == "D" & group[4] == "F" ~ "CDBF",
    group[1] == "B" & group[2] == "C" & group[3] == "E" & group[4] == "F" ~ "ECBF",
    group[1] == "B" & group[2] == "D" & group[3] == "E" & group[4] == "F" ~ "EDBF",
    group[1] == "C" & group[2] == "D" & group[3] == "E" & group[4] == "F" ~ "CDFE"
  )
 tmp <- data.frame("x" = 1:4,
                   "group" = unlist(strsplit(ordering, "")),
                   stringsAsFactors = F)
 third <- inner_join(tp, tmp, "group") %>%
   arrange(x) %>%
   pull(country)
  
 return(third)
}
