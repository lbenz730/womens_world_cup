library(dplyr)
library(ggplot2)
library(ggthemes)

### 538 WWC Predictions
fte <- read.csv("https://projects.fivethirtyeight.com/soccer-api/international/2019/wwc_matches.csv", as.is = T)
fte <- filter(fte, !is.na(score1)) %>%
  mutate("date" = as.Date(date))

### 538 Log Loss
fte$score1[38] <- 2
fte_log_loss <- sum(-log(fte$prob1) * (fte$score1 > fte$score2) -
  log(fte$prob2) * (fte$score2 > fte$score1) -
  log(fte$probtie + 0.000001) * (fte$score1 == fte$score2))


### My Log Loss
fixtures <- read.csv("fixtures.csv", as.is = T)
fixtures$team_score[37] <- 2
pred_history <- read.csv("pred_history.csv", as.is = T)
lsb <- select(fixtures, date, team, opponent, team_score, opp_score) %>%
  filter(!is.na(team_score)) %>%
  mutate("date" = as.Date(date, "%m/%d/%y")) %>%
  inner_join(mutate(pred_history, "date" = as.Date(date)), 
             by = c("team", "opponent", "date"), suffix = c("", "_pred"))

lsb_log_loss <- sum(-log(lsb$win) * (lsb$team_score > lsb$opp_score) -
                      log(lsb$loss) * (lsb$team_score < lsb$opp_score) -
                      log(lsb$tie + 0.000001) * (lsb$team_score == lsb$opp_score))


log_loss <- function(d) {
  x <- filter(lsb, date <= d)
  y <- filter(fte, date <= d)
  
  return(c(sum(-log(x$win) * (x$team_score > x$opp_score) -
        log(x$loss) * (x$team_score < x$opp_score) -
        log(x$tie + 0.000001) * (x$team_score == x$opp_score)),
  sum(-log(y$prob1) * (y$score1 > y$score2) -
        log(y$prob2) * (y$score2 > y$score1) -
        log(y$probtie + 0.000001) * (y$score1 == y$score2))))
  
}

modified_log_loss <- function(d) {
  x <- filter(lsb, date <= d)
  y <- filter(fte, date <= d)
  return(c(sum(-log(x$win - x$win * x$loss) * (x$team_score > x$opp_score) -
                 log(x$loss - x$win * x$loss) * (x$team_score < x$opp_score) -
                 log(x$tie + x$win * x$loss) * (x$team_score == x$opp_score)),
           sum(-log(y$prob1 - y$prob1 * y$prob2) * (y$score1 > y$score2) -
                 log(y$prob2 - y$prob1 * y$prob2) * (y$score2 > y$score1) -
                 log(y$probtie + y$prob1 * y$prob2) * (y$score1 == y$score2)))) 
}

ll_mat <- sapply(unique(lsb$date), log_loss)
ll_mat2 <- sapply(unique(lsb$date), modified_log_loss)

df <- data.frame("date" = unique(lsb$date),
           "log_loss" = c(ll_mat[1,], ll_mat[2,]),
           "model" = rep(c("LSB", "538"), each = length(unique(lsb$date))))

df2 <- data.frame("date" = unique(lsb$date),
                 "log_loss" = c(ll_mat2[1,], ll_mat2[2,]),
                 "model" = rep(c("LSB", "538"), each = length(unique(lsb$date))))

ggplot(df, aes(x = date, y = log_loss)) +
  geom_line(aes(color = model), size = 2) +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Date",
       y = "Log-Loss",
       title = "2019 FIFA Women's World Cup",
       subtitle = "Cumulative Log Loss (Multinomial)",
       color = "Model") +
  scale_color_manual(values = c("#ED713B", "seagreen"))

ggplot(df2, aes(x = date, y = log_loss)) +
  geom_line(aes(color = model), size = 2) +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Date",
       y = "Log-Loss",
       title = "2019 FIFA Women's World Cup",
       subtitle = "Cumulative Log Loss (Ordinal)",
       color = "Model") +
  scale_color_manual(values = c("#ED713B", "seagreen"))


rbind(mutate(df, "method" = "Multinomial"), 
      mutate(df2, "method" = "Ordinal")) %>%
  ggplot(aes(x = date, y = log_loss)) +
  facet_wrap(~method) +
  geom_line(aes(color = model), size = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Date",
       y = "Log-Loss",
       title = "2019 FIFA Women's World Cup",
       subtitle = "Cumulative Log Loss",
       color = "Model") +
  scale_color_manual(values = c("#ED713B", "seagreen"))
