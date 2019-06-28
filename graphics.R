library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(forcats)
library(knitr)
library(kableExtra)
library(ggforce)
library(gridExtra)
library(grid)

wc_sims <- read.csv("wc_sims.csv", as.is = T)

### Exp Points
ggplot(wc_sims, aes(x = fct_relevel(wc_sims$country, 
                                    arrange(wc_sims, desc(exp_pts)) %>% 
                                      pull(country)), 
                    y = exp_pts)) +
  facet_wrap(~group, scales = "free_x") +
  geom_bar(aes(fill = group), stat = "identity") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Country",
       y = "Expcted Points",
       title = "Expected Group Stage Points",
       subtitle = "2019 FIFA Women's World Cup")

clean <- function(x) { 
  return(as.numeric(gsub("[>%<]", "",gsub("\u2705", "100", gsub("\u274c", "0", x)))))
}

fix <- function(x) {
  ifelse(as.numeric(x) < 0.11 & as.numeric(x) > 0 , "< 0.1%", 
         ifelse(as.numeric(x) > 99 & as.numeric(x) < 100, "> 99%", 
                ifelse(as.numeric(x) == 100, emo::ji("check"),
                       ifelse(as.numeric(x) == 0, emo::ji("x"),
                              paste0(x, "%")))))
}

### Odds Chart
select(wc_sims, country, group, r16, qtrs, semis, finals, champ) %>%
  arrange(round(desc(champ), 2), round(desc(finals), 2), desc(semis), desc(qtrs), desc(r16)) %>%
  mutate("champ" = sprintf("%.1f", 100 * champ),
         "finals" = sprintf("%.1f", 100 * finals),
         "semis" = sprintf("%.1f", 100 * semis),
         "qtrs" = sprintf("%.1f", 100 * qtrs),
         "r16" = sprintf("%.1f", 100 * r16)) %>%
  mutate("champ" = fix(champ),
         "finals" = fix(finals),
         "semis" = fix(semis),
         "qtrs" = fix(qtrs),
         "r16" = fix(r16)) %>%
  mutate(champ = cell_spec(
    champ, color = "white", bold = T,
    background = ifelse(clean(champ) %in% c(0, 100), "white", 
                        spec_color(clean(champ), 
                                   end = 0.9, option = "C", direction = 1,
                                   scale_from = c(0,100)))
  )) %>%  
  mutate(finals = cell_spec(
    finals, color = "white", bold = T,
    background = ifelse(clean(finals) %in% c(0, 100), "white", 
                        spec_color(clean(finals), 
                                   end = 0.9, option = "C", direction = 1,
                                   scale_from = c(0,100)))
  )) %>%
  mutate(semis = cell_spec(
    semis, color = "white", bold = T,
    background = ifelse(clean(semis) %in% c(0, 100), "white", 
                        spec_color(clean(semis), 
                                   end = 0.9, option = "C", direction = 1,
                                   scale_from = c(0,100)))
  )) %>%
  mutate(qtrs = cell_spec(
    qtrs, color = "white", bold = T,
    background = ifelse(clean(qtrs) %in% c(0, 100), "white", 
                        spec_color(clean(qtrs), 
                                   end = 0.9, option = "C", direction = 1,
                                   scale_from = c(0,100)))
  )) %>%
  mutate(r16 = cell_spec(
    r16, color = "white", bold = T,
    background = ifelse(clean(r16) %in% c(0, 100), "white", 
                        spec_color(clean(r16), 
                                   end = 0.9, option = "C", direction = 1,
                                   scale_from = c(0,100)))
  )) %>%
  rename("Country" = country,
         "Group" = group, 
         "Round of 16" = r16,
         "Quarterfinals" = qtrs,
         "Semifinals" = semis,
         "Finals" = finals,
         "Champion" = champ) %>%
  kable(., escape = F, align = "ccccccc") %>%
  kable_styling("striped", full_width = F, position = "center") %>%
  row_spec(0, bold = T, font_size = 12) %>%
  add_header_above(c("2019 FIFA Women's World Cup" = 7), bold = T, font_size = 24)




### Exp GD
ggplot(wc_sims, aes(x = fct_relevel(wc_sims$country, 
                                    arrange(wc_sims, desc(exp_gd)) %>% 
                                      pull(country)), 
                    y = exp_gd)) +
  facet_wrap(~group, scales = "free_x") +
  geom_bar(aes(fill = exp_gd > 0), stat = "identity") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Country",
       y = "Expected GD",
       title = "Expected Group Stage Goal Differential",
       subtitle = "2019 FIFA Women's World Cup") +
  scale_fill_manual(values = c("red", "seagreen"))


### Group Stage Distributions
select(wc_sims, country, group, first_in_group, second_in_group, third_in_group) %>%
  mutate("fourth_in_group" = 1 - first_in_group - second_in_group - third_in_group) %>%
  gather(place, pct, -country, -group) %>%
  mutate("place" = case_when(place == "first_in_group" ~ "1",
                             place == "second_in_group" ~ "2",
                             place == "third_in_group" ~ "3",
                             place == "fourth_in_group" ~ "4")) %>%
  ggplot(aes(x = fct_relevel(country, 
                             arrange(wc_sims, desc(first_in_group)) %>% 
                               pull(country)), y = 100 * pct)) +
  facet_wrap(~group, scale = "free_x") +
  geom_bar(aes(fill = place), stat = "identity",
           position = "dodge") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Country",
       y = "Percent",
       title = "Group Finish Distributions",
       subtitle = "2019 FIFA Women's World Cup",
       fill = "Ladder Postion")

### Goal Plot Function
goal_plot <- function(team1, team2, location, col1, col2, m,  knockout = F) {
  lambda1 <- suppressWarnings(predict(glm.futbol, newdata = data.frame("team" = team1,
                                                                       "opponent" = team2,
                                                                       "location" = location,
                                                                       stringsAsFactors = F), 
                                      type = "response"))
  lambda2 <- suppressWarnings(predict(glm.futbol, newdata = invert(data.frame("team" = team1,
                                                                              "opponent" = team2,
                                                                              "location" = location,
                                                                              stringsAsFactors = F)), 
                                      type = "response"))
  max_goals <- 20
  score_matrix <- dpois(0:max_goals, lambda1) %o% dpois(0:max_goals, lambda2)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)]) + 1/2 * knockout * tie_prob
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)]) + 1/2 * knockout * tie_prob
  tie_prob2 <- ifelse(tie_prob < 0.01, "< 1%", paste0(tie_prob * 100, "%"))
  win_prob2 <- ifelse(win_prob < 0.01, "< 1%", ifelse(win_prob > 0.99, "> 99%", paste0(win_prob * 100, "%")))
  loss_prob2 <- ifelse(loss_prob < 0.01, "< 1%", ifelse(loss_prob > 0.99, "> 99%", paste0(loss_prob * 100, "%")))
  
  z <- data.frame("Team" = rep(c(team1, team2), rep(1 + m,2)),
                  "Goals" = rep(c(as.character(0:(m-1)), paste0(m, "+")), 2),
                  "Probability" = c(dpois(0:(m-1), lambda1), sum(dpois(m:20, lambda1)),
                                    dpois(0:(m-1), lambda2), sum(dpois(m:20, lambda2))))
  
  vec <- c(team1, team2)
  vec <- sort(vec)
  if(vec[1] == team2) {
    tmp <- col1
    col1 <- col2
    col2 <- tmp
  }
  
  message <- ifelse(knockout, "Chance of Extra Time:", "Tie Probability:")
  ys <- max(z$Probability) - seq(0.03, 0.18, 0.03)
  ggplot(z, aes(x = Goals, y = Probability, fill = Team)) + 
    geom_bar(stat = "identity", position='dodge', colour  = "black") + 
    labs(title = paste(team1, "vs.", team2, "Goal Distributions")) +
    scale_fill_manual(values=c(col1, col2)) +
    annotate("text", x = m-0.5, y = ys[1], label = paste(team1, "Expected Goals:", round(lambda1, 2))) +
    annotate("text", x = m-0.5, y = ys[2], label = paste(team1, "Win Probability:", win_prob2)) + 
    annotate("text", x = m-0.5, y = ys[3], label = paste(team2, "Expected Goals:", round(lambda2, 2))) + 
    annotate("text", x = m-0.5, y = ys[4], label = paste(team2, "Win Probability:", loss_prob2)) + 
    annotate("text", x = m-0.5, y = ys[5], label = paste(message, tie_prob2)) + 
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.title = element_text(size = 14)) +
    scale_x_discrete(limits = c(as.character(0:(m-1)), paste0(m, "+")),
                     labels = c(as.character(0:(m-1)), paste0(m, "+")))
  
  if(knockout) {
    lambda1 <- lambda1/3
    lambda2 <- lambda2/3
    score_matrix <- dpois(0:max_goals, lambda1) %o% dpois(0:max_goals, lambda2)
    tie_prob <- sum(diag(score_matrix)) * tie_prob
    p <- p + annotate("text", x = m-0.5, y = ys[6], label = paste("Chance of Penalties", 
                                                                  round(tie_prob, 2)))
  }
  return(p)
}


wc_sims_history <- read.csv("wc_sims_history.csv", as.is = T)

### Evoluation of Advancement
grid.arrange(
  ggplot(filter(wc_sims_history, group == "A"), 
         aes(x = as.Date(date), y = qtrs)) + 
    geom_line(aes(color = country), size = 1.1) +
    theme_bw() +
    guides(color = guide_legend(ncol=2)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 14)) +
    labs(x = "", y = "", subtitle = "Group A", color = "Country") +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("dodgerblue", "navy", "forestgreen", "red")),
  
  ggplot(filter(wc_sims_history, group == "B"), 
         aes(x = as.Date(date), y = qtrs)) + 
    geom_line(aes(color = country), size = 1.1) +
    theme_bw() +
    guides(color = guide_legend(ncol=2)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 14)) +
    labs(x = "", y = "", subtitle = "Group B", color = "Country") +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("red", "black", "forestgreen", "red4")),
  
  ggplot(filter(wc_sims_history, group == "C"), 
         aes(x = as.Date(date), y = qtrs)) + 
    geom_line(aes(color = country), size = 1.1) +
    theme_bw() +
    guides(color = guide_legend(ncol=2)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 14)) +
    labs(x = "", y = "", subtitle = "Group C", color = "Country") +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("forestgreen", "goldenrod1", "blue", "green")),
  
  ggplot(filter(wc_sims_history, group == "D"), 
         aes(x = as.Date(date), y = qtrs)) + 
    geom_line(aes(color = country), size = 1.1) +
    theme_bw() +
    guides(color = guide_legend(ncol=2)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 14)) +
    labs(x = "", y = "", subtitle = "Group D", color = "Country") +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("dodgerblue", "red", "navy", "blue")),
  
  ggplot(filter(wc_sims_history, group == "E"), 
         aes(x = as.Date(date), y = qtrs)) + 
    geom_line(aes(color = country), size = 1.1) +
    theme_bw() +
    guides(color = guide_legend(ncol=2)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 14)) +
    labs(x = "", y = "", subtitle = "Group E", color = "Country") +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("forestgreen", "red", "orange", "navy")),
  
  ggplot(filter(wc_sims_history, group == "F"), 
         aes(x = as.Date(date), y = qtrs)) + 
    geom_line(aes(color = country), size = 1.1) +
    theme_bw() +
    guides(color = guide_legend(ncol=2)) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          axis.title = element_text(size = 14)) +
    labs(x = "", y = "", subtitle = "Group F", color = "Country") +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("dodgerblue", "yellow", "red", "navy")),
  top = textGrob("Evolutions of WWC Progression Probability", gp=gpar(fontsize=20)), 
  left = textGrob("Odds of Advancing to Quarterfinals", gp=gpar(fontsize=14), rot = 90), 
  bottom = textGrob("Date", gp=gpar(fontsize=14)), ncol = 3
)

### Deltas
deltas <- filter(wc_sims_history, date %in% c("2019-06-06", "2019-06-11")) %>%
  group_by(country, group) %>%
  summarise("R16" = r16[date == "2019-06-11"]- r16[date == "2019-06-06"],
            "Quarters" = qtrs[date == "2019-06-11"]- qtrs[date == "2019-06-06"],
            "Semis" = semis[date == "2019-06-11"]- semis[date == "2019-06-06"],
            "Finals" = finals[date == "2019-06-11"]- finals[date == "2019-06-06"],
            "Champ" = champ[date == "2019-06-11"]- champ[date == "2019-06-06"]
  ) %>%
  ungroup()


gather(deltas, round, prob, -country, -group) %>%
  ggplot(aes(x = country, y = prob)) +
  geom_bar(aes(fill = fct_relevel(round, "R16", "Quarters", "Semis", "Finals", "Champ")),
           stat = "identity", position = "dodge") +
  facet_wrap(~group, scales = "free_x") +
  labs(x = "Country", y = "Change in Round Advancement Probability",
       fill = "Round", title = "Women's World Cup Change in Round Advancement Probability",
       subtitle = "After Matchday 1") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

third_history$sim <- rep(1:10000, each = 4)

third_tab <- group_by(third_history, sim) %>%
  summarise("min_pts" = min(pts),
            "min_gd" = min(goal_diff[pts == min(pts)]), 
            "min_gf" = min(goals_forced[pts == min(pts) &
                                          goal_diff == min(goal_diff[pts ==min(pts)])]))



ggplot(third_tab, aes(x = min_gd, y = ..density..)) +
  geom_histogram(bins = 12, fill = "seagreen") +
  facet_wrap(~paste(min_pts, "Points")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  labs(x = "Goal Differential",
       y = "Density",
       title = "Points and Goal Differenttial for Worst Advancing Third Place Country")
