library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(forcats)
library(knitr)
library(kableExtra)

wc_sims <- read.csv("wc_sims.csv", as.is = T)


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


select(wc_sims, country, group, r16, qtrs, semis, finals, champ) %>%
  arrange(desc(champ), desc(finals), desc(semis), desc(qtrs), desc(r16)) %>%
  mutate("champ" = sprintf("%.1f", 100 * champ),
         "finals" = sprintf("%.1f", 100 * finals),
         "semis" = sprintf("%.1f", 100 * semis),
         "qtrs" = sprintf("%.1f", 100 * qtrs),
         "r16" = sprintf("%.1f", 100 * r16)) %>%
  mutate("champ" = ifelse(as.numeric(champ) < 0.1, "< 0.1%", paste0(champ, "%")),
         "finals" = ifelse(as.numeric(finals) < 0.1, "< 0.1%", paste0(finals, "%")),
         "semis" = ifelse(as.numeric(semis) < 0.1, "< 0.1%", paste0(semis, "%")),
         "qtrs" = ifelse(as.numeric(qtrs) < 0.1, "< 0.1%", paste0(qtrs, "%")),
         "r16" = ifelse(as.numeric(r16) < 0.1, "< 0.1%", ifelse(as.numeric(r16) > 99, "> 99%", 
                                                                           paste0(r16, "%")))) %>%
  mutate(champ = cell_spec(
    champ, color = "white", bold = T,
    background = spec_color(as.numeric(gsub("[%<]", "", champ)), end = 0.9, option = "C", direction = 1,
                            scale_from = c(0,100))
  )) %>%  
  mutate(finals = cell_spec(
    finals, color = "white", bold = T,
    background = spec_color(as.numeric(gsub("[%<]", "", finals)), end = 0.9, option = "C", direction = 1,
                            scale_from = c(0,100))
  )) %>%
  mutate(semis = cell_spec(
    semis, color = "white", bold = T,
    background = spec_color(as.numeric(gsub("[%<]", "", semis)), end = 0.9, option = "C", direction = 1,
                            scale_from = c(0,100))
  )) %>%
  mutate(qtrs = cell_spec(
    qtrs, color = "white", bold = T,
    background = spec_color(as.numeric(gsub("[%<]", "", qtrs)), end = 0.9, option = "C", direction = 1,
                            scale_from = c(0,100))
  )) %>%
  mutate(r16 = cell_spec(
    r16, color = "white", bold = T,
    background = spec_color(as.numeric(gsub("[>%<]", "", r16)), end = 0.9, option = "C", direction = 1,
                            scale_from = c(0,100))
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

