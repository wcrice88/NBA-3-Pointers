install.packages("tidyverse")
install.packages("dplyr")
install.packages("stargazer")
install.packages("wooldridge")
install.packages("jtools")
install.packages("huxtable")
library(huxtable)
library(jtools)
library(dplyr)
library(stargazer)
library(tidyverse)
library(ggplot2)
library(wooldridge)
library(readr)
library(readxl)


Team_Stats_NBA_per_100_Poss <- read_excel("~/Downloads/NBA STATS/Team Stats NBA per 100 Poss.xlsx")
View(Team_Stats_NBA_per_100_Poss)


#Main Data Sheet

NBA_3pt_Project <- Team_Stats_NBA_per_100_Poss %>%
  rename(winpercent = "w %") %>%
  mutate(winpercent=winpercent*100) %>%
  mutate(x3p_percent=x3p_percent*100) %>%
  filter(season < 2023)

glimpse(NBA_3pt_Project)

NBA_Plot <- ggplot(data = NBA_3pt_Project, aes(x=x3p_percent, y=winpercent)) +
  geom_point() +
  ylab("Win Percentage") +
  ylim(0,100) +
  stat_smooth(method=lm, col="blue") +
  ggtitle("Is 3-Point Percentage a Predictor of Win Percentage?")
NBA_Plot

NBA_3pt_Simple_Regression <- lm(winpercent ~ x3p_percent, data = NBA_3pt_Project)

summary(NBA_3pt_Simple_Regression)

#Data, Plot, and Regression for 3-Point Importance During 1970's

Seventies_3pt_Data <- NBA_3pt_Project %>%
  filter(season <1980)

Seventies_Plot <- ggplot(data = Seventies_3pt_Data, aes(x=x3pa_per_100_poss, y=winpercent))+
  geom_point() +
  ylab("Win Percentage") +
  ylim(0,100) +
  xlab("3-Point Percentage") +
  stat_smooth(method=lm, col="red") +
  ggtitle("3-Pointers Attempted and Win % During the 1970's")
Seventies_Plot

Seventies_Regression <- lm(winpercent ~ x3p_percent + x3p_per_100_poss + x3pa_per_100_poss + x2p_per_100_poss + x2pa_per_100_poss + ft_per_100_poss + fta_per_100_poss + pts_per_100_poss, data = Seventies_3pt_Data)
export_summs(Seventies_Regression, model.names="Seventies Regression")


#Data, Plot, and Regression for 3-Point Importance During 1980's


Eighties_3pt_Data <- NBA_3pt_Project %>%
  filter(between(season, 1980, 1989))

Eighties_Plot <- ggplot(data = Eighties_3pt_Data, aes(x=x3pa_per_100_poss, y=winpercent))+
  geom_point() +
  ylab("Win Percentage") +
  ylim(0,100) +
  xlab("3-Point Percentage") +
  stat_smooth(method=lm, col="red") +
  ggtitle("3-Pointers Attempted and Win % During the 1980's")
Eighties_Plot

Eighties_Regression <- lm(winpercent ~ x3p_percent + x3p_per_100_poss + x3pa_per_100_poss + x2p_per_100_poss + x2pa_per_100_poss + ft_per_100_poss + fta_per_100_poss + pts_per_100_poss, data = Eighties_3pt_Data)
export_summs(Eighties_Regression, model.names="Eighties Regression")


#Data, Plot, and Regression for 3-Point Importance During 1990's

Nineties_3pt_Data <- NBA_3pt_Project %>%
  filter(between(season, 1990, 1999))

Nineties_Plot <- ggplot(data = Nineties_3pt_Data, aes(x=x3pa_per_100_poss, y=winpercent))+
  geom_point() +
  ylab("Win Percentage") +
  ylim(0,100) +
  xlab("3-Point Percentage") +
  stat_smooth(method=lm, col="red") +
  ggtitle("3-Pointers Attempted and Win % During the 1990's")
Nineties_Plot

Nineties_Regression <- lm(winpercent ~ x3p_percent + x3p_per_100_poss + x3pa_per_100_poss + x2p_per_100_poss + x2pa_per_100_poss + ft_per_100_poss + fta_per_100_poss + pts_per_100_poss, data = Nineties_3pt_Data)
export_summs(Nineties_Regression, model.names="Nineties Regression")


#Data, Plot, and Regression for 3-Point Importance During 2000's

Oughts_3pt_Data <- NBA_3pt_Project %>%
  filter(between(season, 2000, 2009))

Oughts_Plot <- ggplot(data = Oughts_3pt_Data, aes(x=x3pa_per_100_poss, y=winpercent))+
  geom_point() +
  ylab("Win Percentage") +
  ylim(0,100) +
  xlab("3-Point Percentage") +
  stat_smooth(method=lm, col="red") +
  ggtitle("3-Pointers Attempted and Win % During the 2000's")
Oughts_Plot

Oughts_Regression <- lm(winpercent ~ x3p_percent + x3p_per_100_poss + x3pa_per_100_poss + x2p_per_100_poss + x2pa_per_100_poss + ft_per_100_poss + fta_per_100_poss + pts_per_100_poss, data = Oughts_3pt_Data)
export_summs(Oughts_Regression, model.names="Oughts Regression")

#Data, Plot, and Regression for 3-Point Importance During 2010's

Tens_3pt_Data <- NBA_3pt_Project %>%
  filter(between(season, 2010, 2019))

Tens_Plot <- ggplot(data = Tens_3pt_Data, aes(x=x3pa_per_100_poss, y=winpercent))+
  geom_point() +
  ylab("Win Percentage") +
  ylim(0,100) +
  xlab("3-Point Percentage") +
  stat_smooth(method=lm, col="red") +
  ggtitle("3-Pointers Attempted and Win % During the 2010's")
Tens_Plot

Tens_Regression <- lm(winpercent ~ x3p_percent + x3p_per_100_poss + x3pa_per_100_poss + x2p_per_100_poss + x2pa_per_100_poss + ft_per_100_poss + fta_per_100_poss + pts_per_100_poss, data = Tens_3pt_Data)
export_summs(Tens_Regression, model.names="Tens Regression")


#Data, Plot, and Regression for 3-Point Importance During 2020's

Twenties_3pt_Data <- NBA_3pt_Project %>%
  filter(between(season, 2020, 2023))

Twenties_Plot <- ggplot(data = Twenties_3pt_Data, aes(x=x3pa_per_100_poss, y=winpercent))+
  geom_point() +
  ylab("Win Percentage") +
  ylim(0,100) +
  xlab("3-Point Percentage") +
  stat_smooth(method=lm, col="red") +
  ggtitle("3-Pointers Attempted and Win % During the 2020's")
Twenties_Plot

Twenties_Regression <- lm(winpercent ~ x3p_percent + x3p_per_100_poss + x3pa_per_100_poss + x2p_per_100_poss + x2pa_per_100_poss + ft_per_100_poss + fta_per_100_poss + pts_per_100_poss, data = Twenties_3pt_Data)
export_summs(Twenties_Regression, model.names="Twenties Regression")
