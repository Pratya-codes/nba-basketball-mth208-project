library("rvest")
library("tidyverse")

html = read_html("https://www.basketball-reference.com/leagues/NBA_2025_per_game.html")
table = html %>% html_table()
Player_database=table[[1]]
Player_database = Player_database[order(Player_database$PTS,decreasing = TRUE),]
Player_database$PTS = as.double(Player_database$PTS)
Player_database$Player = as.character(Player_database$Player)
###write your location to save csv
write.csv(Player_database,"../data/Player_database.csv")
Player_database

#team rating for 2024-25
html = read_html("https://www.basketball-reference.com/leagues/NBA_2025_ratings.html")
table = html %>% html_table()
team_rating_24_25=table[[1]]
team_rating_24_25 = team_rating_24_25[-1, ]
colnames(team_rating_24_25) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_24_25$Team = as.character(team_rating_24_25$Team)
team_rating_24_25 <- team_rating_24_25 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_24_25,"../data/team_rating_24_25.csv")
### this table is according to descending order of Adjusted net rating
team_rating_24_25

#team rating for 2023-24
html = read_html("https://www.basketball-reference.com/leagues/NBA_2024_ratings.html")
table = html %>% html_table()
team_rating_23_24=table[[1]]
team_rating_23_24 = team_rating_23_24[-1, ]
colnames(team_rating_23_24) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_23_24$Team = as.character(team_rating_23_24$Team)
team_rating_23_24 <- team_rating_23_24 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_23_24,"../data/team_rating_23_24.csv")
### this table is according to descending order of Adjusted net rating
team_rating_23_24

#team rating for 2022-23
html = read_html("https://www.basketball-reference.com/leagues/NBA_2023_ratings.html")
table = html %>% html_table()
team_rating_22_23=table[[1]]
team_rating_22_23 = team_rating_22_23[-1, ]
colnames(team_rating_22_23) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_22_23$Team = as.character(team_rating_22_23$Team)
team_rating_22_23 <- team_rating_22_23 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_22_23,"../data/team_rating_22_23.csv")
### this table is according to descending order of Adjusted net rating
team_rating_22_23

#team rating for 2021-22
html = read_html("https://www.basketball-reference.com/leagues/NBA_2022_ratings.html")
table = html %>% html_table()
team_rating_21_22=table[[1]]
team_rating_21_22 = team_rating_21_22[-1, ]
colnames(team_rating_21_22) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_21_22$Team = as.character(team_rating_21_22$Team)
team_rating_21_22 <- team_rating_21_22 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_21_22,"../data/team_rating_21_22.csv")
### this table is according to descending order of Adjusted net rating
team_rating_21_22

#team rating for 2020-21
html = read_html("https://www.basketball-reference.com/leagues/NBA_2021_ratings.html")
table = html %>% html_table()
team_rating_20_21=table[[1]]
team_rating_20_21 = team_rating_20_21[-1, ]
colnames(team_rating_20_21) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_20_21$Team = as.character(team_rating_20_21$Team)
team_rating_20_21 <- team_rating_20_21 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_20_21,"../data/team_rating_20_21.csv")
### this table is according to descending order of Adjusted net rating
team_rating_20_21

#team rating for 2019-20
html = read_html("https://www.basketball-reference.com/leagues/NBA_2020_ratings.html")
table = html %>% html_table()
team_rating_19_20=table[[1]]
team_rating_19_20 = team_rating_19_20[-1, ]
colnames(team_rating_19_20) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_19_20$Team = as.character(team_rating_19_20$Team)
team_rating_19_20 <- team_rating_19_20 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_19_20,"../data/team_rating_19_20.csv")
### this table is according to descending order of Adjusted net rating
team_rating_19_20

#team rating for 2018-19
html = read_html("https://www.basketball-reference.com/leagues/NBA_2019_ratings.html")
table = html %>% html_table()
team_rating_18_19=table[[1]]
team_rating_18_19 = team_rating_18_19[-1, ]
colnames(team_rating_18_19) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_18_19$Team = as.character(team_rating_18_19$Team)
team_rating_18_19 <- team_rating_18_19 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_18_19,"../data/team_rating_18_19.csv")
### this tabl#e is according to descending order of Adjusted net rating
team_rating_18_19

#team rating for 2017-18
html = read_html("https://www.basketball-reference.com/leagues/NBA_2018_ratings.html")
table = html %>% html_table()
team_rating_17_18=table[[1]]
team_rating_17_18 = team_rating_17_18[-1, ]
colnames(team_rating_17_18) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_17_18$Team = as.character(team_rating_17_18$Team)
team_rating_17_18 <- team_rating_17_18 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_17_18,"../data/team_rating_17_18.csv")
### this table is according to descending order of Adjusted net rating
team_rating_17_18

#team rating for 2016-17
html = read_html("https://www.basketball-reference.com/leagues/NBA_2017_ratings.html")
table = html %>% html_table()
team_rating_16_17=table[[1]]
team_rating_16_17 = team_rating_16_17[-1, ]
colnames(team_rating_16_17) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_16_17$Team = as.character(team_rating_16_17$Team)
team_rating_16_17 <- team_rating_16_17 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv
write.csv(team_rating_16_17,"../data/team_rating_16_17.csv")
### this table is according to descending order of Adjusted net rating
team_rating_16_17

#team rating for 2015-16
html = read_html("https://www.basketball-reference.com/leagues/NBA_2016_ratings.html")
table = html %>% html_table()
team_rating_15_16=table[[1]]
team_rating_15_16 = team_rating_15_16[-1, ]
colnames(team_rating_15_16) = c("Rk", "Team", "Conf", "Div", "W", "L", "W/L%",
                                "MOV", "ORtg", "DRtg", "NRtg",
                                "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg")
team_rating_15_16$Team = as.character(team_rating_15_16$Team)
team_rating_15_16 <- team_rating_15_16 %>%
  mutate(across(c(Rk,W, L, `W/L%`, MOV, ORtg, DRtg, NRtg, Adj_MOV, Adj_ORtg, Adj_DRtg, Adj_NRtg),
                as.numeric))
###write your location to save csv

write.csv(team_rating_15_16, "../data/team_rating_15_16.csv")

### this table is according to descending order of Adjusted net rating
team_rating_15_16


