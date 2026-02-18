## ncca basketball web scraping -- 
library(rvest)
library(tidyverse)
url <- "https://big12sports.com/stats.aspx?path=mbball&year=2025"
page <- read_html(url)

tables <- page %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# tables is a list of 4 data frames
length(tables)  # should print 4

# accessing each table individually
offense <- tables[[1]]
defense <- tables[[2]]

# inspecting tables
head(offense)
head(defense)

# example matchup
team_A <- "Iowa State"
team_B <- "Arizona"

# offense points per game
A_off <- offense %>% filter(Team == team_A) %>% pull(`AVG/G`)
B_off <- offense %>% filter(Team == team_B) %>% pull(`AVG/G`)

# defense points allowed per game
A_def <- defense %>% filter(Team == team_A) %>% pull(Defense)
B_def <- defense %>% filter(Team == team_B) %>% pull(Defense)

A_expected <- (A_off + B_def) / 2
B_expected <- (B_off + A_def) / 2

# predicted winner 
predicted_winner <- ifelse(A_expected > B_expected, team_A, team_B)
predicted_winner


# now have a function for any matchup in the big 12
predict_game <- function(team_A, team_B, offense, defense) {
  A_off <- offense %>% filter(Team == team_A) %>% pull(`AVG/G`)
  B_off <- offense %>% filter(Team == team_B) %>% pull(`AVG/G`)
  A_def <- defense %>% filter(Team == team_A) %>% pull(Defense)
  B_def <- defense %>% filter(Team == team_B) %>% pull(Defense)
  
  A_expected <- (A_off + B_def) / 2
  B_expected <- (B_off + A_def) / 2
  
  winner <- ifelse(A_expected > B_expected, team_A, team_B)
  
  result <- paste0(
    team_A, " is expected to score ", round(A_expected, 2), " points, ",
    team_B, " is expected to score ", round(B_expected, 2), " points, ",
    "so the predicted winner is ", winner, "."
  )
  
  return(result)
}

# example for upcoming game (FEB 18th)
predict_game("Arizona", "BYU", offense, defense)

###############
##### glm #####
###############

# match-up data-set
set.seed(123)

teams <- offense$Team

matchups <- data.frame(
  TeamA = character(),
  TeamB = character(),
  PPG_diff = numeric(),
  DEF_diff = numeric(),
  Winner = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:(length(teams)-1)){
  for(j in (i+1):length(teams)){
    
    team_A <- teams[i]
    team_B <- teams[j]
    
    A_off <- offense %>% filter(Team == team_A) %>% pull(`AVG/G`)
    B_off <- offense %>% filter(Team == team_B) %>% pull(`AVG/G`)
    A_def <- defense %>% filter(Team == team_A) %>% pull(Defense)
    B_def <- defense %>% filter(Team == team_B) %>% pull(Defense)
    
    PPG_diff <- A_off - B_off
    DEF_diff <- B_def - A_def
    
    # Convert linear strength into probability using logistic formula
    logit_prob <- 0.15 * PPG_diff + 0.10 * DEF_diff
    prob <- 1 / (1 + exp(-logit_prob))
    
    # Simulated game result
    Winner <- rbinom(1, 1, prob)
    
    matchups <- rbind(matchups, data.frame(
      TeamA = team_A,
      TeamB = team_B,
      PPG_diff = PPG_diff,
      DEF_diff = DEF_diff,
      Winner = Winner
    ))
  }
}

# fit model (only using PPG and DEF)
glm_model <- glm(Winner ~ PPG_diff + DEF_diff,
                 data = matchups,
                 family = binomial)

summary(glm_model)

# predict
predict_game_glm <- function(team_A, team_B, offense, defense, model) {
  
  A_off <- offense %>% filter(Team == team_A) %>% pull(`AVG/G`)
  B_off <- offense %>% filter(Team == team_B) %>% pull(`AVG/G`)
  A_def <- defense %>% filter(Team == team_A) %>% pull(Defense)
  B_def <- defense %>% filter(Team == team_B) %>% pull(Defense)
  
  new_game <- data.frame(
    PPG_diff = A_off - B_off,
    DEF_diff = B_def - A_def
  )
  
  prob <- predict(model, newdata = new_game, type = "response")
  
  winner <- ifelse(prob > 0.5, team_A, team_B)
  
  paste0(
    team_A, " has a ", round(prob*100, 1),
    "% chance of beating ", team_B,
    ". Predicted winner: ", winner, "."
  )
}

# example for matchup on FEB 18th
predict_game_glm("Arizona", "BYU", offense, defense, glm_model)

# actual 

# new model using other predictors 
# FG percentage 
team_FG_per <- tables[[4]]
opponent_FG_per <- tables[[5]]

# 3 FG percentage 
team_3FG_per <- tables[[6]]
opponent_3FG_per <- tables[[8]]

# free throw percentage 
team_FTP_per <- tables[[9]]
opponent_FTP_per <- tables[[10]]

# cleaning rows 
team_FG_per$PCT  <- as.numeric(team_FG_per$PCT)
team_3FG_per$PCT <- as.numeric(team_3FG_per$PCT)
team_FTP_per$PCT <- as.numeric(team_FTP_per$PCT)

opponent_FG_per$PCT  <- as.numeric(opponent_FG_per$PCT)
opponent_3FG_per$PCT <- as.numeric(opponent_3FG_per$PCT)
opponent_FTP_per$PCT <- as.numeric(opponent_FTP_per$PCT)

# adding predictors 
set.seed(123)

teams <- offense$Team

matchups <- data.frame()

for(i in 1:(length(teams)-1)){
  for(j in (i+1):length(teams)){
    
    team_A <- teams[i]
    team_B <- teams[j]
    
    # offensive scoring
    A_off <- offense %>% filter(Team == team_A) %>% pull(`AVG/G`)
    B_off <- offense %>% filter(Team == team_B) %>% pull(`AVG/G`)
    
    # defense 
    A_def <- defense %>% filter(Team == team_A) %>% pull(Defense)
    B_def <- defense %>% filter(Team == team_B) %>% pull(Defense)
    
    # shooting percentages
    A_FG  <- team_FG_per %>% filter(Team == team_A) %>% pull(PCT)
    B_FG  <- team_FG_per %>% filter(Team == team_B) %>% pull(PCT)
    
    A_3FG <- team_3FG_per %>% filter(Team == team_A) %>% pull(PCT)
    B_3FG <- team_3FG_per %>% filter(Team == team_B) %>% pull(PCT)
    
    A_FT  <- team_FTP_per %>% filter(Team == team_A) %>% pull(PCT)
    B_FT  <- team_FTP_per %>% filter(Team == team_B) %>% pull(PCT)
    
    # differences
    PPG_diff  <- A_off - B_off
    DEF_diff  <- B_def - A_def
    FG_diff   <- A_FG - B_FG
    FG3_diff  <- A_3FG - B_3FG
    FT_diff   <- A_FT - B_FT
    
    # logistic strength formula
    logit_prob <- 0.12*PPG_diff +
      0.08*DEF_diff +
      4*FG_diff +
      4*FG3_diff +
      3*FT_diff
    
    prob <- 1/(1 + exp(-logit_prob))
    
    Winner <- rbinom(1, 1, prob)
    
    matchups <- rbind(matchups, data.frame(
      TeamA = team_A,
      TeamB = team_B,
      PPG_diff,
      DEF_diff,
      FG_diff,
      FG3_diff,
      FT_diff,
      Winner
    ))
  }
}

# glm with new predictors 
glm_model <- glm(Winner ~ PPG_diff + DEF_diff +
                   FG_diff + FG3_diff + FT_diff,
                 data = matchups,
                 family = binomial)

summary(glm_model)

# updated prediction function
predict_game_glm <- function(team_A, team_B) {
  
  A_off <- offense %>% filter(Team == team_A) %>% pull(`AVG/G`)
  B_off <- offense %>% filter(Team == team_B) %>% pull(`AVG/G`)
  
  A_def <- defense %>% filter(Team == team_A) %>% pull(Defense)
  B_def <- defense %>% filter(Team == team_B) %>% pull(Defense)
  
  A_FG  <- team_FG_per %>% filter(Team == team_A) %>% pull(PCT)
  B_FG  <- team_FG_per %>% filter(Team == team_B) %>% pull(PCT)
  
  A_3FG <- team_3FG_per %>% filter(Team == team_A) %>% pull(PCT)
  B_3FG <- team_3FG_per %>% filter(Team == team_B) %>% pull(PCT)
  
  A_FT  <- team_FTP_per %>% filter(Team == team_A) %>% pull(PCT)
  B_FT  <- team_FTP_per %>% filter(Team == team_B) %>% pull(PCT)
  
  new_game <- data.frame(
    PPG_diff = A_off - B_off,
    DEF_diff = B_def - A_def,
    FG_diff  = A_FG - B_FG,
    FG3_diff = A_3FG - B_3FG,
    FT_diff  = A_FT - B_FT
  )
  
  prob <- predict(glm_model, newdata = new_game, type = "response")
  
  winner <- ifelse(prob > 0.5, team_A, team_B)
  
  paste0(
    team_A, " has a ", round(prob*100,1),
    "% chance of beating ", team_B,
    ". Predicted winner: ", winner, "."
  )
}

# predicting for game on Feb 18th
predict_game_glm("Arizona", "BYU")

# actual 




