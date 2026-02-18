# NCAA Big 12 Basketball Prediction Analysis

## Overview

This project uses **web scraping, data cleaning, and logistic regression modeling** to predict outcomes of NCAA Big 12 men’s basketball games. Using official team statistics from the 2025 season, the project simulates matchups and estimates each team’s probability of winning.

---

## Data Collection

- Source: [Big 12 official stats](https://big12sports.com/stats.aspx?path=mbball&year=2025)  
- Data collected via **R `rvest` package** and stored in R data frames.  
- Tables scraped include:
  - Team scoring offense (`AVG/G`)  
  - Team defense (`Defense`)  
  - Shooting percentages: FG%, 3PT%, and FT%  

---

## Data Cleaning

- Converted percentage columns to numeric (`as.numeric`)  
- Standardized team names across tables  
- Calculated **game-level differences** between two teams for each predictor:
  - Points per game difference (`PPG_diff`)  
  - Defensive points allowed difference (`DEF_diff`)  
  - Field goal percentage difference (`FG_diff`)  
  - 3-point percentage difference (`FG3_diff`)  
  - Free throw percentage difference (`FT_diff`)  

---

## Exploratory Simulation

- Simulated every possible matchup between teams in the dataset  
- Calculated a **logistic strength score** based on team differences  
- Converted scores to probabilities using the **sigmoid function**  

Example simulation formula:

logit = 0.12 * PPG_diff + 0.08 * DEF_diff + 4 * FG_diff + 4 * FG3_diff + 3 * FT_diff

probability of Team A winning = 1 / (1 + exp(-logit))

---

## Predictive Model

- Logistic regression (`glm` with `family = binomial`) trained on simulated matchups  
- Predictors used:
  - `PPG_diff`  
  - `DEF_diff`  
  - `FG_diff`  
  - `FG3_diff`  
  - `FT_diff`  
- Model outputs **probability that Team A beats Team B**

---

## Prediction Function

- Function: `predict_game_glm(team_A, team_B)`  
- Inputs: Team names  
- Outputs:
  - Probability Team A wins  
  - Predicted winner  

**Example:**

```r
predict_game_glm("Arizona", "BYU")
# Output: "Arizona has a 81.2% chance of beating BYU. Predicted winner: Arizona."
```
