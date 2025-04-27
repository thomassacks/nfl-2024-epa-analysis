# NFL 2024 EPA Modeling: Team Offensive and Defensive Efficiency Analysis

## 📈 Project Overview
This project uses 2024 NFL play-by-play data to model **Expected Points Added (EPA)** across both offensive and defensive units. The purpose is to identify team strengths, weaknesses, and overall balance using EPA-based analytics that mirror what real NFL front offices study.

EPA measures how each play changes a team's expected points. Higher EPA on offense is good (more scoring), while **lower (more negative)** EPA allowed on defense is good (stronger defense).

---

## 🔍 Methodology
- **Data Source:** 2024 play-by-play data from `nflfastR`
- **Modeling:**
  - Linear regression to predict EPA based on down, yards to go, yardline, time, score differential, and play type
- **Team Summaries:**
  - **Offensive EPA per play**: Higher is better
  - **Defensive EPA allowed per play**: Lower (more negative) is better
- **Visualizations:**
  - Team rankings
  - Quadrant matrix for offensive vs defensive efficiency
  - Situation-based EPA breakdowns (short/medium/long distance)
- **Team Colors:** Official team colors were used for all visual clarity

---

## 📊 Key Visualizations

### 🏈 Offensive EPA Rankings
Offensive EPA Rankings
- Teams at the top (e.g., **BAL, BUF, DET**) have the most efficient offenses.
- Negative EPA offenses (bottom teams) are struggling to generate positive plays.

### 💪 Defensive EPA Allowed Rankings
Defensive EPA Rankings
- Negative numbers are better here. Teams like **PHI, DEN, HOU** suppress opponent scoring effectively.
- Teams with positive EPA allowed struggle defensively.

### 🔹 Offensive vs Defensive Matrix
Offense vs Defense Quadrant
- **Elite Teams**: High offense, low defense EPA (top right quadrant)
- **Pretenders**: Good offense, weak defense
- **Gritty Teams**: Bad offense, good defense
- **Rebuilders**: Struggles on both sides

---

## 📈 Model Summary
The regression model showed:
- **Positive yardage needed** and **longer distances** hurt EPA.
- **Score differential** impacts team playcalling.
- **Run plays** have significantly different EPA profiles than pass plays.

*(R-squared was low, as expected for noisy play-by-play football data, but significant variables still surfaced.)*

---

## 🔢 Deeper Analysis

### 🔹 Offense by Play Type
We calculated EPA separately for run vs pass plays.
- Some teams excel far more in one dimension (e.g., **BUF passing** vs **CAR running**).

### 🔹 EPA by Situation
EPA was broken down into short (1-3 yards), medium (4-7), and long (8+) distance scenarios for deeper situational football understanding.
- **Short yardage** produced much higher EPA swings.
- **Long downs** correlated with highly negative EPA, as expected.

### 🔹 EPA Variability
We measured the **standard deviation of EPA** by team.
- **TEN, IND, CLE** were the most volatile teams — huge swings from play to play.
- **BAL, BUF** showed much steadier EPA profiles.

---

## 🔖 Files Included
| File | Description |
|:---|:---|
| `epa_model_script.R` | Full R script for data cleaning, modeling, and visualizations |
| `offensive_epa_per_play_colored.png` | Bar chart of team offensive EPA |
| `defensive_epa_allowed_per_play_colored.png` | Bar chart of team defensive EPA allowed |
| `epa_offense_vs_defense_quadrant.png` | Quadrant matrix (offense vs defense) |
| `README.md` | Full project documentation (this file!) |

---

## 💬 Key Takeaways
- **Elite teams** (— e.g., **BAL, DET**) balance strong offense with good defense.
- **Pretenders** (— strong offense but weak defense) include teams like **BUF**.
- **Defensive stalwarts** with shaky offenses include teams like **PHI**.
- **Rebuilding teams** (struggles on both sides) include **CLE, LV**.

EPA is an incredibly powerful tool when used situationally and contextually — NFL analytics staffs rely on it heavily for decision-making and roster construction.

---

# 🏋️ Contact
Feel free to reach out for collaboration, deeper custom analyses, or general NFL analytics discussion!
Email | thomasbsacks@gmail.com
LinkedIn | https://www.linkedin.com/in/thomas-sacks/

