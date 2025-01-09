# WNBA Analysis Project

This repository contains an R script designed for analyzing WNBA statistics, generating insightful visualizations, and building predictive models based on the 2024 season data. The project utilizes data provided by the `wehoop` package, enabling advanced exploration of play-by-play data, team performance, and player contributions.

## Features

- **Data Loading**:
  - Play-by-play data
  - Team statistics
  - Player statistics
- **Data Wrangling**:
  - Creation of summarized datasets for teams and players
  - Filtering and transformation of raw data for analysis
- **Visualization**:
  - Team performance trends (e.g., scoring margins, shooting percentages)
  - Player contributions (e.g., top scorers, rebounders, and assist leaders)
  - Shot charts for individual players and teams
- **Modeling**:
  - Linear regression analysis to predict scoring margins
  - Diagnostic plots to evaluate model performance

## Visual Highlights

- **Team Averages**:
  - Scatterplots for 3-point attempts vs. makes
  - Shooting efficiency comparisons between 2-point and 3-point field goals
- **Player Metrics**:
  - Bar charts showcasing top scorers, rebounders, and assist leaders
- **Shot Charts**:
  - Detailed shot locations for teams and individual players during home and away games

## Requirements

The project relies on the following R packages:

- `wehoop`
- `dplyr`
- `tidyverse`
- `ggplot2`
- `ggimage`
- `sportyR`
- `grid`
- `gridExtra`

To install the required packages, use the following command:

```r
install.packages(c("wehoop", "dplyr", "tidyverse", "ggplot2", "ggimage", "sportyR", "gridExtra"))
```

## Insights and Applications

This analysis is intended to provide:

- Insights into team and player performance during the 2024 WNBA season.
- Visual tools for evaluating trends, such as shooting efficiency and team contributions.
- A foundation for predictive modeling in sports analytics.

## Future Work

- Expand analysis to include historical WNBA data.
- Incorporate machine learning models for advanced prediction.
- Add interactive visualizations using `shiny` or other R frameworks.



