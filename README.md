# Student-Performance-Analysis
The Student Performance Analysis is R-based application that uses machine learning to predict academic performance and provide data-driven insights. It features both classification (pass/fail prediction) and regression (final grade prediction) models, integrated into an interactive Shiny dashboard for educators and researchers.

## Features
- **Interactive Shiny App** – User-friendly interface for predictions and visualizations.
- **Dual ML Models** – Random Forest for both classification & regression tasks.
- **Data-Driven Insights** – Identify key factors affecting student performance.
- **Comprehensive EDA** – Explore trends and correlations in the dataset.
- **Model Metrics** – View accuracy, precision, recall, F1-score, MSE, RMSE, and R².

## Prerequisites
**Hardware:**
- Processor: Intel i5 or higher (or AMD equivalent)
- RAM: 8GB minimum (16GB recommended)
- Storage: 50GB free space
- Internet connection for data access

**Software:**
- R (latest stable version)
- RStudio or VS Code with R extensions
- Required R packages:
- shiny, randomForest, ggplot2, dplyr, caret, e1071

## Steps to Run
1. Clone the repository or download the Final.R file.
2. Place the dataset student-mat-formatted.csv in the same directory as the script.
3. Open Final.R in RStudio or your preferred R environment.
4. Install the required packages:
   install.packages(c("shiny", "randomForest", "ggplot2", "dplyr", "caret", "e1071"))
5. Launch the Shiny app:
   shiny::runApp()

## Usage
1. Input Student Details – Enter G1, G2, absences, study time, and family support in the sidebar.
2. Predict Performance – Click "Predict Performance" to see pass/fail prediction and estimated final grade (G3).
3. Explore Metrics – Navigate through tabs for model metrics, feature importance, confusion matrix, and EDA plots.
4. Dataset Summary – View dataset structure and summary statistics.

## Results
1. Classification Model: Predicts pass/fail status with high accuracy.
2. Regression Model: Estimates final grade (G3).
3. Top Influential Factors: G1, G2, absences, study time, and family support.

## Contributors
Diksha Gupta
Mayank Anand
