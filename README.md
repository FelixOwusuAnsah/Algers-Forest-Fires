# Algers-Forest-Fires
# PROJECT SUMMARY
Forest Fire Prediction Using Machine Learning (Unsupervised and Supervised)

**OBJECTIVE**
To develop predictive models for forest fire occurrence using weather and fuel moisture data.

**DATA**
Total observations: 1,467

Features: month, FFMC, DMC, DC, ISI, Temperature, RH, wind, rain

Target variable: outcome (Fire = 1, No Fire = 0)

**MODELS DEVELOPED**
**Model	Accuracy	Recall	Specificity	AUC**
Logistic Regression (Firth)	65.8%	82.6%	44.1%	0.696
Decision Tree	63.9%	86.5%	34.8%	0.701
Random Forest	62.8%	69.6%	54.0%	0.670

**KEY FINDINGS**
Decision Tree catches the most fires (86.5% recall)

Logistic Regression has highest overall accuracy

Random Forest produces fewest false alarms

Wind speed is the strongest predictor of fire

**RECOMMENDATION**
Deploy the Decision Tree model for early warning systems as it detects the highest number of actual fires.

**FILES IN THIS PROJECT**
Forest_Fire_Data.csv - Main dataset

Algers_Forest.csv - Secondary dataset

R scripts for EDA, unsupervised learning, and supervised models

**REQUIREMENTS**
R version 4.5+

**Packages:** tidyverse, rpart, randomForest, logistf, pROC, caret
